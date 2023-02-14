use super::{test::TestCriterion, *};
use crate::hex::{HexRecordCollection, HexRecordType};
use std::{
    cell::RefCell,
    fs::File,
    io::Read,
    sync::{Arc, RwLock},
    time::Duration,
};
#[allow(unused)]
#[derive(Debug, PartialEq, Eq)]
pub enum InterruptType {
    Reset,
    Nmi,
    Firq,
    Irq,
    Swi,
    Swi2,
    Swi3,
}
#[allow(unused)]
impl InterruptType {
    pub fn vector(&self) -> u16 {
        use InterruptType::*;
        match self {
            Reset => 0xfffe,
            Nmi => 0xfffc,
            Swi => 0xfffa,
            Irq => 0xfff8,
            Firq => 0xfff6,
            Swi2 => 0xfff4,
            Swi3 => 0xfff2,
        }
    }
}
/// The Core struct implements the 6809 simulator and debugger.
/// Its implementation spans multiple files: runtime.rs, debug.rs, memory.rs, registers.rs
pub struct Core {
    pub reg: registers::Set, // the full set of 6809 registers
    pub ram: Arc<RwLock<Vec<u8>>>,
    pub ram_top: u16,              // keep track of where the caller wants ram to end
    pub acia: Option<acia::Acia>,  // ACIA simulator
    pub reset_vector: Option<u16>, // overrides the reset vector if set
    /* interrupt processing */
    pub cart_pending: bool,  // true if cart is loaded but hasn't been run yet
    pub in_cwai: bool,       // if true, the processor is within a CWAI instruction
    pub in_sync: bool,       // if true, the processor is within a SYNC instruction
    pub hsync_prev: Instant, // the last time hsync occurred
    pub vsync_prev: Instant, // the last time vsync occurred
    /* perf measurement */
    pub start_time: Instant,    // the most recent time at which self.exec() started a program
    pub instruction_count: u64, // the number of instructions executed since the most recent program started
    pub clock_cycles: u64,      // the number of clock cycles consumed since the most recent program started
    pub eval_time: Duration,    // the total time spent in the eval method of instructions
    pub prep_time: Duration,    // the total time spent preparing to call eval methods for all instructions
    pub commit_time: Duration,  // the total time spent committing the Outcome of all instructions
    /* fields for debugging */
    pub in_debugger: bool,
    pub breakpoints: Vec<debug::Breakpoint>,    // all current breakpoints
    pub watch_hits: RefCell<Vec<u16>>,          // tracks writes to addresses for which watch breakpoints have been set
    pub addr_to_sym: HashMap<u16, Vec<String>>, // map from address to symbol
    pub sym_to_addr: HashMap<String, u16>,      // map from symbol to address
    pub list_mode: Option<debug::ListMode>,     // equals Some(ListMode) if currently in list (disassemble) mode
    pub program_start: u16,                     // the starting address of the program; should be equal to reset vector
    pub faulted: bool,                          // true if the CPU has faulted (e.g., stack oveflow)
    pub history: Option<VecDeque<String>>,      // list of instructions that have been recently executed
    pub step_mode: debug::StepMode,             // determines current step mode (see debug.rs)
    pub next_linear_step: u16, // tracks the address of the next contiguous instruction (differs from PC when there is a branch or jump)
    pub trace: bool,           // if true then display each instruction as it's executed
}
impl Core {
    pub fn new( ram_top: u16, acia_addr: Option<u16>) -> Core {
        instructions::init();

        Core {
            reg: { Default::default() },
            // allocate the entire 16-bit address space (we need ROM as well as RAM)
            // mem: vec![0u8; 0x10000].into_boxed_slice(),
            ram: Arc::new(RwLock::new(vec![0;0x10000])),
            acia: acia_addr.map(|a| acia::Acia::new(a).expect("failed to start ACIA")),
            reset_vector: None,
            cart_pending: false,
            in_cwai: false,
            in_sync: false,
            hsync_prev: Instant::now(),
            vsync_prev: Instant::now(),
            ram_top,
            start_time: Instant::now(),
            instruction_count: 0,
            clock_cycles: 0,
            eval_time: Duration::ZERO,
            prep_time: Duration::ZERO,
            commit_time: Duration::ZERO,
            in_debugger: false,
            breakpoints: Vec::new(),
            watch_hits: RefCell::new(Vec::new()),
            addr_to_sym: HashMap::new(),
            sym_to_addr: HashMap::new(),
            list_mode: None,
            program_start: 0,
            faulted: false,
            history: None,
            step_mode: debug::StepMode::Off,
            next_linear_step: 0,
            trace: config::ARGS.trace,
        }
    }

    /// load_bytes copies bytes from a slice of u8 into simulator memory at addr
    /// This is only used in tests atm.
    #[cfg(test)]
    pub fn load_bytes(&mut self, bytes: &[u8], mut addr: u16) -> Result<(), Error> {
        for byte in bytes {
            self._write_u8(memory::AccessType::Generic, addr, *byte)?;
            addr += 1;
        }

        Ok(())
    }

    #[allow(unused)]
    pub fn load_program_from_file(&mut self, filename: &str) -> Result<(), Error> {
        let path = Path::new(filename);
        let ext = path.extension().and_then(OsStr::to_str).unwrap_or("");
        match ext.to_ascii_lowercase().as_str() {
            "asm" | "s" => {
                // the file looks like assembly source code, so try to assemble it
                let asm = Assembler::new();
                info!("Assembling {}", filename);
                let program = asm.assemble_from_file(filename)?;
                self.load_program(&program, Some(filename))?;
            }
            "hex" => {
                // the file looks like machine code in hex format; read it
                let hex = HexRecordCollection::read_from_file(filename)?;
                info!("Successfully loaded hex file {}", filename);
                self.load_hex(&hex, Some(filename))?;
            }
            _ => return Err(general_err!("invalid file extension")),
        }
        Ok(())
    }
    /// load_hex copies the contents of a HexRecordCollection into simulator memory
    pub fn load_hex(&mut self, hex: &HexRecordCollection, hex_path: Option<&str>) -> Result<u16, Error> {
        let mut extent = 0u16;
        let mut eof = false;
        let mut rom_write = false;
        // clean out the reset vector in case it was set by a previous program
        self.force_reset_vector(0)?;
        {
            // scope for ram RwLock
            let mut ram = self.ram.write().unwrap();
            for r in hex.iter() {
                match r.record_type {
                    HexRecordType::Data => {
                        if let Some(data) = r.data.as_ref() {
                            if r.address as usize + r.data_size as usize > ram.len() {
                                return Err(Error::new(
                                    ErrorKind::Memory,
                                    None,
                                    format!(
                                        "program overflowed system RAM ({} byte object at {:04X})",
                                        r.data_size, r.address
                                    )
                                    .as_str(),
                                ));
                            }
                            let mut addr = r.address as usize;
                            for &b in data {
                                ram[addr] = b;
                                addr += 1;
                                extent += 1;
                                if addr >= self.ram_top as usize {
                                    rom_write = true;
                                }
                            }
                        }
                    }
                    HexRecordType::EndOfFile => {
                        eof = true;
                        break;
                    }
                    _ => warn!("ignoring unsupported record type ({}) in hex file.", r.record_type),
                }
            }
        }
        if !eof {
            return Err(general_err!("failed to find EOF record in hex file"));
        }
        if rom_write {
            info!("Portions of this program reside in ROM")
        }
        verbose_println!("loaded {} bytes from hex file", extent);
        if config::auto_load_syms() {
            if let Some(path) = hex_path {
                match self.try_auto_load_symbols(path) {
                    Ok(n) => info!("Auto-loaded {} symbols.", n),
                    Err(e) => warn!("Failed to auto-load symbols: {}", e),
                }
            }
        }
        Ok(extent)
    }

    /// load_bin loads binary data from a file into memory at the given address
    #[allow(unused)]
    pub fn load_bin(&mut self, bin_path: &Path, addr: u16) -> Result<usize, Error> {
        let mut f = File::open(bin_path)?;
        let mut ram = self.ram.write().unwrap();
        let extent = f.read(&mut ram[addr as usize..])?;
        verbose_println!(
            "loaded {} bytes at 0x{:04x} from binary file \"{}\"",
            extent,
            addr,
            bin_path.display()
        );
        Ok(extent)
    }

    #[allow(unused)]
    pub fn load_cart(&mut self, cart_path: &Path) -> Result<usize, Error> {
        let size = self.load_bin(cart_path, 0xc000)?;
        self.cart_pending = true;
        Ok(size)
    }

    /// load_program copies the binary representation of the given Program into simulator memory
    pub fn load_program(&mut self, program: &Program, program_path: Option<&str>) -> Result<u16, Error> {
        let mut extent = 0u16;
        let mut rom_write = false;
        // clean out the reset vector in case it was set by a previous program
        self.force_reset_vector(0)?;
        {
            // scope for ram RwLock
            let mut ram = self.ram.write().unwrap();
            for line in &program.lines {
                if let Some(bob) = line.obj.as_ref().and_then(|o| o.bob_ref()) {
                    if bob.size as usize + bob.addr as usize > ram.len() {
                        return Err(Error::new(
                            ErrorKind::Memory,
                            None,
                            format!(
                                "program overflowed system RAM ({} byte object at {:04X})",
                                bob.size, bob.addr
                            )
                            .as_str(),
                        ));
                    }
                    extent += bob.to_bytes(&mut ram[bob.addr as usize..]);
                    if bob.addr as usize + bob.size as usize >= self.ram_top as usize {
                        rom_write = true;
                    }
                }
            }
        }
        if rom_write {
            info!("Portions of this program reside in ROM")
        }
        verbose_println!("loaded {} bytes", extent);
        if config::auto_load_syms() {
            if let Some(path) = program_path {
                match self.try_auto_load_symbols(path) {
                    Ok(n) => info!("Auto-loaded {} symbols.", n),
                    Err(e) => warn!("Failed to auto-load symbols: {}", e),
                }
            }
        }
        Ok(extent)
    }
    /// check_criteria evaluates each TestCriterion provided and returns Err(Error) if any fail
    #[allow(unused)]
    pub fn check_criteria(&self, criteria: &Vec<TestCriterion>) -> Result<(), Error> {
        if criteria.is_empty() {
            return Ok(());
        }
        info!(
            "Validating {} test criteri{}",
            criteria.len(),
            if criteria.len() == 1 { "on" } else { "a" }
        );
        let mut error_count = 0;
        for tc in criteria {
            print!("\t{} --> ", tc);
            match tc.eval(self) {
                Ok(_) => println!(green!("PASS")),
                Err(e) => {
                    error_count += 1;
                    println!(red!("FAIL {}"), e.msg)
                }
            }
        }
        if error_count == 0 {
            Ok(())
        } else {
            Err(Error {
                kind: ErrorKind::Test,
                ctx: None,
                msg: format!("Failed {error_count} test(s)"),
            })
        }
    }
}
