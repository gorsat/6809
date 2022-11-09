use std::time::Duration;

use super::{test::TestCriterion, *};
use crate::hex::{HexRecordCollection, HexRecordType};
/// The Core struct implements the 6809 simulator and debugger.
/// Its implementation spans multiple files: runtime.rs, debug.rs, memory.rs, registers.rs
pub struct Core {
    pub reg: registers::Set,      // the full set of 6809 registers
    pub mem: Box<[u8]>,           // memory is both RAM and ROM -- the whole address space
    pub ram_top: u16,             // keep track of where the caller wants ram to end
    pub acia: Option<acia::Acia>, // ACIA simulator
    /* perf measurement */
    pub start_time: Instant,    // the most recent time at which self.exec() started a program
    pub instruction_count: u64, // the number of instructions executed since the most recent program started
    pub clock_cycles: u64,      // the number of clock cycles consumed since the most recent program started
    pub eval_time: Duration,    // the total time spent in the eval method of instructions
    pub prep_time: Duration,    // the total time spent preparing to call eval methods for all instructions
    pub commit_time: Duration,  // the total time spent committing the Outcome of all instructions
    /* fields for debugging */
    pub breakpoints: Vec<debug::Breakpoint>,    // all current breakpoints
    pub watch_hits: Vec<u16>,                   // tracks writes to addresses for which watch breakpoints have been set
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
    pub fn new(ram_top: u16, acia_addr: Option<u16>) -> Core {
        instructions::init();

        Core {
            reg: { Default::default() },
            // allocate the entire 16-bit address space (we need ROM as well as RAM)
            mem: vec![0u8; 0x10000].into_boxed_slice(),
            acia: acia_addr.map(|a| acia::Acia::new(a).expect("failed to start ACIA")),
            ram_top,
            start_time: Instant::now(),
            instruction_count: 0,
            clock_cycles: 0,
            eval_time: Duration::ZERO,
            prep_time: Duration::ZERO,
            commit_time: Duration::ZERO,
            breakpoints: Vec::new(),
            watch_hits: Vec::new(),
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
    /// load_hex copies the contents of a HexRecordCollection into simulator memory
    pub fn load_hex(&mut self, hex: &HexRecordCollection, hex_path: Option<&str>) -> Result<u16, Error> {
        let mut extent = 0u16;
        let mut eof = false;
        let mut rom_write = false;
        // clean out the reset vector in case it was set by a previous program
        self.force_reset_vector(0)?;
        for r in hex.iter() {
            match r.record_type {
                HexRecordType::Data => {
                    if let Some(data) = r.data.as_ref() {
                        if r.address as usize + r.data_size as usize > self.mem.len() {
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
                            self.mem[addr] = b;
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

    /// load_program copies the binary representation of the given Program into simulator memory
    pub fn load_program(&mut self, program: &Program, program_path: Option<&str>) -> Result<u16, Error> {
        let mut extent = 0u16;
        let mut rom_write = false;
        // clean out the reset vector in case it was set by a previous program
        self.force_reset_vector(0)?;
        for line in &program.lines {
            if let Some(bob) = line.obj.as_ref().and_then(|o| o.bob_ref()) {
                if bob.size as usize + bob.addr as usize > self.mem.len() {
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
                extent += bob.to_bytes(&mut self.mem[bob.addr as usize..]);
                if bob.addr as usize + bob.size as usize >= self.ram_top as usize {
                    rom_write = true;
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
    pub fn check_criteria(&self, criteria: &Vec<TestCriterion>) -> Result<(), Error> {
        if criteria.len() == 0 {
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
