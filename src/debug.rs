use super::*;
use std::io::{stdin, stdout, BufRead, Write};

macro_rules! help {
    ($name:ident,$help:expr) => {
        #[allow(non_upper_case_globals)]
        static $name: &'static str = $help;
    };
}
macro_rules! show_help {
    ($name:ident) => {
        println!("{}", $name)
    };
}

help!(cmd_g, "g - Go; Resume execution at PC");
help!(cmd_his, "his - Show recent history of executed instructions");
help!(cmd_c, "c - Context; Display the state of all registers");
help!(cmd_ba, "ba <loc> [<notes>] - Breakpoint Add; add break at <loc>");
help!(cmd_bw, "bw <loc> [<notes>] - Add Watch Breakpoint on <loc>");
help!(cmd_bd, "bd <num> - Breakpoint Delete; delete breakpoint #<num>");
help!(cmd_bl, "bl - Breakpoint List; list all breakpoints");
help!(
    cmd_bn,
    "bn <num> <notes> - Breakpoint Notes; change notes for breakpoint <num>"
);
help!(
    cmd_bi,
    "bt - Breakpoint Toggle; active/inactive toggle for breakpoint <num>"
);
help!(cmd_dm, "dm [<loc>] [<num>] - Dump Memory; show <num> bytes at <loc>");
help!(cmd_ds, "ds [<num>] - Dump Stack; show <num> bytes of system stack");
help!(cmd_f, "f <value> <start_loc> [end_loc] - find next occurance of value");
help!(cmd_l, "l [<loc>] [<num>] - List <num> instructions at <loc>");
help!(cmd_wd, "wd - Working Directory; display the current working directory");
help!(cmd_q, "q - Quit; terminate this application");
help!(cmd_r, "r - Restart program at original Program Counter address");
help!(cmd_rs, "rs - Restart Step; restart in step mode");
help!(cmd_s, "s - Step; enter step mode (press esc to exit)");
help!(cmd_so, "so - Step Over current instruction, then enter step mode");
help!(cmd_t, "t - Trace; toggle tracing on/off");
help!(cmd_load, "load <file> - Load Symbols; load symbols from .sym file");
help!(cmd_sym, "sym [<loc>] - List all symbols or show symbols at <loc>");
help!(cmd_h, "h - Help; display this help text");

static COMMAND_HELP: &[&str] = &[
    cmd_g,
    cmd_his,
    cmd_c,
    cmd_ba,
    cmd_bw,
    cmd_bi,
    cmd_bd,
    cmd_bl,
    cmd_bn,
    cmd_dm,
    cmd_ds,
    cmd_l,
    cmd_q,
    cmd_r,
    cmd_rs,
    cmd_s,
    cmd_so,
    cmd_t,
    cmd_wd,
    cmd_load,
    cmd_h,
    cmd_sym,
    "<loc> syntax: Hex address (e.g. FF0A) or '?' followed by symbol (e.g. \"?START\")",
];

/// Tracks the state of the debugger's list mode.
pub struct ListMode {
    pub lines_remaining: u16,
    pub saved_ctx: registers::Set,
}

/// Contains all metadata and state for a single breakpoint.
pub struct Breakpoint {
    /// true if breakpoint is active
    active: bool,
    /// true if this is a watch breakpoint (rather than an instruction breakpoint)
    watch: bool,
    /// address associated with this breakpoint
    addr: u16,
    /// all symbols associated with this breakpoint's address
    syms: Option<Vec<String>>,
    /// optional notes added by the user
    notes: Option<String>,
}

impl PartialEq for Breakpoint {
    fn eq(&self, other: &Self) -> bool { self.addr == other.addr }
}

impl Breakpoint {
    pub fn new(addr: u16, watch: bool, syms: Option<&Vec<String>>, notes: Option<String>) -> Self {
        Breakpoint {
            active: true,
            watch,
            addr,
            syms: syms.map(|s| {
                let mut v = Vec::new();
                for y in s {
                    v.push(y.clone())
                }
                v
            }),
            notes,
        }
    }
}
impl std::fmt::Display for Breakpoint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s1;
        let s2;
        write!(
            f,
            "{:04X}{:1}{:1} {:10}{}",
            self.addr,
            if self.watch { "w" } else { "" },
            if !self.active { "*" } else { "" },
            if let Some(syms) = self.syms.as_ref() {
                s1 = syms.join(",");
                s1.as_str()
            } else {
                ""
            },
            if let Some(notes) = self.notes.as_ref() {
                s2 = format!("  \"{}\"", notes.as_str());
                s2.as_str()
            } else {
                ""
            }
        )
    }
}

/// Tracks the state of the debugger's step mode.
#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum StepMode {
    Off,
    Stepping,
    StepOverPending(u16),
    SteppingOverTo(u16),
}
impl Core {
    pub fn debug_cli(&mut self) -> Result<(), Error> {
        self.in_debugger = true;
        let save_pc = self.reg.pc;
        // clear step mode
        self.step_mode = StepMode::Off;
        // clear watch hits
        self.watch_hits.get_mut().clear();
        // clear list mode
        if let Some(lm) = &self.list_mode {
            self.reg = lm.saved_ctx;
            self.list_mode = None;
        }
        println!("Current context: [{} -> ({})]", self.reg, self.reg.cc);
        loop {
            let mut input = String::new();
            if self.faulted {
                print!(concat!(blue!("Debug ["), red!("FAULT"), blue!("]> ")));
            } else {
                print!(blue!("Debug> "))
            };
            _ = stdout().flush();
            _ = stdin().read_line(&mut input);
            let cmd: Vec<&str> = input.split_whitespace().collect();
            if cmd.is_empty() {
                continue;
            }
            match cmd[0].to_lowercase().as_str() {
                "g" => {
                    // resume running the program
                    break;
                }
                "c" => {
                    println!("Current context: [{} -> ({})]", self.reg, self.reg.cc);
                }
                "dm" => {
                    // dump memory
                    let mut addr = self.reg.pc;
                    if cmd.len() > 1 {
                        if let Some(a) = self.parse_address(cmd[1]) {
                            addr = a;
                        } else {
                            println!("Invalid address or symbol.");
                            show_help!(cmd_dm);
                            continue;
                        }
                    }
                    let mut count = 16;
                    if cmd.len() > 2 {
                        if let Some(more) = self.parse_number(cmd[2]) {
                            count = more.u16();
                        }
                        // just ignore bad count
                    }
                    // make sure we don't overflow u16
                    if addr as u32 + count as u32 > 0xffff {
                        count = (0x10000u32 - addr as u32) as u16;
                    }
                    println!("Dumping {} bytes at {:04X}", count, addr);
                    self.dump_mem(addr, count)
                }
                "ds" => {
                    // dump stack
                    let addr = self.reg.s;
                    let mut count = 16;
                    if cmd.len() > 1 {
                        if let Some(more) = self.parse_number(cmd[1]) {
                            count = more.u16();
                        }
                    }
                    println!("Dumping {} bytes from System stack ({:04X})", count, addr);
                    self.dump_mem(addr, count);
                }
                "f" => {
                    // find: f <value> <start_loc> [end_loc]
                    if cmd.len() < 3 {
                        println!("Bad syntax.");
                        show_help!(cmd_f);
                        continue;
                    }
                    if let Some(value) = self.parse_number(cmd[1]) {
                        if let Some(start) = self.parse_address(cmd[2]) {
                            let end = if cmd.len() > 3 {
                                if let Some(end) = self.parse_address(cmd[3]) {
                                    end
                                } else {
                                    println!("Invalid end address.");
                                    continue;
                                }
                            } else {
                                0xfeff
                            };
                            let mut found_at = None;
                            let mut pat = [0u8; 2];
                            'search: for addr in start..=(end - value.size() + 1) {
                                value.get_as_bytes(&mut pat);
                                for i in 0..value.size() {
                                    let b = self._read_u8(memory::AccessType::System, addr + i, None).unwrap();
                                    if b != pat[i as usize] {
                                        continue 'search;
                                    }
                                }
                                // found it!
                                found_at = Some(addr);
                                break;
                            }
                            if let Some(addr) = found_at {
                                println!("{} found at {:04x}", value, addr);
                            } else {
                                println!("{} not found", value);
                            }
                        } else {
                            println!("Invalid start address.");
                            continue;
                        }
                    } else {
                        println!("Invalid search value: \"{}\"", cmd[1]);
                        continue;
                    }
                }
                "l" | "list" => {
                    // list (disassemble)
                    let mut addr = self.reg.pc;
                    let mut num = 16;
                    if cmd.len() > 1 {
                        if let Some(a) = self.parse_address(cmd[1]) {
                            addr = a
                        } else {
                            println!("Invalid address or symbol");
                            show_help!(cmd_l);
                            continue;
                        }
                    }
                    if cmd.len() > 2 {
                        if let Some(n) = self.parse_number(cmd[2]) {
                            num = n.u16()
                        }
                        // (else) ignore bad count
                    }
                    self.list_mode = Some(ListMode {
                        lines_remaining: num,
                        saved_ctx: self.reg,
                    });
                    println!("Listing {} lines at {:04X}", num, addr);
                    self.reg.pc = addr;
                    break;
                }
                "his" => {
                    self.show_history();
                    continue;
                }
                "ba" => {
                    // breakpoint add
                    if cmd.len() == 1 {
                        show_help!(cmd_ba);
                        continue;
                    }
                    if let Some(addr) = self.parse_address(cmd[1]) {
                        self.breakpoints.push(Breakpoint::new(
                            addr,
                            false,
                            self.symbol_by_addr(addr),
                            if cmd.len() > 2 { Some(cmd[2..].join(" ")) } else { None },
                        ));
                        println!("Breakpoint {} added at {:04X}", self.breakpoints.len() - 1, addr);
                    } else {
                        println!("Invalid address or symbol.");
                        continue;
                    }
                }
                "bw" => {
                    // watch breakpoint add
                    if cmd.len() == 1 {
                        show_help!(cmd_bw);
                        continue;
                    }
                    if let Some(addr) = self.parse_address(cmd[1]) {
                        self.breakpoints.push(Breakpoint::new(
                            addr,
                            true,
                            self.symbol_by_addr(addr),
                            if cmd.len() > 2 { Some(cmd[2..].join(" ")) } else { None },
                        ));
                        println!("Breakpoint {} added watching {:04X}", self.breakpoints.len() - 1, addr);
                    } else {
                        println!("Invalid address or symbol.");
                        continue;
                    }
                }
                "bd" => {
                    // breakpoint delete
                    if cmd.len() == 1 {
                        show_help!(cmd_bd);
                        continue;
                    }
                    if let Some(index) = self.parse_breakpoint_index(cmd[1]) {
                        let bp = self.breakpoints.remove(index);
                        println!("Breakpoint {} deleted ({})", index, bp);
                    }
                }
                "bi" => {
                    // breakpoint delete
                    if cmd.len() == 1 {
                        show_help!(cmd_bi);
                        continue;
                    }
                    if let Some(index) = self.parse_breakpoint_index(cmd[1]) {
                        let bp = &mut self.breakpoints[index];
                        bp.active = !bp.active;
                        println!(
                            "Breakpoint {} {}activated: {}",
                            index,
                            if bp.active { "" } else { "de" },
                            bp
                        );
                    }
                }
                "bl" => {
                    // breakpoint list
                    if self.breakpoints.is_empty() {
                        println!("No breakpoints are set.");
                        continue;
                    }
                    println!("Breakpoints:");
                    for i in 0..self.breakpoints.len() {
                        println!("  {}. {}", i, self.breakpoints[i]);
                    }
                }
                "bn" => {
                    // breakpoint notes
                    if cmd.len() < 3 {
                        show_help!(cmd_bn);
                        continue;
                    }
                    if let Some(index) = self.parse_breakpoint_index(cmd[1]) {
                        self.breakpoints[index].notes = Some(cmd[2..].join(" "));
                        println!("Breakpoint {} notes updated: {}", index, self.breakpoints[index]);
                    }
                }
                "wd" => {
                    if let Ok(pb) = std::env::current_dir() {
                        if let Some(dir) = pb.to_str() {
                            println!("Current working directory: {}", dir);
                            continue;
                        }
                    }
                    println!("Error: Can't get or display current working directory.");
                }
                "q" | "quit" => return Err(Error::new(ErrorKind::Exit, None, "session terminated by user")),
                "r" | "restart" => {
                    self.reset()?;
                    break;
                }
                "rs" => {
                    self.reset()?;
                    self.step_mode = StepMode::Stepping;
                    break;
                }
                "s" | "step" => {
                    // enter step mode
                    self.step_mode = StepMode::Stepping;
                    println!(
                        "Entering step mode. Type <esc> to exit or <enter> to step over.\nPress any other key to step forward."
                    );
                    break;
                }
                "so" => {
                    // step over then step
                    println!("Stepping over... (destination = {:04X})", self.next_linear_step);
                    self.step_mode = StepMode::StepOverPending(self.next_linear_step);
                    break;
                }
                "sym" => {
                    if self.sym_to_addr.is_empty() {
                        println!("No symbols loaded. Use 'load' to load symbols.");
                        continue;
                    }
                    if cmd.len() == 1 {
                        let mut s = self
                            .sym_to_addr
                            .iter()
                            .map(|(k, v)| (k.as_str(), *v))
                            .collect::<Vec<(&str, u16)>>();
                        s.sort_by(|(s1, _), (s2, _)| s1.cmp(s2));
                        s.iter().for_each(|(sym, addr)| {
                            println!("  {:04X} - {}", addr, sym);
                        });
                        println!("{} symbols listed.", s.len());
                        continue;
                    }
                    if cmd[1].starts_with('?') {
                        let name = &cmd[1][1..];
                        if let Some(addr) = self.symbol_by_name(name) {
                            println!("Symbol {} = {:04X}", name, addr);
                            let syms = self.symbol_by_addr(addr).unwrap();
                            let others = syms
                                .iter()
                                .filter_map(|s| if s.as_str() != name { Some(s.as_str()) } else { None })
                                .collect::<Vec<&str>>();
                            if !others.is_empty() {
                                println!("Additional symbols: {}", others.join(", "));
                            }
                        } else {
                            println!("Symbol {} not found.", cmd[1]);
                        }
                    } else if let Ok(addr) = u16::from_str_radix(cmd[1], 16) {
                        if let Some(syms) = self.symbol_by_addr(addr) {
                            let list = syms.iter().map(|s| s.as_str()).collect::<Vec<&str>>();
                            println!("Symbols at {:04X}: {}", addr, list.join(", "));
                        } else {
                            println!("No symbols found at {:04X}", addr);
                        }
                    } else {
                        println!("Bad address.");
                        show_help!(cmd_sym)
                    }
                }
                "load" => {
                    // load symbols
                    if cmd.len() != 2 {
                        show_help!(cmd_load);
                        continue;
                    }
                    match self.load_symbols(cmd[1]) {
                        Ok(n) => println!("Loaded {} symbols.", n),
                        Err(e) => println!("{}", e),
                    }
                }
                "t" | "trace" => {
                    // toggle trace
                    self.trace = !self.trace;
                    println!("Trace is now {}.", if self.trace { "ON" } else { "OFF" });
                }
                "h" => {
                    for help in COMMAND_HELP {
                        println!("{}", help);
                    }
                }
                _ => {
                    println!("Unknown command. Try 'h' for help.");
                }
            }
        }
        if self.reg.pc != save_pc {
            // whenever we alter the program counter, history must be cleared
            self.clear_history();
        }
        term::flush_keyboard_input();
        self.in_debugger = false;
        Ok(())
    }
    pub fn load_symbols(&mut self, filename: &str) -> Result<usize, Error> {
        let path = std::path::Path::new(filename);
        if let Ok(f) = std::fs::File::open(path) {
            self.clear_symbols();
            let lines = std::io::BufReader::new(f).lines();
            for res in lines {
                if let Err(e) = res {
                    let msg = format!("Error reading symbol file: {}", e);
                    return Err(Error::new(ErrorKind::IO, None, msg.as_str()));
                }
                let line = res.unwrap();
                let comps: Vec<&str> = line.split(',').collect();
                if comps.len() != 2 {
                    return Err(Error::new(ErrorKind::IO, None, "Invalid symbol file format"));
                }
                if let Ok(addr) = u16::from_str_radix(comps[0], 16) {
                    self.add_symbol(addr, comps[1]);
                } else {
                    let msg = format!("Bad format for address in symbol file: {}", comps[1]);
                    return Err(Error::new(ErrorKind::IO, None, msg.as_str()));
                }
            }
            return Ok(self.sym_to_addr.len());
        }
        let msg = format!("Failed to open symbol file {}", filename);
        Err(Error::new(ErrorKind::IO, None, msg.as_str()))
    }
    pub fn try_auto_load_symbols(&mut self, program_path: &str) -> Result<usize, Error> {
        let path = std::path::Path::new(program_path);
        if let Some(stem) = path.file_stem() {
            if let Some(basename) = stem.to_str() {
                let mut pb = path.to_path_buf();
                pb.set_file_name(basename);
                pb.set_extension("sym");
                if let Some(sym_filename) = pb.to_str() {
                    return self.load_symbols(sym_filename);
                }
            }
        }
        Err(Error::new(ErrorKind::IO, None, "Failed to process symbol file path"))
    }
    fn parse_breakpoint_index(&self, index_in_str: &str) -> Option<usize> {
        let mut index = None;
        if let Some(u) = self.parse_number(index_in_str) {
            if u.u16() >= self.breakpoints.len() as u16 {
                println!("Breakpoint does not exist. Use \"bl\" to see current breakpoints.");
            }
            index = Some(u.u16() as usize);
        }
        index
    }
    pub fn get_breakpoint_by_addr(&self, addr: u16, watch_only: bool) -> Option<&Breakpoint> {
        for i in 0..self.breakpoints.len() {
            if addr == self.breakpoints[i].addr
                && self.breakpoints[i].active
                && (!watch_only || self.breakpoints[i].watch)
            {
                return Some(&self.breakpoints[i]);
            }
        }
        None
    }
    pub fn debug_check_for_watch_hit(&self, addr: u16) {
        for bp in &self.breakpoints {
            if addr == bp.addr && bp.active && bp.watch {
                println!("Hit at {:04X}", addr);
                self.watch_hits.borrow_mut().push(addr);
                return;
            }
        }
    }
    fn clear_symbols(&mut self) { self.addr_to_sym.clear(); }
    fn add_symbol(&mut self, addr: u16, name: &str) {
        // add symbol to addr_to_sym table
        if let Some(names) = self.addr_to_sym.get_mut(&addr) {
            // address is already in the symbol table
            // just add this name to the list
            names.push(name.to_string());
        } else {
            self.addr_to_sym.insert(addr, vec![name.to_string()]);
        }
        // add symbol to sym_to_addr table
        self.sym_to_addr.insert(name.to_string(), addr);
    }
    pub fn symbol_by_name(&self, name: &str) -> Option<u16> { self.sym_to_addr.get(name).copied() }
    pub fn symbol_by_addr(&self, addr: u16) -> Option<&Vec<String>> { self.addr_to_sym.get(&addr) }
    fn parse_address(&self, addr_sym: &str) -> Option<u16> {
        if let Some(name) = addr_sym.strip_prefix('?') {
            self.symbol_by_name(name)
        } else if let Ok(addr) = u16::from_str_radix(addr_sym, 16) {
            Some(addr)
        } else {
            None
        }
    }
    fn parse_number(&self, str_num: &str) -> Option<u8u16> {
        let mut number: Option<u8u16> = None;
        let mut negative = false;
        let mut s = str_num.to_string();

        if s.starts_with('-') {
            negative = true;
            s.remove(0);
        }

        if let Some(hex) = s.strip_prefix("0x") {
            if let Ok(val) = u16::from_str_radix(hex, 16) {
                number = Some(u8u16::from_u16_shrink(val));
            }
        } else if let Ok(val) = s.parse::<u16>() {
            number = Some(u8u16::from_u16_shrink(val));
        }

        number.map(|v| {
            if negative {
                let (n, _) = v.force_signed(negative);
                n
            } else {
                v
            }
        })
    }
    fn show_history(&self) {
        let mut count = 0;
        if let Some(history) = self.history.as_ref() {
            count = history.len();
            if count > 0 {
                println!("Showing executed instruction history (length = {})", count);
                for line in history {
                    println!("{}", line);
                }
            }
        }
        if count == 0 {
            println!("No history available.")
        }
    }
    fn clear_history(&mut self) { self.history = None; }
    pub fn pre_instruction_debug_check(&mut self, pc: u16) -> bool {
        if let Some(lm) = self.list_mode.as_mut() {
            if lm.lines_remaining == 0 {
                // done listing; return to debugger
                return true;
            }
            lm.lines_remaining -= 1;
            // when listing, skip all other considerations
            return false;
        }
        if self.faulted {
            // can't run anything if we're faulted
            return true;
        }
        // if break_start is true then always break into debugger when the instruction at program_start is about to be executed
        if self.program_start == pc && config::ARGS.break_start {
            return true;
        }
        // if we're in step mode then we wait for a keypress before executing another instruction
        if let Some(key) = term::get_keyboard_input(self.step_mode == StepMode::Stepping, true) {
            // if we're in step mode then any key other than escape just steps to the next instruction
            if self.step_mode == StepMode::Stepping {
                if key == 27 {
                    println!("Exiting step mode...");
                    return true;
                } else if key == 13 {
                    println!("Stepping over... (destination = {:04X})", self.next_linear_step);
                    self.step_mode = StepMode::StepOverPending(self.next_linear_step);
                }
                return false;
            } else if matches!(self.step_mode, StepMode::StepOverPending(_)) {
                //terminal::flush_keyboard_input();
            } else {
                // if we're not in step mode and not pending a step-over then any key pauses execution and provides a debug prompt
                println!("Execution paused at {:04X} (key={})", pc, key);
                return true;
            }
        }
        let hit_breakpoint = || -> bool {
            let mut breakpoint = false;
            let watch_hits = self.watch_hits.borrow();
            // if we hit a watch then break into the debugger
            if !watch_hits.is_empty() {
                for addr in watch_hits.iter() {
                    if let Some(bp) = self.get_breakpoint_by_addr(*addr, true) {
                        println!("Paused at watch breakpoint: {}", bp);
                    }
                }
                breakpoint = true;
            }
            // if we're at a breakpoint then break into the debugger
            for bp in &self.breakpoints {
                if pc == bp.addr && bp.active {
                    println!("Paused at breakpoint: {}", bp);
                    breakpoint = true;
                }
            }
            breakpoint
        };
        hit_breakpoint()
    }
    pub fn post_instruction_debug_check(&mut self, instruction_pc: u16, outcome: &instructions::Outcome) {
        if let StepMode::StepOverPending(addr) = self.step_mode {
            // time to start our step-over; remember the address we're stepping to
            self.step_mode = StepMode::SteppingOverTo(addr);
            // flush the input buffer so we don't immediately stop stepping once we reach the destination
            // terminal::flush_keyboard_input();
        } else if let StepMode::SteppingOverTo(addr) = self.step_mode {
            if instruction_pc == addr {
                // we hit our destination address so switch back into stepping mode
                self.step_mode = StepMode::Stepping;
            }
        }
        if self.trace || self.step_mode == StepMode::Stepping || self.list_mode.is_some() || config::ARGS.history > 0 {
            let mut sym_plus = false;
            let mut sym = String::from(self.symbol_by_addr(instruction_pc).map_or("", |v| {
                sym_plus = v.len() > 1;
                v[v.len() - 1].as_str()
            }));
            if sym_plus {
                sym.push('+');
            }
            let mut extra_data = String::from(outcome.dbgstr.as_ref().map_or("", |d| d.as_str()));
            // if this instruction doesn't use inherent addressing and we have symbols loaded then check to see if
            // there is a symbol associated with the instruction's effective address and, if there is, add the
            // symbol to the instruction display
            if outcome.inst.flavor.mode != instructions::AddressingMode::Inherent && !self.sym_to_addr.is_empty() {
                if let Some(syms) = self.symbol_by_addr(outcome.inst.ea) {
                    // extra_data = format!("{:04X},", outcome.inst.ea);
                    extra_data.push_str(syms[syms.len() - 1].as_str());
                    if syms.len() > 1 {
                        // there are additional symbols for this address; indicate this with a '+'
                        extra_data.push('+')
                    }
                }
            }
            let mut line = format!(
                "{:04X}: {:10} {:8} {:10} {:10}",
                instruction_pc,
                sym,
                outcome.inst.flavor.desc.name,
                outcome.inst.operand.as_ref().unwrap_or(&String::from("")),
                extra_data,
            );
            if self.list_mode.is_none() {
                line.push_str(format!(" [{} -> ({})]", self.reg, self.reg.cc).as_str());
            }
            if self.trace || self.step_mode == StepMode::Stepping || self.list_mode.is_some() {
                println!("{}", line);
            }
            // we only push trace lines into history if we're configured for history and we're not in list mode
            if config::ARGS.history > 0 && self.list_mode.is_none() {
                if self.history.is_none() {
                    self.history = Some(VecDeque::new());
                }
                if let Some(history) = self.history.as_mut() {
                    history.push_back(line);
                    if history.len() > config::ARGS.history {
                        history.pop_front();
                    }
                }
            }
        }
        if self.list_mode.is_some() {
            // in list mode, we need to just move the PC forward by the size of the instruction we just saw
            self.reg.pc += outcome.inst.size;
        }
        self.next_linear_step = outcome.inst.ctx.pc + outcome.inst.size;
    }
    pub fn fault(&mut self, addr: u16, e: &Error) {
        println!("{}", e);
        println!("System faulted when executing instruction at {:04X}.", addr);
        self.faulted = true;
    }
    pub fn dump_mem(&mut self, addr: u16, count: u16) {
        let mut row = 0;
        const COLS_PER_ROW: u16 = 8;
        loop {
            if row * COLS_PER_ROW >= count {
                break;
            }
            print!(blue!("{:04X}:"), addr + row * COLS_PER_ROW);
            for col in 0..(COLS_PER_ROW * 2) {
                let i = row * COLS_PER_ROW + col % COLS_PER_ROW;
                let (index, overflow) = addr.overflowing_add(i);
                if overflow {
                    row = count;
                    break;
                }
                let b = self._read_u8(memory::AccessType::System, index, None).unwrap();
                if col < COLS_PER_ROW {
                    if i < count {
                        print!(" {:02X}", b);
                    } else {
                        print!("   ");
                    }
                } else {
                    if col % COLS_PER_ROW == 0 {
                        print!(" ");
                    }
                    if i < count {
                        print!(
                            " {}",
                            if b.is_ascii_alphanumeric() || b.is_ascii_graphic() || b.is_ascii_punctuation() {
                                b as char
                            } else {
                                '.'
                            }
                        );
                    }
                }
            }
            row += 1;
            println!();
        }
    }
}
