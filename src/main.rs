//! # A 6809 Assembler, Simulator and Debugger written in Rust.
//!
//! ## Getting Started
//! To assemble and run a program:
//! ```
//! cargo run -- -r /path/to/program.asm
//! ```
//! ...or if you've already built the binary then just...
//! ```
//! 6809 -r /path/to/program.asm
//! ```
//! ## Options
//! Help for command line options is available using -h or --help.
#[macro_use]
mod macros;
#[macro_use]
mod term;
mod acia;
mod assembler;
mod config;
mod core;
mod debug;
mod error;
mod hex;
mod instructions;
mod memory;
mod obj;
mod parse;
mod pathid;
mod program;
mod registers;
mod runtime;
mod test;
mod u8oru16;
use crate::assembler::Assembler;
use hex::HexRecordCollection;
use std::collections::{HashMap, VecDeque};
use std::ffi::OsStr;
use std::path::Path;
use std::result::Result;
use std::time::Instant;
use std::{fmt, io};
pub(crate) use u8oru16::u8u16;
pub(crate) use {crate::core::Core, crate::error::*, program::*};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    config::init();
    term::init();
    // process_file does all the work
    if let Err(e) = process_file(config::ARGS.file.as_str()) {
        println!("{}", e);
        return Err(Box::new(e));
    }
    Ok(())
}
/// process_file drives the top level functionality (assemble, load, run) of the app
fn process_file(filename: &str) -> Result<(), Error> {
    let mut program: Option<Program> = None;
    let mut hex: Option<HexRecordCollection> = None;
    let path = Path::new(filename);
    let ext = path.extension().and_then(OsStr::to_str).unwrap_or("");
    match ext.to_ascii_lowercase().as_str() {
        "asm" | "s" => {
            // the file looks like assembly source code, so try to assemble it
            let asm = Assembler::new();
            info!("Assembling {}", filename);
            program = Some(asm.assemble_from_file(filename)?);
        }
        "hex" => {
            // the file looks like machine code in hex format; read it
            hex = Some(HexRecordCollection::read_from_file(path)?);
            info!("Successfully loaded hex file {}", filename);
        }
        _ => return Err(general_err!("unrecognized file type")),
    }
    if config::run() {
        // we're going to try to run the program; create a CPU simulator
        let mut core = Core::new(
            config::ARGS.ram_top,
            if config::ARGS.acia_disable {
                None
            } else {
                Some(config::ARGS.acia_addr)
            },
        );
        if !config::debug() {
            info!("Debugger not enabled. Press <ctrl-c> to exit.")
        }
        if let Some(program) = program.as_ref() {
            // we have a Program object; load it into the simulator
            core.load_program(program, Some(filename))?;
        } else if let Some(hex) = hex.as_ref() {
            // we have machine code; load it into the simulator
            core.load_hex(hex, Some(filename))?;
        }
        info!("Executing {}", filename);
        // put the simulator in a reset state and start running the program
        core.reset()?;
        core.exec()?;

        if let Some(program) = program {
            if !config::debug() {
                // if there are any test criteria then check them now
                core.check_criteria(&program.results)?;
            }
        }
    }
    Ok(())
}
#[cfg(test)]
mod tests {

    use regex::Regex;

    use super::*;
    use std::fs;
    #[test]
    pub fn rudimentary() -> Result<(), Error> {
        const PROGRAM01: &[u8] = &[0x96, 0x40, 0x9b, 0x41, 0x97, 0x42, 0x3f];
        let mut core = Core::new(0xefff, None);
        info!("Starting MC6809E CPU rudimentary test...");
        core.reg.a = 0xaa;
        core.reg.b = 0xbb;
        core.ram.write().unwrap()[core.reg.pc as usize] = 0x12;
        core.reg.s = core.reg.s.wrapping_sub(1u16);
        assert!(core.reg.s as usize == core.ram.write().unwrap().len() - 1);
        core.ram.write().unwrap()[core.reg.s as usize] = 0xff;
        core.ram.write().unwrap()[core.reg.s as usize] = 0;

        core.reg.cc.set(registers::CCBit::C, true);
        core.reg.cc.set(registers::CCBit::H, true);
        assert!(core.reg.cc.reg == 0x21);

        println!("{} -> ({})", core.reg, core.reg.cc);

        core.reset()?;
        // after reset CC bits I and F should be set; all other bits should be 0
        assert!(core.reg.cc.reg == 0x50);

        // load program
        core.load_bytes(PROGRAM01, 0u16)?;
        // set parameters in ram
        core.ram.write().unwrap()[0x40] = 0x38;
        core.ram.write().unwrap()[0x41] = 0x2b;

        info!("Running simple test program...");
        let mut step = 0;
        loop {
            let temp_pc = core.reg.pc;
            let outcome = core.exec_next(true)?;
            step += 1;
            println!(
                "{:2} {:04x}: {:5}  {:5}  [{} -> ({})]",
                step,
                temp_pc,
                outcome.inst.flavor.desc.name,
                outcome.inst.operand.unwrap_or_default(),
                core.reg,
                core.reg.cc
            );
            if outcome.inst.flavor.detail.op == 0x3f {
                break;
            }
            if step > PROGRAM01.len() {
                return Err(Error::new(
                    ErrorKind::Runtime,
                    None,
                    "Failed to find end of basic test program.",
                ));
            }
        }
        core.dump_mem(0x40, 3);
        // check outcome
        assert!(core.ram.write().unwrap()[0x42] == 0x63);
        info!("Rudimentary test complete.");
        Ok(())
    }
    #[test]
    fn extended_basic() -> Result<(), Error> {
        // assemble ExBasROM.asm and validate the result against ExBasROM.lst
        let asm_filename = "test/basic/ExBasROM.asm";
        let lst_filename = "test/basic/ExBasROM.lst";
        let output_filename = "test/basic/extended_basic_test.lst";
        let re_data_line = Regex::new(
            r"(?i)^([0-9a-f]{4}) ([0-9a-f]{4}) ([0-9a-f]{2})\s?([0-9a-f]{2})?\s?([0-9a-f]{2})?\s?([0-9a-f]{2})?\s?([0-9a-f]{2})?\s?([0-9a-f]{2})?",
        ).unwrap();
        let re_cont_line = Regex::new(r"(?i)^[^\n]\s+([0-9a-f]{2})?\s?([0-9a-f]{2})?\s?([0-9a-f]{2})?\s?([0-9a-f]{2})?\s?([0-9a-f]{2})?\s?([0-9a-f]{2})?.*$").unwrap();
        let asm = Assembler::new();
        let program = asm.assemble_from_file(asm_filename)?;
        let mut output_file = fs::File::create(output_filename)?;
        program.write_listing(&mut output_file)?;
        let old_list = fs::read_to_string(lst_filename)?;
        let old_lines = old_list.lines();
        struct ListLine {
            num: usize,
            addr: u16,
            data: Vec<u8>,
        }
        let mut data_lines: HashMap<usize, ListLine> = HashMap::new();
        let mut current_data_line = 0;
        for (lst_line_index, old_line) in old_lines.enumerate() {
            let lst_line_num = lst_line_index + 1;
            if let Some(bytes) = re_cont_line.captures(old_line) {
                let ll = data_lines
                    .get_mut(&current_data_line)
                    .unwrap_or_else(|| panic!("invalid data continuation, .lst line {lst_line_num}"));
                for b in bytes.iter().skip(1).flatten() {
                    ll.data.push(
                        u8::from_str_radix(b.as_str(), 16)
                            .unwrap_or_else(|_| panic!("invalid data format, .lst line {lst_line_num}")),
                    );
                }
            } else if let Some(cap) = re_data_line.captures(old_line) {
                let mut ll = ListLine {
                    num: cap
                        .get(1)
                        .unwrap_or_else(|| panic!("missing src line number, .lst line {lst_line_num}"))
                        .as_str()
                        .parse::<usize>()
                        .unwrap_or_else(|_| panic!("invalid src line number syntax, .lst line {lst_line_num}")),
                    addr: u16::from_str_radix(
                        cap.get(2)
                            .unwrap_or_else(|| panic!("missing address, .lst line {lst_line_num}"))
                            .as_str(),
                        16,
                    )
                    .unwrap_or_else(|_| panic!("invalid address syntax, .lst line {lst_line_num}")),
                    data: Vec::new(),
                };
                for b in cap.iter().skip(3).flatten() {
                    ll.data.push(
                        u8::from_str_radix(b.as_str(), 16)
                            .unwrap_or_else(|_| panic!("invalid data format, .lst line {lst_line_num}")),
                    );
                }
                current_data_line = ll.num;
                if let Some(dup) = data_lines.insert(ll.num, ll) {
                    panic!("duplicate of src line {} encountered in .lst", dup.num);
                }
            } else {
                current_data_line = 0;
            }
        }
        for pl in program.lines {
            if let Some(prod) = pl.obj {
                if let Some(bob) = prod.bob_ref() {
                    if let Some(data) = bob.data.as_ref() {
                        let ll = data_lines
                            .get(&pl._prog_line_num)
                            .unwrap_or_else(|| panic!("no data for line {} of src", pl._prog_line_num));
                        let mut i = 0;
                        for u in data.iter() {
                            if let Some(b) = u.msb() {
                                if b != ll.data[i] {
                                    panic!(
                                        "data mismatch at byte index {i} on line {} of src ({:02X} != {:02X})",
                                        ll.num, b, ll.data[i]
                                    )
                                }
                                i += 1;
                            }
                            if u.lsb() != ll.data[i] {
                                panic!(
                                    "data mismatch at byte index {i} on line {} of src ({:02X} != {:02X})",
                                    ll.num,
                                    u.lsb(),
                                    ll.data[i]
                                )
                            }
                            i += 1;
                        }
                        data_lines.remove(&pl._prog_line_num);
                    }
                }
            }
        }
        if !data_lines.is_empty() {
            let mut list: Vec<&ListLine> = data_lines.values().collect();
            list.sort_by_key(|ll| ll.num);
            list.iter().for_each(|ll| {
                let mut data = String::new();
                for b in ll.data.iter() {
                    data.push_str(&format!("{:02X} ", b));
                }
                println!("line:{:04} addr:{:04X} data:{data}", ll.num, ll.addr);
            });
            panic!(
                "{} data lines generated by assembler do not exist in the .lst",
                data_lines.len()
            )
        }
        Ok(())
    }
    #[test]
    fn various_programs() -> Result<(), Error> {
        // try to load and run each .asm file in the ./test directory
        // all of them should run successfully and pass all associated test criteria
        const TEST_PATH: &str = "test";
        println!("Attempting to run all .asm files in {}", TEST_PATH);
        let mut entries = fs::read_dir(TEST_PATH)?
            .map(|res| res.map(|e| e.path()))
            .collect::<Result<Vec<_>, io::Error>>()?;
        entries.sort();
        for p in entries {
            if !p.is_file() {
                continue;
            }
            if let Some(ext) = p.extension() {
                if !ext.eq_ignore_ascii_case("asm") {
                    continue;
                }
                if let Err(e) = process_file(p.to_str().unwrap()) {
                    println!("Error processing file {}: {}", p.display(), e);
                    return Err(e);
                }
            }
        }
        Ok(())
    }
    #[test]
    fn runtime_errors() -> Result<(), Error> {
        // try to load and run each .asm file in the ./test/errors directory
        // every one of them should cleanly return an ErrorKind::Runtime error
        const TEST_PATH: &str = "test/errors";
        println!("Attempting to run all .asm files in {}", TEST_PATH);
        let mut entries = fs::read_dir(TEST_PATH)?
            .map(|res| res.map(|e| e.path()))
            .collect::<Result<Vec<_>, io::Error>>()?;
        entries.sort();
        for pb in entries {
            if !pb.is_file() {
                continue;
            }
            if let Some(ext) = pb.extension() {
                if !ext.eq_ignore_ascii_case("asm") {
                    continue;
                }
                if let Some(msg) = match process_file(pb.to_str().unwrap()) {
                    Err(e) if e.kind == ErrorKind::Runtime => None,
                    Err(e) if e.kind == ErrorKind::Recursion => None,
                    Err(e) => Some(e.to_string()),
                    Ok(()) => Some("Ok()".to_string()),
                } {
                    panic!(
                        "Expected ErrorKind::Runtime when running {} but got {}",
                        pb.to_str().unwrap(),
                        msg
                    )
                }
            }
        }
        Ok(())
    }
}
