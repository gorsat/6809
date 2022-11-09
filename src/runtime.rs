/// Implements the runtime engine of the simulator.
use crate::instructions::{PPPostByte, TEPostByte};

use super::*;
use memory::AccessType;

impl Core {
    /// Resets the 6809 by clearing the registers and
    /// then loading the program counter from the reset vector
    /// (or using the override value if one was specified on the command line.)
    pub fn reset(&mut self) -> Result<(), Error> {
        self.reg.reset();
        if let Some(addr) = config::ARGS.reset_vector {
            self.force_reset_vector(addr)?
        }
        self.reg.pc = self._read_u16(memory::AccessType::System, 0xfffe, None)?;
        self.program_start = self.reg.pc;
        self.faulted = false;
        Ok(())
    }
    /// Writes the given address to the reset vector
    pub fn force_reset_vector(&mut self, addr: u16) -> Result<(), Error> {
        self._write_u8u16(memory::AccessType::System, 0xfffe, u8u16::u16(addr))
    }
    /// Displays current perf information to stdout
    fn report_perf(&self) {
        if !config::ARGS.perf {
            return;
        }
        let delta = self.start_time.elapsed().as_secs_f64();
        info!(
            "Executed {} instructions in {:.2} sec; {:.3} MIPS; effective clock: {:.3} MHz",
            self.instruction_count,
            delta,
            self.instruction_count as f64 / (delta * 1.0e6),
            self.clock_cycles as f64 / (delta * 1.0e6)
        );
        info!(
            "prep_time = {:.3} sec; eval_time = {:.3} sec; commit_time = {:.3} sec",
            self.prep_time.as_secs_f64(),
            self.eval_time.as_secs_f64(),
            self.commit_time.as_secs_f64()
        );
    }
    /// Starts executing instructions at the current program counter.  
    /// Does not set or read any registers before attempting to execute.  
    /// Will attempt to execute until a SWI* instruction or a fault is encountered.
    /// A normal exit results in Ok; anything else results in Err.
    pub fn exec(&mut self) -> Result<(), Error> {
        self.start_time = Instant::now();
        loop {
            let temp_pc = self.reg.pc;
            if let Err(e) = self.exec_one() {
                if e.kind == ErrorKind::Exit {
                    // this is a normal exit; return Ok
                    return Ok(());
                }
                // if the debugger is disabled then stop executing and return the error
                // otherwise, the debug cli will be invoked when we try to exec the next instruction (due to the fault)
                if !config::debug() {
                    return Err(e);
                } else {
                    self.fault(temp_pc, &e);
                }
            }
        }
    }
    /// Helper function for exec.  
    /// Wraps calls to exec_next and adds debug checks and SWI* checks.
    fn exec_one(&mut self) -> Result<(), Error> {
        if config::debug() && self.pre_instruction_debug_check(self.reg.pc) {
            self.debug_cli()?;
        }
        let temp_pc = self.reg.pc;
        let outcome = self.exec_next(self.list_mode.is_none())?;
        if config::help_humans() {
            self.post_instruction_debug_check(temp_pc, &outcome);
        }
        use instructions::Meta::*;
        if matches!(outcome.meta, Some(SWI) | Some(SWI2) | Some(SWI3)) {
            // encountered SWI
            self.report_perf();
            // if the debugger is enabled then break now
            if config::debug() {
                info!("Encountered SWI. Breaking into debugger...");
                self.debug_cli()?;
            } else {
                // otherwise, just stop executing and return exit error
                info!("Encountered SWI. Program execution terminated.");
                return Err(Error::new(ErrorKind::Exit, None, ""));
            }
        }
        Ok(())
    }
    /// Attempt to execute the next instruction at PC.  
    /// If commit=true then commit any/all changes to the machine state.
    /// Otherwise, the changes are only reflected in the instruction::Outcome object.
    /// If list_mode.is_some() then the instruction is not evaluated and Outcome reflects
    /// the state prior to the instruction.
    pub fn exec_next(&mut self, commit: bool) -> Result<instructions::Outcome, Error> {
        let mut start = Instant::now();
        let mut inst = instructions::Instance::new(&self.reg, None);
        let mut op16: u16 = 0; // 16-bit representation of the opcode
        let mut live_ctx: registers::Set = self.reg;

        // get the base op code
        loop {
            inst.buf[inst.size as usize] = self._read_u8(AccessType::Program, live_ctx.pc + inst.size, None)?;
            op16 |= inst.buf[inst.size as usize] as u16;
            inst.size += 1;
            if inst.size == 1 && instructions::is_high_byte_of_16bit_instruction(inst.buf[0]) {
                op16 = op16 << 8;
                continue;
            }
            break;
        }
        // keep track of how many bytes the opcode takes up
        inst.opsize = inst.size;
        // get the instruction Flavor
        // Note: doing this with if/else rather than ok_or or ok_or_else because it performs better
        inst.flavor = if let Some(flavor) = instructions::opcode_to_flavor(op16) {
            flavor
        } else {
            return Err(runtime_err!(
                Some(self.reg),
                "Bad instruction: {:04X} found at {:04X}",
                op16,
                self.reg.pc
            ));
        };
        self.process_addressing_mode(&mut inst, &mut live_ctx)?;

        assert!(inst.size >= inst.flavor.detail.sz);
        // adjust the program counter before evaluating instructions
        live_ctx.pc = self.checked_pc_add(live_ctx.pc, inst.size, &inst)?;

        let mut o = instructions::Outcome::new(inst, live_ctx);
        // track how long all this preparation took
        self.prep_time += start.elapsed();
        start = Instant::now();

        // evaluate the instruction if we're not in list mode
        if self.list_mode.is_none() {
            (o.inst.flavor.desc.eval)(&self, &mut o)?;
        }
        // this creates a string containing the effective address and the contents of 2 bytes at that address
        // it was helpful in debugging the assembler and runtime but it's pretty confusing otherwise.
        // if config::help_humans() && o.inst.flavor.mode != instructions::AddressingMode::Inherent {
        //     o.dbgstr = Some(format!(
        //         "{:04X}: {:02X} {:02X}",
        //         o.inst.ea,
        //         self.mem[o.inst.ea as usize],
        //         self.mem[0xffff & (1 + o.inst.ea as usize)]
        //     ));
        // }
        self.eval_time += start.elapsed();
        start = Instant::now();

        // if caller wants to commit the changes and we're not in list mode then commit now
        if commit && self.list_mode.is_none() {
            self.reg = o.new_ctx;
            // and complete any writes to the address space
            if let Some(v) = o.writes.as_ref() {
                for w in v {
                    self._write_u8u16(w.at, w.addr, w.val)?;
                }
            }
        }
        self.commit_time += start.elapsed();

        self.instruction_count += 1;
        self.clock_cycles += o.inst.flavor.detail.clk as u64;
        Ok(o)
    }
    /// Increase the program counter by the given value (rhs).
    /// Returns Error::Runtime in the case of overflow.
    /// Otherwise, Ok.
    fn checked_pc_add(&self, pc: u16, rhs: u16, inst: &instructions::Instance) -> Result<u16, Error> {
        // avoiding ok_or and ok_or_else to increase performance
        // ok_or would invoke the runtime_err! macro every time (regardless of result)
        // ok_or_else seems to be slightly slower than manually checking with if/else
        if let Some(pc) = pc.checked_add(rhs) {
            Ok(pc)
        } else {
            Err(runtime_err!(
                Some(self.reg),
                "Instruction overflow: instruction {} at {:04X}",
                inst.flavor.desc.name,
                self.reg.pc
            ))
        }
    }

    /// Performs the general setup work for an instruction based on addressing mode.
    /// This includes determining the effective address for the instruction,
    /// updating the instruction size, modifying any registers that are changed by the addressing mode (e.g. ,X+),
    /// and providing a disassembled string representing the operand.
    /// Changes are reflected in the provided inst and live_ctx objects.
    fn process_addressing_mode(
        &self, inst: &mut instructions::Instance, live_ctx: &mut registers::Set,
    ) -> Result<(), Error> {
        match inst.flavor.mode {
            instructions::AddressingMode::Immediate => {
                // effective address is the current PC
                inst.ea = self.checked_pc_add(live_ctx.pc, inst.size, inst)?;
                let addr_size = inst.flavor.detail.sz - inst.size;
                assert!(addr_size > 0 && addr_size < 3);
                let data = self._read_u8u16(AccessType::Program, inst.ea, addr_size)?;
                inst.size += addr_size;
                if config::help_humans() {
                    inst.operand = Some(match inst.flavor.desc.pbt {
                        instructions::PBT::NA => format!("#${}", data),
                        instructions::PBT::TransferExchange => TEPostByte::to_string(data.u8()),
                        instructions::PBT::PushPull => {
                            PPPostByte::to_string(data.u8(), inst.flavor.desc.reg == registers::Name::U)
                        }
                    });
                }
            }
            instructions::AddressingMode::Direct => {
                // effective address is u16 whose high byte = DP
                // and low byte is stored at the current PC
                inst.ea = ((live_ctx.dp as u16) << 8)
                    | (self._read_u8(
                        AccessType::Program,
                        self.checked_pc_add(live_ctx.pc, inst.size, inst)?,
                        None,
                    )? as u16);
                inst.size += 1;
                if config::help_humans() {
                    inst.operand = Some(format!("${:04X}", inst.ea));
                }
            }
            instructions::AddressingMode::Extended => {
                // effective address is u16 stored at current PC
                inst.ea = self._read_u16(
                    AccessType::Program,
                    self.checked_pc_add(live_ctx.pc, inst.size, inst)?,
                    None,
                )?;
                inst.size += 2;
                if config::help_humans() {
                    inst.operand = Some(format!("${:04X}", inst.ea));
                }
            }
            instructions::AddressingMode::Inherent => {
                // nothing to do. op code itself is sufficient
            }
            instructions::AddressingMode::Relative => {
                let offset_size = inst.flavor.detail.sz - inst.size;
                let offset = self._read_u8u16(
                    AccessType::Program,
                    self.checked_pc_add(live_ctx.pc, inst.size, inst)?,
                    offset_size,
                )?;
                inst.size += offset_size;
                inst.ea = u8u16::u16(self.checked_pc_add(live_ctx.pc, inst.size, inst)?)
                    .signed_offset(offset)
                    .u16();
                if config::help_humans() {
                    inst.operand = Some(format!("{}", offset.i16()));
                }
            }
            instructions::AddressingMode::Indexed => {
                // todo: move this to a function?
                // read the post-byte
                let pb = self._read_u8(
                    AccessType::Program,
                    self.checked_pc_add(live_ctx.pc, inst.size, inst)?,
                    None,
                )?;
                inst.size += 1;
                // is this indirect mode?
                let indirect = (pb & 0b10010000) == 0b10010000;
                // note which register (preg) the register field (rr) is referencing
                let rr = (pb & 0b01100000) >> 5;
                let (ir_ptr, ir_str): (&mut u16, &str) = match rr {
                    0 => (&mut live_ctx.x, "X"),
                    1 => (&mut live_ctx.y, "Y"),
                    2 => (&mut live_ctx.u, "U"),
                    3 => (&mut live_ctx.s, "S"),
                    _ => unreachable!(),
                };
                match pb & 0x8f {
                    0..=0b11111 => {
                        // ,R + 5 bit offset
                        let offset = ((pb & 0b11111) | if pb & 0b10000 != 0 { 0b11100000 } else { 0 }) as i8;
                        let (addr, _) = u16::overflowing_add(*ir_ptr, offset as u16);
                        inst.ea = addr;
                        if config::help_humans() {
                            inst.operand = Some(format!("{},{}", offset, ir_str))
                        }
                    }
                    0b10000000 => {
                        // ,R+
                        if indirect {
                            return Err(Error::new(
                                ErrorKind::Syntax,
                                Some(self.reg),
                                format!("Illegal indirect indexed addressing mode [,R+] at {:04X}", self.reg.pc)
                                    .as_str(),
                            ));
                        }
                        inst.ea = *ir_ptr;
                        let (r, _) = (*ir_ptr).overflowing_add(1);
                        *ir_ptr = r;
                        if config::help_humans() {
                            inst.operand = Some(format!(",{}+", ir_str));
                        }
                    }
                    0b10000001 => {
                        // ,R++
                        inst.ea = *ir_ptr;
                        let (r, _) = (*ir_ptr).overflowing_add(2);
                        *ir_ptr = r;
                        if config::help_humans() {
                            inst.operand = Some(format!(",{}++", ir_str));
                        }
                    }
                    0b10000010 => {
                        // ,-R
                        if indirect {
                            return Err(Error::new(
                                ErrorKind::Syntax,
                                Some(self.reg),
                                format!("Illegal indirect indexed addressing mode [,-R] at {:04X}", self.reg.pc)
                                    .as_str(),
                            ));
                        }
                        let (r, _) = (*ir_ptr).overflowing_sub(1);
                        *ir_ptr = r;
                        inst.ea = *ir_ptr;
                        if config::help_humans() {
                            inst.operand = Some(format!(",-{}", ir_str));
                        }
                    }
                    0b10000011 => {
                        // ,--R
                        let (r, _) = (*ir_ptr).overflowing_sub(2);
                        *ir_ptr = r;
                        inst.ea = *ir_ptr;
                        if config::help_humans() {
                            inst.operand = Some(format!(",--{}", ir_str));
                        }
                    }
                    0b10000100 => {
                        // EA = ,R + 0 offset
                        inst.ea = *ir_ptr;
                        if config::help_humans() {
                            inst.operand = Some(format!(",{}", ir_str));
                        }
                    }
                    0b10000101 => {
                        // EA = ,R + B offset
                        let (addr, _) = u16::overflowing_add(*ir_ptr, (live_ctx.b as i8) as u16);
                        inst.ea = addr;
                        if config::help_humans() {
                            inst.operand = Some(format!("B,{}", ir_str));
                        }
                    }
                    0b10000110 => {
                        // EA = ,R + A offset
                        let (addr, _) = u16::overflowing_add(*ir_ptr, (live_ctx.a as i8) as u16);
                        inst.ea = addr;
                        if config::help_humans() {
                            inst.operand = Some(format!("A,{}", ir_str));
                        }
                    }
                    // 0b10000111 => {} invalid
                    0b10001000 => {
                        // EA = ,R + 8 bit offset
                        let offset = self._read_u8(AccessType::Program, live_ctx.pc + inst.size, None)? as i8;
                        inst.size += 1;
                        let (addr, _) = u16::overflowing_add(*ir_ptr, offset as u16);
                        inst.ea = addr;
                        if config::help_humans() {
                            inst.operand = Some(format!("{},{}", offset, ir_str));
                        }
                    }
                    0b10001001 => {
                        // ,R + 16 bit offset
                        let offset = self._read_u16(AccessType::Program, live_ctx.pc + inst.size, None)? as i16;
                        inst.size += 2;
                        let (addr, _) = u16::overflowing_add(*ir_ptr, offset as u16);
                        inst.ea = addr;
                        if config::help_humans() {
                            inst.operand = Some(format!("{},{}", offset, ir_str));
                        }
                    }
                    // 0b10001010 => {} invalid
                    0b10001011 => {
                        // ,R + D offset
                        let (addr, _) = u16::overflowing_add(*ir_ptr, live_ctx.d);
                        inst.ea = addr;
                        if config::help_humans() {
                            inst.operand = Some(format!("D,{}", ir_str));
                        }
                    }
                    0b10001100 => {
                        // ,PC + 8 bit offset
                        let offset = self._read_u8(AccessType::Program, live_ctx.pc + inst.size, None)? as i8;
                        inst.size += 1;
                        let (addr, _) = u16::overflowing_add(live_ctx.pc, offset as u16);
                        inst.ea = addr;
                        if config::help_humans() {
                            inst.operand = Some(format!("{},PC", offset));
                        }
                    }
                    0b10001101 => {
                        // ,PC + 16 bit offset
                        let offset = self._read_u16(AccessType::Program, live_ctx.pc + inst.size, None)? as i16;
                        inst.size += 2;
                        let (addr, _) = u16::overflowing_add(live_ctx.pc, offset as u16);
                        inst.ea = addr;
                        if config::help_humans() {
                            inst.operand = Some(format!("{},PC", offset));
                        }
                    }
                    // 0b10001110 => {} invalid
                    0b10001111 => {
                        // EA = [,address]
                        inst.ea = self._read_u16(AccessType::Program, live_ctx.pc + inst.size, None)?;
                        if config::help_humans() {
                            inst.operand = Some(format!("[{:04X}]", inst.ea));
                        }
                        inst.size += 2;
                    }
                    _ => {
                        return Err(Error::new(
                            ErrorKind::Syntax,
                            Some(self.reg),
                            format!(
                                "Invalid indexed addressing post-byte {:02X} in instruction at {:04X}",
                                pb, self.reg.pc
                            )
                            .as_str(),
                        ));
                    }
                }
                // if indirect flag is set then set inst.ea to self.ram[inst.ea]
                if indirect {
                    inst.ea = self._read_u16(AccessType::Generic, inst.ea, None)?;
                }
            }
            _ => panic!("Invalid addressing mode! {:?}", inst.flavor.mode),
        }
        Ok(())
    }
}
