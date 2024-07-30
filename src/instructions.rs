#![allow(non_snake_case)]

use crate::core::InterruptType;

use super::*;
use memory::AccessType;
use std::{fmt::Debug, sync::Once};

pub static mut FLAVOR_TABLE: [Option<Flavor>; 768] = [None; 768];
pub static mut DESC_BY_NAME: Option<HashMap<&'static str, &'static Descriptor>> = None;
static INIT: Once = Once::new();
fn ft_index(op_code: u16) -> Option<usize> {
    match op_code & 0xff00 {
        0 => Some(op_code as usize),
        0x1000 => Some(0x100 + (op_code & 0xff) as usize),
        0x1100 => Some(0x200 + (op_code & 0xff) as usize),
        _ => None,
    }
}
pub fn opcode_to_flavor(op: u16) -> Option<&'static Flavor> {
    // SAFETY: FLAVOR_TABLE is a static mut that is initialized once by init()
    unsafe { instructions::FLAVOR_TABLE[ft_index(op)?].as_ref() }
}
pub fn name_to_descriptor(name: &str) -> Option<&'static Descriptor> {
    // SAFETY: DESC_BY_NAME is a static mut that is initialized once by init()
    unsafe { DESC_BY_NAME.as_ref()?.get(name).copied() }
}
/// Initialize static lookup tables.
pub fn init() {
    INIT.call_once(|| {
        let mut dbn = HashMap::new();
        for desc in DESCRIPTORS {
            dbn.insert(desc.name, desc);
            for detail in desc.md {
                // SAFETY: FLAVOR_TABLE is a static mut that is initialized once by init()
                unsafe {
                    FLAVOR_TABLE[ft_index(detail.op).unwrap()] = Some(Flavor {
                        desc,
                        mode: instructions::AddressingMode::from(detail.am),
                        detail,
                    })
                }
            }
        }
        // SAFETY: DESC_BY_NAME is a static mut that is initialized once by init()
        unsafe { DESC_BY_NAME = Some(dbn) }
    });
}

/// All the supported addressing modes. Note that the assembler's notion of addressing mode
/// can initially vary from what the runtime eventually sees.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum AddressingMode {
    Immediate = 0,
    Direct,
    Indexed,
    Extended,
    Inherent,
    Relative,

    /****** added these only for the assembler ******/
    Register,
    Offset,
    IncDec,
    PCRelative,
    /************************************************/
    Error,
}
impl From<usize> for AddressingMode {
    fn from(am: usize) -> AddressingMode {
        match am {
            0 => AddressingMode::Immediate,
            1 => AddressingMode::Direct,
            2 => AddressingMode::Indexed,
            3 => AddressingMode::Extended,
            4 => AddressingMode::Inherent,
            5 => AddressingMode::Relative,
            6 => AddressingMode::Register,
            7 => AddressingMode::Offset,
            8 => AddressingMode::IncDec,
            9 => AddressingMode::PCRelative,
            _ => {
                panic!("Invalid AddressingMode")
            }
        }
    }
}

/// Post-Byte Type - the type of post-byte required for a given instruction.
#[derive(Debug)]
#[allow(clippy::upper_case_acronyms)]
pub enum PBT {
    NA = 0,
    TransferExchange = 1,
    PushPull = 2,
}

/// A namespace to organize some helpers for processing post-byte codes for transfer and exchange instructions
#[allow(non_snake_case)]
pub mod TEPostByte {
    use super::*;

    const D: u8 = 0b0000;
    const X: u8 = 0b0001;
    const Y: u8 = 0b0010;
    const U: u8 = 0b0011;
    const S: u8 = 0b0100;
    const PC: u8 = 0b0101;
    const A: u8 = 0b1000;
    const B: u8 = 0b1001;
    const CC: u8 = 0b1010;
    const DP: u8 = 0b1011;
    // the given string must be uppercase
    pub fn nibble(reg: &str) -> Option<u8> {
        match reg {
            "D" => Some(D),
            "X" => Some(X),
            "Y" => Some(Y),
            "U" => Some(U),
            "S" => Some(S),
            "PC" => Some(PC),
            "A" => Some(A),
            "B" => Some(B),
            "CC" => Some(CC),
            "DP" => Some(DP),
            _ => None,
        }
    }
    pub fn nibble_str(nib: u8) -> &'static str {
        match nib {
            D => "D",
            X => "X",
            Y => "Y",
            U => "U",
            S => "S",
            PC => "PC",
            A => "A",
            B => "B",
            CC => "CC",
            DP => "DP",
            _ => "",
        }
    }
    pub fn is_valid(pb: u8) -> bool {
        let r1 = pb >> 4;
        let r2 = pb & 0xf;
        (r1 != r2) && (((r1 >= A) && (r2 >= A)) || ((r1 < A) && (r2 < A)))
    }
    pub fn make(src: &str, dst: &str) -> Option<u8> {
        if let Some(s) = nibble(src) {
            if let Some(d) = nibble(dst) {
                let pb = (s << 4) | d;
                if is_valid(pb) {
                    return Some(pb);
                }
            }
        }
        None
    }
    pub fn to_string(pb: u8) -> String {
        let mut out = String::new();
        out.push_str(nibble_str(pb >> 4));
        out.push(',');
        out.push_str(nibble_str(pb & 0x0f));
        out
    }
    pub fn to_registers(pb: u8) -> Option<(registers::Name, registers::Name)> {
        if !is_valid(pb) {
            return None;
        }
        let r1 = registers::Name::from_str(nibble_str(pb >> 4));
        let r2 = registers::Name::from_str(nibble_str(pb & 0x0f));
        if r1 == registers::Name::Z || r2 == registers::Name::Z {
            None
        } else {
            Some((r1, r2))
        }
    }
}
/// A namespace to organize some helpers for processing instruction post-byte codes for PSH and PUL
#[allow(non_snake_case)]
pub mod PPPostByte {
    pub const PC: u8 = 0b10000000;
    pub const SU: u8 = 0b01000000;
    pub const Y: u8 = 0b00100000;
    pub const X: u8 = 0b00010000;
    pub const DP: u8 = 0b00001000;
    pub const B: u8 = 0b00000100;
    pub const A: u8 = 0b00000010;
    pub const CC: u8 = 0b00000001;
    pub const _STR: [&str; 8] = ["CC", "A", "B", "DP", "X", "Y", "U", "PC"];
    // the given string must be uppercase
    pub fn from_str(reg: &str) -> Option<u8> {
        match reg {
            "X" => Some(X),
            "Y" => Some(Y),
            "S" => Some(SU),
            "U" => Some(SU),
            "PC" => Some(PC),
            "A" => Some(A),
            "B" => Some(B),
            "CC" => Some(CC),
            "DP" => Some(DP),
            _ => None,
        }
    }
    pub fn make(regs: &Vec<String>) -> Option<u8> {
        let mut pb = 0u8;
        for reg in regs {
            if let Some(f) = from_str(reg.as_str()) {
                if (pb & f) != 0 {
                    return None;
                }
                pb |= f;
            } else {
                return None;
            }
        }
        Some(pb)
    }
    pub fn to_string(pb: u8, using_user_stack: bool) -> String {
        let mut count = 0;
        let mut out = String::new();
        for bit in 0u8..8 {
            let mask = 1 << bit;
            if pb & mask != 0 {
                count += 1;
                if count > 1 {
                    out.push(',');
                }
                if using_user_stack && bit == SU {
                    out.push('S');
                } else {
                    out.push_str(_STR[bit as usize]);
                }
            }
        }
        out
    }
}

/// Enumerates instructions that cause execution to jump out of the simulator.
#[allow(clippy::upper_case_acronyms)]
#[derive(PartialEq, Eq, Debug)]
pub enum Meta {
    CWAI,
    EXIT,
    SWI,
    SWI2,
    SWI3,
    SYNC,
}
impl Meta {
    pub fn from_opcode(i: u16) -> Option<Self> {
        match i {
            0x3c => Some(Meta::CWAI),
            0x1111 => Some(Meta::EXIT),
            0x3f => Some(Meta::SWI),
            0x103f => Some(Meta::SWI2),
            0x113f => Some(Meta::SWI3),
            0x13 => Some(Meta::SYNC),
            _ => None,
        }
    }
    #[allow(dead_code)]
    pub fn to_interrupt_type(&self) -> Option<InterruptType> {
        match self {
            Meta::SWI => Some(InterruptType::Swi),
            Meta::SWI2 => Some(InterruptType::Swi2),
            Meta::SWI3 => Some(InterruptType::Swi3),
            _ => None,
        }
    }
}
/// Tracks a write operation prior to commit.
pub struct WriteRecord {
    pub addr: u16,
    pub at: AccessType,
    pub val: u8u16,
}
/// Contains all the information about an instruction and the results of executing the instruction in the given context.
/// Instructions are executed virtually first, with their results recorded in Outcome object.
/// Thereafter, the results of the instruction may be committed to the simulator's registers and memory.
pub struct Outcome {
    /// the full instance info for this instruction
    pub inst: Instance,
    /// register set as a result of this instruction
    pub new_ctx: registers::Set,
    /// indicates if this is a meta-instruction (and what type)
    pub meta: Option<Meta>,
    /// all the writes that result from this instruction
    pub writes: Option<Vec<WriteRecord>>,
    /// helpful debug info string (address and 16 bit value at address)
    pub dbgstr: Option<String>,
}
impl Outcome {
    pub fn new(inst: Instance, new_ctx: registers::Set) -> Outcome {
        Outcome {
            inst,
            new_ctx,
            meta: None,
            writes: None,
            dbgstr: None,
        }
    }

    pub fn write(&mut self, addr: u16, at: AccessType, val: u8u16) {
        if self.writes.is_none() {
            self.writes = Some(Vec::new());
        }
        self.writes.as_mut().unwrap().push(WriteRecord { addr, at, val });
    }
}
pub fn is_high_byte_of_16bit_instruction(op: u8) -> bool { op == 0x10 || op == 0x11 }
/// Information about a specific instance of an instruction in the context of a running program.
/// This includes the current values of all the registers and the calculated effective address
/// along with all the specifics of the instruction itself (Flavor, etc.)
///
/// One of the more wasteful things I'm doing is copying a whole register set at least twice
/// for each instruction executed: Once when the Instance object is created and again
/// when the Outcome object is committed.
pub struct Instance {
    /// Context before this instruction executes (ctx.pc points to this instruction)
    pub ctx: registers::Set,
    /// the Flavor of this instruction
    pub flavor: &'static Flavor,
    /// number of bytes taken up by opcode
    pub opsize: u16,
    /// Full size in bytes of this instruction instance
    pub size: u16,
    /// Copy of full instruction bytes (only <size> bytes are valid)
    pub buf: [u8; 8],
    /// The effective address referenced by the instruction
    pub ea: u16,
    /// The human readable operand
    pub operand: Option<String>,
}
const BAD_FLAVOR: &Flavor = &Flavor {
    desc: &DESCRIPTORS[0],
    mode: AddressingMode::Error,
    detail: MODE_DETAIL_ERROR,
};

impl Instance {
    pub fn new(context: &registers::Set, flavor: Option<&'static Flavor>) -> Instance {
        Instance {
            ctx: *context,
            flavor: flavor.unwrap_or(BAD_FLAVOR),
            opsize: 0,
            size: 0,
            buf: [0; 8],
            ea: 0,
            operand: None,
        }
    }
}
/// Properties of an instruction that vary depending on addressing mode.
#[derive(Debug)]
pub struct ModeDetail {
    /// op code
    pub op: u16,
    /// min clock cycle cost
    pub clk: u8,
    /// min total size of instruction
    pub sz: u16,
    /// AddressingMode in number form
    pub am: usize,
}
impl ModeDetail {
    pub fn addressing_mode(&self) -> AddressingMode { AddressingMode::from(self.am) }
    pub fn op_size(&self) -> u16 { if (self.op & 0xff00) != 0 { 2 } else { 1 } }
    pub fn op_as_u8u16(&self) -> u8u16 {
        match self.op_size() {
            2 => u8u16::u16(self.op),
            1 => u8u16::u8(self.op as u8),
            _ => panic!("invalid op_size"),
        }
    }
}
type M = ModeDetail;
pub const MODE_DETAIL_ERROR: &ModeDetail = &M {
    op: 0x0000,
    clk: 0,
    sz: 0,
    am: 0,
};

/// 6809 instructions are executed by the simulator via evaluation functions that have this signature.
type EvalFn = fn(&Core, &mut Outcome) -> Result<(), Error>;

/// Enumerates the general operand type associated with an instruction.
#[derive(Debug, PartialEq, Eq)]
pub enum OperandType {
    /// no operand (same as OT::Mode and AddressingMode::Inherent)
    None,
    /// operand type determined by addressing mode
    Mode,
    /// instruction uses push/pull post-byte
    Push,
    /// instruction uses transfer/exchange post-byte
    Exch,
}
pub type OT = OperandType;
/// Information about all the instruction variations that share a common name.
/// Each variation has an associated ModeDetail. The variations are called flavors.
pub struct Descriptor {
    /// the instruction name
    pub name: &'static str,
    /// the register this instruction is focused on (e.g. for LDA this is A)
    pub reg: registers::Name,
    /// the evaluation function for this instruction
    pub eval: EvalFn,
    /// the operand type expected for this instruction
    pub ot: OT,
    /// details for all the addressing modes supported for this instruction
    pub md: &'static [ModeDetail],
    /// the post-byte type expected for this instruction
    pub pbt: PBT,
}
// Can't use default impl of Debug because it doesn't know what to do with EvalFn.
// Also can't seem to implement Debug for EvalFn.
impl Debug for Descriptor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Descriptor")
            .field("name", &self.name)
            .field("reg", &self.reg)
            .field("ot", &self.ot)
            .field("md", &self.md)
            .field("pbt", &self.pbt)
            .finish()
    }
}
impl Descriptor {
    pub fn get_mode_detail(&self, am: AddressingMode) -> Option<&'static ModeDetail> {
        self.md.iter().find(|&m| m.addressing_mode() == am)
    }
    // return true if this instruction uses only inherent addressing mode
    pub fn is_inherent(&self) -> bool { self.md.len() == 1 && self.md[0].addressing_mode() == AddressingMode::Inherent }
}
/// Represents a fully specified instruction -- one that maps to a specific op code.
/// It combines a Descriptor with a specific ModeDetail
#[derive(Clone, Copy, Debug)]
pub struct Flavor {
    /// the Descriptor for this instruction
    pub desc: &'static Descriptor,
    /// the addressing mode for this instruction
    pub mode: AddressingMode,
    /// the ModeDetail for this instruction
    pub detail: &'static ModeDetail,
}

impl std::fmt::Display for Flavor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{:width$}",
            format!(
                "{:>4X} {}, {:?}, ({})",
                self.detail.op, self.desc.name, self.mode, self.detail.sz
            ),
            width = f.width().unwrap_or(0)
        )
    }
}

//
// instruction implementations and helpers
//
fn __nop(_: &Core, _: &mut Outcome) -> Result<(), Error> {
    // do nothing
    Ok(())
}
fn __psh_one(_: &Core, o: &mut Outcome, stack: registers::Name, reg: registers::Name) -> Result<(), Error> {
    let mut addr = o.new_ctx.get_register(stack).u16();
    if addr < registers::reg_size(reg) {
        return Err(runtime_err!(
            Some(o.inst.ctx),
            "{} stack overflow",
            if stack == registers::Name::U { "user" } else { "system" }
        ));
    }
    addr -= registers::reg_size(reg);
    let at = if stack == registers::Name::U {
        AccessType::UserStack
    } else {
        AccessType::SystemStack
    };
    o.write(addr, at, o.new_ctx.get_register(reg));
    o.new_ctx.set_register(stack, u8u16::u16(addr));
    Ok(())
}
fn __pul_one(c: &Core, o: &mut Outcome, stack: registers::Name, reg: registers::Name) -> Result<(), Error> {
    let addr = o.new_ctx.get_register(stack).u16();
    let size = registers::reg_size(reg);
    if (addr as usize + size as usize) > (1 + c.ram_top as usize) {
        return Err(runtime_err!(
            Some(o.inst.ctx),
            "{} stack underflow",
            if stack == registers::Name::U { "user" } else { "system" }
        ));
    }
    let at = if stack == registers::Name::U {
        AccessType::UserStack
    } else {
        AccessType::SystemStack
    };
    let val = c._read_u8u16(at, addr, size)?;
    o.new_ctx.set_register(reg, val);
    let (new_addr, _) = addr.overflowing_add(size);
    o.new_ctx.set_register(stack, u8u16::u16(new_addr));
    Ok(())
}
fn __psh(c: &Core, o: &mut Outcome) -> Result<(), Error> {
    let pb = c._read_u8(AccessType::Program, o.inst.ea, None)?;
    if pb & PPPostByte::PC != 0 {
        __psh_one(c, o, o.inst.flavor.desc.reg, registers::Name::PC)?;
    }
    if pb & PPPostByte::SU != 0 {
        let reg = if o.inst.flavor.desc.reg == registers::Name::S {
            registers::Name::U
        } else {
            registers::Name::S
        };
        __psh_one(c, o, o.inst.flavor.desc.reg, reg)?;
    }
    if pb & PPPostByte::Y != 0 {
        __psh_one(c, o, o.inst.flavor.desc.reg, registers::Name::Y)?;
    }
    if pb & PPPostByte::X != 0 {
        __psh_one(c, o, o.inst.flavor.desc.reg, registers::Name::X)?;
    }
    if pb & PPPostByte::DP != 0 {
        __psh_one(c, o, o.inst.flavor.desc.reg, registers::Name::DP)?;
    }
    if pb & PPPostByte::B != 0 {
        __psh_one(c, o, o.inst.flavor.desc.reg, registers::Name::B)?;
    }
    if pb & PPPostByte::A != 0 {
        __psh_one(c, o, o.inst.flavor.desc.reg, registers::Name::A)?;
    }
    if pb & PPPostByte::CC != 0 {
        __psh_one(c, o, o.inst.flavor.desc.reg, registers::Name::CC)?;
    }
    Ok(())
}
fn __pul(c: &Core, o: &mut Outcome) -> Result<(), Error> {
    let pb = c._read_u8(AccessType::Program, o.inst.ea, None)?;
    if pb & PPPostByte::CC != 0 {
        __pul_one(c, o, o.inst.flavor.desc.reg, registers::Name::CC)?;
    }
    if pb & PPPostByte::A != 0 {
        __pul_one(c, o, o.inst.flavor.desc.reg, registers::Name::A)?;
    }
    if pb & PPPostByte::B != 0 {
        __pul_one(c, o, o.inst.flavor.desc.reg, registers::Name::B)?;
    }
    if pb & PPPostByte::DP != 0 {
        __pul_one(c, o, o.inst.flavor.desc.reg, registers::Name::DP)?;
    }
    if pb & PPPostByte::X != 0 {
        __pul_one(c, o, o.inst.flavor.desc.reg, registers::Name::X)?;
    }
    if pb & PPPostByte::Y != 0 {
        __pul_one(c, o, o.inst.flavor.desc.reg, registers::Name::Y)?;
    }
    if pb & PPPostByte::SU != 0 {
        let reg = if o.inst.flavor.desc.reg == registers::Name::S {
            registers::Name::U
        } else {
            registers::Name::S
        };
        __pul_one(c, o, o.inst.flavor.desc.reg, reg)?;
    }
    if pb & PPPostByte::PC != 0 {
        __pul_one(c, o, o.inst.flavor.desc.reg, registers::Name::PC)?;
    }
    Ok(())
}

fn __rti(c: &Core, o: &mut Outcome) -> Result<(), Error> {
    __pul_one(c, o, registers::Name::S, registers::Name::CC)?;
    // if E flag was set in the saved CC then restore all registers
    // otherwise, only restore CC and PC (and the E flag was reset by restoring CC)
    if o.new_ctx.cc.is_set(registers::CCBit::E) {
        __pul_one(c, o, registers::Name::S, registers::Name::A)?;
        __pul_one(c, o, registers::Name::S, registers::Name::B)?;
        __pul_one(c, o, registers::Name::S, registers::Name::DP)?;
        __pul_one(c, o, registers::Name::S, registers::Name::X)?;
        __pul_one(c, o, registers::Name::S, registers::Name::Y)?;
        __pul_one(c, o, registers::Name::S, registers::Name::U)?;
    }
    __pul_one(c, o, registers::Name::S, registers::Name::PC)?;
    Ok(())
}
fn __tfr(c: &Core, o: &mut Outcome) -> Result<(), Error> {
    let pb = c._read_u8(AccessType::Program, o.inst.ea, None)?;
    if let Some((r1, r2)) = TEPostByte::to_registers(pb) {
        // Note: CCR unaffected unless CC is the destination register
        o.new_ctx.set_register(r2, o.new_ctx.get_register(r1));
        Ok(())
    } else {
        Err(syntax_err_ctx!(
            Some(o.new_ctx),
            "invalid registers for TFR instruction"
        ))
    }
}
fn __exg(c: &Core, o: &mut Outcome) -> Result<(), Error> {
    let pb = c._read_u8(AccessType::Program, o.inst.ea, None)?;
    if let Some((r1, r2)) = TEPostByte::to_registers(pb) {
        // Note: CCR unaffected unless CC is one of the registers exchanged
        let r2_val = o.new_ctx.get_register(r2);
        o.new_ctx.set_register(r2, o.new_ctx.get_register(r1));
        o.new_ctx.set_register(r1, r2_val);
        Ok(())
    } else {
        Err(syntax_err_ctx!(
            Some(o.new_ctx),
            "invalid registers for EXG instruction"
        ))
    }
}
fn __add(c: &Core, o: &mut Outcome) -> Result<(), Error> { __add_carry(c, o, false) }
fn __adc(c: &Core, o: &mut Outcome) -> Result<(), Error> { __add_carry(c, o, true) }
fn __add_carry(c: &Core, o: &mut Outcome, carry: bool) -> Result<(), Error> {
    let reg = o.inst.flavor.desc.reg;
    let data = c._read_u8u16(AccessType::Generic, o.inst.ea, registers::reg_size(reg))?;
    let reg_val = o.new_ctx.get_register(reg);
    let new_val = match reg {
        registers::Name::A | registers::Name::B => u8u16::u8(o.new_ctx.cc.add_u8(reg_val.u8(), data.u8(), carry)),
        registers::Name::D => u8u16::u16(o.new_ctx.cc.add_u16(reg_val.u16(), data.u16())),
        _ => panic!("invalid register for add instruction"),
    };
    o.new_ctx.set_register(reg, new_val);
    Ok(())
}
fn __sub(c: &Core, o: &mut Outcome) -> Result<(), Error> {
    let reg = o.inst.flavor.desc.reg;
    let data = c._read_u8u16(AccessType::Generic, o.inst.ea, registers::reg_size(reg))?;
    let reg_val = o.new_ctx.get_register(reg);
    let new_val = match reg {
        registers::Name::A | registers::Name::B => u8u16::u8(o.new_ctx.cc.sub_u8(reg_val.u8(), data.u8(), false)),
        registers::Name::D => u8u16::u16(o.new_ctx.cc.sub_u16(reg_val.u16(), data.u16())),
        _ => panic!("invalid register for sub instruction"),
    };
    o.new_ctx.set_register(reg, new_val);
    Ok(())
}
fn __sbc(c: &Core, o: &mut Outcome) -> Result<(), Error> {
    let reg = o.inst.flavor.desc.reg;
    let data = c._read_u8(AccessType::Generic, o.inst.ea, None)?;
    let reg_val = o.new_ctx.get_register(reg);
    let new_val = match reg {
        registers::Name::A | registers::Name::B => u8u16::u8(o.new_ctx.cc.sub_u8(reg_val.u8(), data, true)),
        _ => panic!("invalid register for sub instruction"),
    };
    o.new_ctx.set_register(reg, new_val);
    Ok(())
}
fn __neg(c: &Core, o: &mut Outcome) -> Result<(), Error> {
    let reg = o.inst.flavor.desc.reg;
    if reg == registers::Name::Z {
        let val = c._read_u8(AccessType::Generic, o.inst.ea, None)?;
        let new_val = o.new_ctx.cc.neg_u8(val);
        o.write(o.inst.ea, AccessType::Generic, u8u16::u8(new_val));
    } else {
        assert!(reg == registers::Name::A || reg == registers::Name::B);
        let val = o.new_ctx.get_register(reg).u8();
        let new_val = o.new_ctx.cc.neg_u8(val);
        o.new_ctx.set_register(reg, u8u16::u8(new_val));
    }
    Ok(())
}
fn __daa(_: &Core, o: &mut Outcome) -> Result<(), Error> {
    let mut lsd = o.new_ctx.a & 0x0f;
    let mut msd = o.new_ctx.a & 0xf0;
    let mut oa = (0u8, false);
    if lsd > 9 || o.new_ctx.cc.is_set(registers::CCBit::H) {
        lsd += 6;
        if lsd > 9 {
            lsd &= 0x0f;
            oa = msd.overflowing_add(0x10);
            msd = oa.0;
        }
    }
    if msd > 0x90 || oa.1 || o.new_ctx.cc.is_set(registers::CCBit::C) {
        oa = msd.overflowing_add(0x60);
        msd = oa.0;
    }
    // always use reg_from_ methods to alter a, b and d
    o.new_ctx.set_register(registers::Name::A, u8u16::u8(msd | lsd));
    o.new_ctx.cc.set(registers::CCBit::C, oa.1);
    o.new_ctx.cc.set(registers::CCBit::Z, o.new_ctx.a == 0);
    o.new_ctx.cc.set(registers::CCBit::N, o.new_ctx.a & 0b10000000 != 0);
    o.new_ctx.cc.set(registers::CCBit::V, false);
    // the H flag is not effected by this operation
    Ok(())
}
fn __and(c: &Core, o: &mut Outcome) -> Result<(), Error> {
    let reg = o.inst.flavor.desc.reg;
    assert!(registers::reg_size(reg) == 1);
    let data = c._read_u8(AccessType::Generic, o.inst.ea, None)?;
    if reg == registers::Name::CC {
        o.new_ctx.cc.reg &= data;
    } else {
        let reg_val = o.new_ctx.get_register(reg);
        let new_val = o.new_ctx.cc.and_u8(reg_val.u8(), data);
        o.new_ctx.set_register(reg, u8u16::u8(new_val));
    }
    Ok(())
}
fn __bit(c: &Core, o: &mut Outcome) -> Result<(), Error> {
    let reg = o.inst.flavor.desc.reg;
    assert!(reg == registers::Name::A || reg == registers::Name::B);
    let data = c._read_u8(AccessType::Generic, o.inst.ea, None)?;
    let reg_val = o.new_ctx.get_register(reg);
    o.new_ctx.cc.and_u8(reg_val.u8(), data);
    Ok(())
}
fn __or(c: &Core, o: &mut Outcome) -> Result<(), Error> {
    let reg = o.inst.flavor.desc.reg;
    assert!(registers::reg_size(reg) == 1);
    let data = c._read_u8(AccessType::Generic, o.inst.ea, None)?;
    if reg == registers::Name::CC {
        o.new_ctx.cc.reg |= data;
    } else {
        let reg_val = o.new_ctx.get_register(reg);
        let new_val = o.new_ctx.cc.or_u8(reg_val.u8(), data);
        o.new_ctx.set_register(reg, u8u16::u8(new_val));
    }
    Ok(())
}
fn __xor(c: &Core, o: &mut Outcome) -> Result<(), Error> {
    let reg = o.inst.flavor.desc.reg;
    assert!(reg == registers::Name::A || reg == registers::Name::B);
    let data = c._read_u8(AccessType::Generic, o.inst.ea, None)?;
    let reg_val = o.new_ctx.get_register(reg);
    let new_val = o.new_ctx.cc.xor_u8(reg_val.u8(), data);
    o.new_ctx.set_register(reg, u8u16::u8(new_val));
    Ok(())
}
fn __bra(_: &Core, o: &mut Outcome) -> Result<(), Error> {
    o.new_ctx.set_register(registers::Name::PC, u8u16::u16(o.inst.ea));
    Ok(())
}
fn __bsr(c: &Core, o: &mut Outcome) -> Result<(), Error> {
    __psh_one(c, o, registers::Name::S, registers::Name::PC)?;
    __bra(c, o)
}
fn __bcc(c: &Core, o: &mut Outcome) -> Result<(), Error> {
    if !o.new_ctx.cc.is_set(registers::CCBit::C) {
        __bra(c, o)?;
    }
    Ok(())
}
fn __bcs(c: &Core, o: &mut Outcome) -> Result<(), Error> {
    if o.new_ctx.cc.is_set(registers::CCBit::C) {
        __bra(c, o)?;
    }
    Ok(())
}
fn __bhs(c: &Core, o: &mut Outcome) -> Result<(), Error> { __bcc(c, o) }
fn __blo(c: &Core, o: &mut Outcome) -> Result<(), Error> { __bcs(c, o) }
fn __bvc(c: &Core, o: &mut Outcome) -> Result<(), Error> {
    if !o.new_ctx.cc.is_set(registers::CCBit::V) {
        __bra(c, o)?;
    }
    Ok(())
}
fn __bvs(c: &Core, o: &mut Outcome) -> Result<(), Error> {
    if o.new_ctx.cc.is_set(registers::CCBit::V) {
        __bra(c, o)?;
    }
    Ok(())
}
fn __bhi(c: &Core, o: &mut Outcome) -> Result<(), Error> {
    if !o.new_ctx.cc.is_set(registers::CCBit::Z) && !o.new_ctx.cc.is_set(registers::CCBit::C) {
        __bra(c, o)?;
    }
    Ok(())
}
fn __beq(c: &Core, o: &mut Outcome) -> Result<(), Error> {
    if o.new_ctx.cc.is_set(registers::CCBit::Z) {
        __bra(c, o)?;
    }
    Ok(())
}
fn __bmi(c: &Core, o: &mut Outcome) -> Result<(), Error> {
    if o.new_ctx.cc.is_set(registers::CCBit::N) {
        __bra(c, o)?;
    }
    Ok(())
}
fn __bne(c: &Core, o: &mut Outcome) -> Result<(), Error> {
    if !o.new_ctx.cc.is_set(registers::CCBit::Z) {
        __bra(c, o)?;
    }
    Ok(())
}
fn __bge(c: &Core, o: &mut Outcome) -> Result<(), Error> {
    if !xor!(
        o.new_ctx.cc.is_set(registers::CCBit::V),
        o.new_ctx.cc.is_set(registers::CCBit::N)
    ) {
        __bra(c, o)?;
    }
    Ok(())
}
fn __bgt(c: &Core, o: &mut Outcome) -> Result<(), Error> {
    if !o.new_ctx.cc.is_set(registers::CCBit::Z) {
        __bge(c, o)?;
    }
    Ok(())
}
fn __blt(c: &Core, o: &mut Outcome) -> Result<(), Error> {
    if xor!(
        o.new_ctx.cc.is_set(registers::CCBit::N),
        o.new_ctx.cc.is_set(registers::CCBit::V)
    ) {
        __bra(c, o)?;
    }
    Ok(())
}
fn __ble(c: &Core, o: &mut Outcome) -> Result<(), Error> {
    if o.new_ctx.cc.is_set(registers::CCBit::Z)
        || xor!(
            o.new_ctx.cc.is_set(registers::CCBit::N),
            o.new_ctx.cc.is_set(registers::CCBit::V)
        )
    {
        __bra(c, o)?;
    }
    Ok(())
}
fn __bls(c: &Core, o: &mut Outcome) -> Result<(), Error> {
    if o.new_ctx.cc.is_set(registers::CCBit::C) || o.new_ctx.cc.is_set(registers::CCBit::Z) {
        __bra(c, o)?;
    }
    Ok(())
}
fn __bpl(c: &Core, o: &mut Outcome) -> Result<(), Error> {
    if !o.new_ctx.cc.is_set(registers::CCBit::N) {
        __bra(c, o)?;
    }
    Ok(())
}
fn __abx(_: &Core, o: &mut Outcome) -> Result<(), Error> {
    let x = o.new_ctx.get_register(registers::Name::X).u16();
    let b = o.new_ctx.get_register(registers::Name::B).u16();
    let (newx, _) = x.overflowing_add(b);
    // Note: ABX has no effect on flags
    o.new_ctx.set_register(registers::Name::X, u8u16::u16(newx));
    Ok(())
}
fn __clr(_: &Core, o: &mut Outcome) -> Result<(), Error> {
    let reg = o.inst.flavor.desc.reg;
    if reg == registers::Name::Z {
        o.write(o.inst.ea, AccessType::Generic, u8u16::u8(0));
    } else {
        assert!(reg == registers::Name::A || reg == registers::Name::B);
        o.new_ctx.set_register(reg, u8u16::u8(0));
    }
    o.new_ctx.cc.set(registers::CCBit::Z, true);
    o.new_ctx.cc.set(registers::CCBit::V, false);
    o.new_ctx.cc.set(registers::CCBit::N, false);
    o.new_ctx.cc.set(registers::CCBit::C, false);
    Ok(())
}
fn __inc(c: &Core, o: &mut Outcome) -> Result<(), Error> {
    let reg = o.inst.flavor.desc.reg;
    if reg == registers::Name::Z {
        let data = c._read_u8(AccessType::Generic, o.inst.ea, None)?;
        let new_val = o.new_ctx.cc.inc_u8(data);
        o.write(o.inst.ea, AccessType::Generic, u8u16::u8(new_val));
    } else {
        assert!(reg == registers::Name::A || reg == registers::Name::B);
        let data = o.new_ctx.get_register(reg).u8();
        let new_val = o.new_ctx.cc.inc_u8(data);
        o.new_ctx.set_register(reg, u8u16::u8(new_val));
    }
    Ok(())
}
fn __dec(c: &Core, o: &mut Outcome) -> Result<(), Error> {
    let reg = o.inst.flavor.desc.reg;
    if reg == registers::Name::Z {
        let data = c._read_u8(AccessType::Generic, o.inst.ea, None)?;
        let new_val = o.new_ctx.cc.dec_u8(data);
        o.write(o.inst.ea, AccessType::Generic, u8u16::u8(new_val));
    } else {
        assert!(reg == registers::Name::A || reg == registers::Name::B);
        let data = o.new_ctx.get_register(reg).u8();
        let new_val = o.new_ctx.cc.dec_u8(data);
        o.new_ctx.set_register(reg, u8u16::u8(new_val));
    }
    Ok(())
}
fn __ld(c: &Core, o: &mut Outcome) -> Result<(), Error> {
    let val = c._read_u8u16(
        AccessType::Generic,
        o.inst.ea,
        registers::reg_size(o.inst.flavor.desc.reg),
    )?;
    o.new_ctx.set_register(o.inst.flavor.desc.reg, val);
    // set cc registers
    if registers::reg_size(o.inst.flavor.desc.reg) == 1 {
        o.new_ctx.cc.and_u8(val.u8(), 0xff);
    } else {
        o.new_ctx.cc.and_u16(val.u16(), 0xffff);
    }
    Ok(())
}
fn __cmp(c: &Core, o: &mut Outcome) -> Result<(), Error> {
    let reg_val = o.new_ctx.get_register(o.inst.flavor.desc.reg);
    let val = c._read_u8u16(
        AccessType::Generic,
        o.inst.ea,
        registers::reg_size(o.inst.flavor.desc.reg),
    )?;
    match reg_val {
        u8u16::u16(w) => o.new_ctx.cc.cmp_u16(w, val.u16()),
        u8u16::u8(b) => o.new_ctx.cc.cmp_u8(b, val.u8()),
    }
    Ok(())
}

fn __lea(_: &Core, o: &mut Outcome) -> Result<(), Error> {
    o.new_ctx.set_register(o.inst.flavor.desc.reg, u8u16::u16(o.inst.ea));
    if o.inst.flavor.desc.reg == registers::Name::X || o.inst.flavor.desc.reg == registers::Name::Y {
        o.new_ctx.cc.set(registers::CCBit::Z, o.inst.ea == 0);
    }
    Ok(())
}
fn __st(_: &Core, o: &mut Outcome) -> Result<(), Error> {
    let val = o.inst.ctx.get_register(o.inst.flavor.desc.reg);
    o.write(o.inst.ea, AccessType::Generic, val);
    // set cc flags!
    match val {
        u8u16::u16(w) => {
            o.new_ctx.cc.and_u16(w, 0xffff);
        }
        u8u16::u8(b) => {
            o.new_ctx.cc.and_u8(b, 0xff);
        }
    }
    Ok(())
}
fn __jmp(_: &Core, o: &mut Outcome) -> Result<(), Error> {
    o.new_ctx.set_register(registers::Name::PC, u8u16::u16(o.inst.ea));
    Ok(())
}
fn __jsr(c: &Core, o: &mut Outcome) -> Result<(), Error> {
    __psh_one(c, o, registers::Name::S, registers::Name::PC)?;
    __jmp(c, o)
}
fn __rts(c: &Core, o: &mut Outcome) -> Result<(), Error> { __pul_one(c, o, registers::Name::S, registers::Name::PC) }
fn __tst(c: &Core, o: &mut Outcome) -> Result<(), Error> {
    let acc = match o.inst.flavor.desc.reg {
        registers::Name::A | registers::Name::B => o.new_ctx.get_register(o.inst.flavor.desc.reg).u8(),
        registers::Name::Z => c._read_u8(AccessType::Generic, o.inst.ea, None)?,
        _ => panic!("invalid register for TST instruction?!"),
    };
    o.new_ctx.cc.or_u8(acc, 0);
    Ok(())
}
fn __sex(_: &Core, o: &mut Outcome) -> Result<(), Error> {
    let b = o.new_ctx.get_register(registers::Name::B).u8();
    let a = u8u16::new(if b & 0x80 == 0 { 0 } else { 0xff }, None);
    // sign extend accb into acca (really sign extend accb into accd)
    o.new_ctx.set_register(registers::Name::A, a);
    // set cc's n & z bits based on accb
    o.new_ctx.cc.or_u8(b, 0);
    Ok(())
}
fn __asl(c: &Core, o: &mut Outcome) -> Result<(), Error> {
    let reg = o.inst.flavor.desc.reg;
    if reg == registers::Name::Z {
        let data = c._read_u8(AccessType::Generic, o.inst.ea, None)?;
        let new_val = o.new_ctx.cc.shl_u8(data);
        o.write(o.inst.ea, AccessType::Generic, u8u16::u8(new_val));
    } else {
        assert!(reg == registers::Name::A || reg == registers::Name::B);
        let data = o.new_ctx.get_register(reg).u8();
        let new_val = o.new_ctx.cc.shl_u8(data);
        o.new_ctx.set_register(reg, u8u16::u8(new_val));
    }
    Ok(())
}
fn __asr(c: &Core, o: &mut Outcome) -> Result<(), Error> { __shr(c, o, true) }
fn __lsr(c: &Core, o: &mut Outcome) -> Result<(), Error> { __shr(c, o, false) }
fn __shr(c: &Core, o: &mut Outcome, preserve_sign: bool) -> Result<(), Error> {
    let reg = o.inst.flavor.desc.reg;
    if reg == registers::Name::Z {
        let data = c._read_u8(AccessType::Generic, o.inst.ea, None)?;
        let new_val = o.new_ctx.cc.shr_u8(data, preserve_sign);
        o.write(o.inst.ea, AccessType::Generic, u8u16::u8(new_val));
    } else {
        assert!(reg == registers::Name::A || reg == registers::Name::B);
        let data = o.new_ctx.get_register(reg).u8();
        let new_val = o.new_ctx.cc.shr_u8(data, preserve_sign);
        o.new_ctx.set_register(reg, u8u16::u8(new_val));
    }
    Ok(())
}
fn __rol(c: &Core, o: &mut Outcome) -> Result<(), Error> {
    let reg = o.inst.flavor.desc.reg;
    if reg == registers::Name::Z {
        let data = c._read_u8(AccessType::Generic, o.inst.ea, None)?;
        let new_val = o.new_ctx.cc.rol_u8(data);
        o.write(o.inst.ea, AccessType::Generic, u8u16::u8(new_val));
    } else {
        assert!(reg == registers::Name::A || reg == registers::Name::B);
        let data = o.new_ctx.get_register(reg).u8();
        let new_val = o.new_ctx.cc.rol_u8(data);
        o.new_ctx.set_register(reg, u8u16::u8(new_val));
    }
    Ok(())
}
fn __ror(c: &Core, o: &mut Outcome) -> Result<(), Error> {
    let reg = o.inst.flavor.desc.reg;
    if reg == registers::Name::Z {
        let data = c._read_u8(AccessType::Generic, o.inst.ea, None)?;
        let new_val = o.new_ctx.cc.ror_u8(data);
        o.write(o.inst.ea, AccessType::Generic, u8u16::u8(new_val));
    } else {
        assert!(reg == registers::Name::A || reg == registers::Name::B);
        let data = o.new_ctx.get_register(reg).u8();
        let new_val = o.new_ctx.cc.ror_u8(data);
        o.new_ctx.set_register(reg, u8u16::u8(new_val));
    }
    Ok(())
}
fn __com(c: &Core, o: &mut Outcome) -> Result<(), Error> {
    let reg = o.inst.flavor.desc.reg;
    if reg == registers::Name::Z {
        let data = c._read_u8(AccessType::Generic, o.inst.ea, None)?;
        let new_val = o.new_ctx.cc.com_u8(data);
        o.write(o.inst.ea, AccessType::Generic, u8u16::u8(new_val));
    } else {
        assert!(reg == registers::Name::A || reg == registers::Name::B);
        let data = o.new_ctx.get_register(reg).u8();
        let new_val = o.new_ctx.cc.com_u8(data);
        o.new_ctx.set_register(reg, u8u16::u8(new_val));
    }
    Ok(())
}
fn __mul(_: &Core, o: &mut Outcome) -> Result<(), Error> {
    let a = o.new_ctx.get_register(registers::Name::A);
    let b = o.new_ctx.get_register(registers::Name::B);
    let d = o.new_ctx.cc.mul(a.u8(), b.u8());
    o.new_ctx.set_register(registers::Name::D, u8u16::u16(d));
    Ok(())
}

fn __err(_: &Core, o: &mut Outcome) -> Result<(), Error> {
    panic!("No implementation for instruction {}!", o.inst.flavor.desc.name);
}

fn __meta(c: &Core, o: &mut Outcome) -> Result<(), Error> {
    o.meta = Meta::from_opcode(o.inst.flavor.detail.op);
    if o.meta == Some(Meta::CWAI) {
        // CWAI performs an andcc (immediate)
        let data = c._read_u8(AccessType::Generic, o.inst.ea, None)?;
        o.new_ctx.cc.reg &= data;
    }
    Ok(())
}

use registers::Name;
//
// instruction table
//
#[rustfmt::skip]
pub const DESCRIPTORS: &[Descriptor] = &[
 Descriptor{name:"ABX", 	eval:__abx,	reg: Name::Z, pbt: PBT::NA,  ot:OT::None,md:&[M{op:0x3A,clk:1,sz:1,am:4},]},
 Descriptor{name:"ADCA",	eval:__adc,	reg: Name::A, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x89,clk:2,sz:2,am:0},M{op:0x99,clk:3,sz:2,am:1},M{op:0xA9,clk:4,sz:2,am:2},M{op:0xB9,clk:4,sz:3,am:3},]},
 Descriptor{name:"ADCB",	eval:__adc,	reg: Name::B, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0xC9,clk:2,sz:2,am:0},M{op:0xD9,clk:3,sz:2,am:1},M{op:0xE9,clk:4,sz:2,am:2},M{op:0xF9,clk:4,sz:3,am:3},]},
 Descriptor{name:"ADDA",	eval:__add,	reg: Name::A, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x8B,clk:2,sz:2,am:0},M{op:0x9B,clk:3,sz:2,am:1},M{op:0xAB,clk:4,sz:2,am:2},M{op:0xBB,clk:4,sz:3,am:3},]},
 Descriptor{name:"ADDB",	eval:__add,	reg: Name::B, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0xCB,clk:2,sz:2,am:0},M{op:0xDB,clk:3,sz:2,am:1},M{op:0xEB,clk:4,sz:2,am:2},M{op:0xFB,clk:4,sz:3,am:3},]},
 Descriptor{name:"ADDD",	eval:__add,	reg: Name::D, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0xC3,clk:3,sz:3,am:0},M{op:0xD3,clk:4,sz:2,am:1},M{op:0xE3,clk:5,sz:2,am:2},M{op:0xF3,clk:5,sz:3,am:3},]},
 Descriptor{name:"ANDA",	eval:__and,	reg: Name::A, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x84,clk:2,sz:2,am:0},M{op:0x94,clk:3,sz:2,am:1},M{op:0xA4,clk:4,sz:2,am:2},M{op:0xB4,clk:4,sz:3,am:3},]},
 Descriptor{name:"ANDB",	eval:__and,	reg: Name::B, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0xC4,clk:2,sz:2,am:0},M{op:0xD4,clk:3,sz:2,am:1},M{op:0xE4,clk:4,sz:2,am:2},M{op:0xF4,clk:4,sz:3,am:3},]},
 Descriptor{name:"ANDCC",	eval:__and, reg: Name::CC, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x1C,clk:3,sz:2,am:0},]},
 Descriptor{name:"ASL",	    eval:__asl,	reg: Name::Z, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x08,clk:5,sz:2,am:1},M{op:0x68,clk:6,sz:2,am:2},M{op:0x78,clk:6,sz:3,am:3},]},
 Descriptor{name:"ASLA",	eval:__asl,	reg: Name::A, pbt: PBT::NA,  ot:OT::None,md:&[M{op:0x48,clk:1,sz:1,am:4},]},
 Descriptor{name:"ASLB",	eval:__asl,	reg: Name::B, pbt: PBT::NA,  ot:OT::None,md:&[M{op:0x58,clk:1,sz:1,am:4},]},
 Descriptor{name:"ASR",	    eval:__asr,	reg: Name::Z, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x07,clk:5,sz:2,am:1},M{op:0x67,clk:6,sz:2,am:2},M{op:0x77,clk:6,sz:3,am:3},]},
 Descriptor{name:"ASRA",	eval:__asr,	reg: Name::A, pbt: PBT::NA,  ot:OT::None,md:&[M{op:0x47,clk:1,sz:1,am:4},]},
 Descriptor{name:"ASRB",	eval:__asr,	reg: Name::B, pbt: PBT::NA,  ot:OT::None,md:&[M{op:0x57,clk:1,sz:1,am:4},]},
 Descriptor{name:"BEQ",	    eval:__beq,	reg: Name::Z, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x27,clk:3,sz:2,am:5},]},
 Descriptor{name:"BGE",	    eval:__bge,	reg: Name::Z, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x2C,clk:3,sz:2,am:5},]},
 Descriptor{name:"BGT",	    eval:__bgt,	reg: Name::Z, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x2E,clk:3,sz:2,am:5},]},
 Descriptor{name:"BCC",	    eval:__bcc,	reg: Name::Z, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x24,clk:3,sz:2,am:5},]},
 Descriptor{name:"BHI",	    eval:__bhi,	reg: Name::Z, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x22,clk:3,sz:2,am:5},]},
 Descriptor{name:"BHS",	    eval:__bhs,	reg: Name::Z, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x24,clk:3,sz:2,am:5},]},
 Descriptor{name:"BITA",	eval:__bit,	reg: Name::A, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x85,clk:2,sz:2,am:0},M{op:0x95,clk:3,sz:2,am:1},M{op:0xA5,clk:4,sz:2,am:2},M{op:0xB5,clk:4,sz:3,am:3},]},
 Descriptor{name:"BITB",	eval:__bit,	reg: Name::B, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0xC5,clk:2,sz:2,am:0},M{op:0xD5,clk:3,sz:2,am:1},M{op:0xE5,clk:4,sz:2,am:2},M{op:0xF5,clk:4,sz:3,am:3},]},
 Descriptor{name:"BLE",	    eval:__ble,	reg: Name::Z, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x2F,clk:3,sz:2,am:5},]},
 Descriptor{name:"BCS",	    eval:__err,	reg: Name::Z, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x25,clk:3,sz:2,am:5},]},
 Descriptor{name:"BLO",	    eval:__blo,	reg: Name::Z, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x25,clk:3,sz:2,am:5},]},
 Descriptor{name:"BLS",	    eval:__bls,	reg: Name::Z, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x23,clk:3,sz:2,am:5},]},
 Descriptor{name:"BLT",	    eval:__blt,	reg: Name::Z, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x2D,clk:3,sz:2,am:5},]},
 Descriptor{name:"BMI",	    eval:__bmi,	reg: Name::Z, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x2B,clk:3,sz:2,am:5},]},
 Descriptor{name:"BNE",	    eval:__bne,	reg: Name::Z, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x26,clk:3,sz:2,am:5},]},
 Descriptor{name:"BPL",	    eval:__bpl,	reg: Name::Z, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x2A,clk:3,sz:2,am:5},]},
 Descriptor{name:"BRA",	    eval:__bra,	reg: Name::Z, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x20,clk:3,sz:2,am:5},]},
 Descriptor{name:"BRN",	    eval:__nop,	reg: Name::Z, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x21,clk:3,sz:2,am:5},]},
 Descriptor{name:"BSR",	    eval:__bsr,	reg: Name::Z, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x8D,clk:6,sz:2,am:5},]},
 Descriptor{name:"BVC",	    eval:__bvc,	reg: Name::Z, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x28,clk:3,sz:2,am:5},]},
 Descriptor{name:"BVS",	    eval:__bvs,	reg: Name::Z, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x29,clk:3,sz:2,am:5},]},
 Descriptor{name:"CLR",	    eval:__clr,	reg: Name::Z, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x0F,clk:5,sz:2,am:1},M{op:0x6F,clk:6,sz:2,am:2},M{op:0x7F,clk:6,sz:3,am:3},]},
 Descriptor{name:"CLRA",	eval:__clr,	reg: Name::A, pbt: PBT::NA,  ot:OT::None,md:&[M{op:0x4F,clk:1,sz:1,am:4},]},
 Descriptor{name:"CLRB",	eval:__clr,	reg: Name::B, pbt: PBT::NA,  ot:OT::None,md:&[M{op:0x5F,clk:1,sz:1,am:4},]},
 Descriptor{name:"CMPA",	eval:__cmp,	reg: Name::A, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x81,clk:2,sz:2,am:0},M{op:0x91,clk:3,sz:2,am:1},M{op:0xA1,clk:4,sz:2,am:2},M{op:0xB1,clk:4,sz:3,am:3},]},
 Descriptor{name:"CMPB",	eval:__cmp,	reg: Name::B, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0xC1,clk:2,sz:2,am:0},M{op:0xD1,clk:3,sz:2,am:1},M{op:0xE1,clk:4,sz:2,am:2},M{op:0xF1,clk:4,sz:3,am:3},]},
 Descriptor{name:"CMPD",	eval:__cmp,	reg: Name::D, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x1083,clk:4,sz:4,am:0},M{op:0x1093,clk:5,sz:3,am:1},M{op:0x10A3,clk:6,sz:3,am:2},M{op:0x10B3,clk:6,sz:4,am:3},]},
 Descriptor{name:"CMPS",	eval:__cmp,	reg: Name::S, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x118C,clk:4,sz:4,am:0},M{op:0x119C,clk:5,sz:3,am:1},M{op:0x11AC,clk:7,sz:3,am:2},M{op:0x11BC,clk:6,sz:4,am:3},]},
 Descriptor{name:"CMPU",	eval:__cmp,	reg: Name::U, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x1183,clk:4,sz:4,am:0},M{op:0x1193,clk:5,sz:3,am:1},M{op:0x11A3,clk:6,sz:3,am:2},M{op:0x11B3,clk:6,sz:4,am:3},]},
 Descriptor{name:"CMPX",	eval:__cmp,	reg: Name::X, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x8C,clk:3,sz:3,am:0},M{op:0x9C,clk:4,sz:2,am:1},M{op:0xAC,clk:5,sz:2,am:2},M{op:0xBC,clk:5,sz:3,am:3},]},
 Descriptor{name:"CMPY",	eval:__cmp,	reg: Name::Y, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x108C,clk:4,sz:4,am:0},M{op:0x109C,clk:5,sz:3,am:1},M{op:0x10AC,clk:6,sz:3,am:2},M{op:0x10BC,clk:6,sz:4,am:3},]},
 Descriptor{name:"COM",	    eval:__com,	reg: Name::Z, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x03,clk:5,sz:2,am:1},M{op:0x63,clk:6,sz:2,am:2},M{op:0x73,clk:6,sz:3,am:3},]},
 Descriptor{name:"COMA",	eval:__com,	reg: Name::A, pbt: PBT::NA,  ot:OT::None,md:&[M{op:0x43,clk:1,sz:1,am:4},]},
 Descriptor{name:"COMB",	eval:__com,	reg: Name::B, pbt: PBT::NA,  ot:OT::None,md:&[M{op:0x53,clk:1,sz:1,am:4},]},
 Descriptor{name:"CWAI",	eval:__meta,reg: Name::Z, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x3C,clk:20,sz:2,am:0},]},
 Descriptor{name:"DAA",	    eval:__daa,	reg: Name::Z, pbt: PBT::NA,  ot:OT::None,md:&[M{op:0x19,clk:1,sz:1,am:4},]},
 Descriptor{name:"DEC",	    eval:__dec,	reg: Name::Z, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x0A,clk:5,sz:2,am:1},M{op:0x6A,clk:6,sz:2,am:2},M{op:0x7A,clk:6,sz:3,am:3},]},
 Descriptor{name:"DECA",	eval:__dec,	reg: Name::A, pbt: PBT::NA,  ot:OT::None,md:&[M{op:0x4A,clk:1,sz:1,am:4},]},
 Descriptor{name:"DECB",	eval:__dec,	reg: Name::B, pbt: PBT::NA,  ot:OT::None,md:&[M{op:0x5A,clk:1,sz:1,am:4},]},
 Descriptor{name:"EORA",	eval:__xor,	reg: Name::A, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x88,clk:2,sz:2,am:0},M{op:0x98,clk:3,sz:2,am:1},M{op:0xA8,clk:4,sz:2,am:2},M{op:0xB8,clk:4,sz:3,am:3},]},
 Descriptor{name:"EORB",	eval:__xor,	reg: Name::B, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0xC8,clk:2,sz:2,am:0},M{op:0xD8,clk:3,sz:2,am:1},M{op:0xE8,clk:4,sz:2,am:2},M{op:0xF8,clk:4,sz:3,am:3},]},
 Descriptor{name:"EXG",	    eval:__exg,	reg: Name::Z, pbt: PBT::TransferExchange,  ot:OT::Exch,md:&[M{op:0x1E,clk:5,sz:2,am:0},]},
 Descriptor{name:"EXIT",	eval:__meta,reg: Name::Z, pbt: PBT::NA,  ot:OT::None,md:&[M{op:0x1111,clk:99,sz:2,am:4},]},
 Descriptor{name:"INC",	    eval:__inc,	reg: Name::Z, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x0C,clk:5,sz:2,am:1},M{op:0x6C,clk:6,sz:2,am:2},M{op:0x7C,clk:6,sz:3,am:3},]},
 Descriptor{name:"INCA",	eval:__inc,	reg: Name::A, pbt: PBT::NA,  ot:OT::None,md:&[M{op:0x4C,clk:1,sz:1,am:4},]},
 Descriptor{name:"INCB",	eval:__inc,	reg: Name::B, pbt: PBT::NA,  ot:OT::None,md:&[M{op:0x5C,clk:1,sz:1,am:4},]},
 Descriptor{name:"JMP",	    eval:__jmp,	reg: Name::Z, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x0E,clk:2,sz:2,am:1},M{op:0x6E,clk:3,sz:2,am:2},M{op:0x7E,clk:3,sz:3,am:3},]},
 Descriptor{name:"JSR",	    eval:__jsr,	reg: Name::Z, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x9D,clk:6,sz:2,am:1},M{op:0xAD,clk:6,sz:2,am:2},M{op:0xBD,clk:7,sz:3,am:3},]},
 Descriptor{name:"LBCS",	eval:__bcs,	reg: Name::Z, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x1025,clk:5,sz:4,am:5},]},
 Descriptor{name:"LBLO",	eval:__bcs,	reg: Name::Z, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x1025,clk:5,sz:4,am:5},]},
 Descriptor{name:"LBEQ",	eval:__beq,	reg: Name::Z, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x1027,clk:5,sz:4,am:5},]},
 Descriptor{name:"LBGE",	eval:__bge,	reg: Name::Z, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x102C,clk:5,sz:4,am:5},]},
 Descriptor{name:"LBGT",	eval:__bgt,	reg: Name::Z, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x102E,clk:5,sz:4,am:5},]},
 Descriptor{name:"LBHI",	eval:__bhi,	reg: Name::Z, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x1022,clk:5,sz:4,am:5},]},
 Descriptor{name:"LBCC",	eval:__bhs,	reg: Name::Z, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x1024,clk:5,sz:4,am:5},]},
 Descriptor{name:"LBHS",	eval:__bhs,	reg: Name::Z, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x1024,clk:5,sz:4,am:5},]},
 Descriptor{name:"LBLE",	eval:__ble,	reg: Name::Z, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x102F,clk:5,sz:4,am:5},]},
 Descriptor{name:"LBLS",	eval:__bls,	reg: Name::Z, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x1023,clk:5,sz:4,am:5},]},
 Descriptor{name:"LBLT",	eval:__blt,	reg: Name::Z, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x102D,clk:5,sz:4,am:5},]},
 Descriptor{name:"LBMI",	eval:__bmi,	reg: Name::Z, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x102B,clk:5,sz:4,am:5},]},
 Descriptor{name:"LBNE",	eval:__bne,	reg: Name::Z, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x1026,clk:5,sz:4,am:5},]},
 Descriptor{name:"LBPL",	eval:__bpl,	reg: Name::Z, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x102A,clk:5,sz:4,am:5},]},
 Descriptor{name:"LBRA",	eval:__bra,	reg: Name::Z, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x16,clk:4,sz:3,am:5},]},
 Descriptor{name:"LBRN",	eval:__nop,	reg: Name::Z, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x1021,clk:5,sz:4,am:5},]},
 Descriptor{name:"LBSR",	eval:__bsr,	reg: Name::Z, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x17,clk:7,sz:3,am:5},]},
 Descriptor{name:"LBVC",	eval:__bvc,	reg: Name::Z, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x1028,clk:5,sz:4,am:5},]},
 Descriptor{name:"LBVS",	eval:__bvs,	reg: Name::Z, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x1029,clk:5,sz:4,am:5},]},
 Descriptor{name:"LDA",	    eval:__ld,	reg: Name::A, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x86,clk:2,sz:2,am:0},M{op:0x96,clk:3,sz:2,am:1},M{op:0xA6,clk:4,sz:2,am:2},M{op:0xB6,clk:4,sz:3,am:3},]},
 Descriptor{name:"LDB",	    eval:__ld,	reg: Name::B, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0xC6,clk:2,sz:2,am:0},M{op:0xD6,clk:3,sz:2,am:1},M{op:0xE6,clk:4,sz:2,am:2},M{op:0xF6,clk:4,sz:3,am:3},]},
 Descriptor{name:"LDD",	    eval:__ld,	reg: Name::D, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0xCC,clk:3,sz:3,am:0},M{op:0xDC,clk:4,sz:2,am:1},M{op:0xEC,clk:5,sz:2,am:2},M{op:0xFC,clk:5,sz:3,am:3},]},
 Descriptor{name:"LDS",	    eval:__ld,	reg: Name::S, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x10CE,clk:4,sz:4,am:0},M{op:0x10DE,clk:5,sz:3,am:1},M{op:0x10EE,clk:6,sz:3,am:2},M{op:0x10FE,clk:6,sz:4,am:3},]},
 Descriptor{name:"LDU",	    eval:__ld,	reg: Name::U, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0xCE,clk:3,sz:3,am:0},M{op:0xDE,clk:4,sz:2,am:1},M{op:0xEE,clk:5,sz:2,am:2},M{op:0xFE,clk:5,sz:3,am:3},]},
 Descriptor{name:"LDX",	    eval:__ld,	reg: Name::X, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x8E,clk:3,sz:3,am:0},M{op:0x9E,clk:4,sz:2,am:1},M{op:0xAE,clk:5,sz:2,am:2},M{op:0xBE,clk:5,sz:3,am:3},]},
 Descriptor{name:"LDY",	    eval:__ld,	reg: Name::Y, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x108E,clk:4,sz:4,am:0},M{op:0x109E,clk:5,sz:3,am:1},M{op:0x10AE,clk:6,sz:3,am:2},M{op:0x10BE,clk:6,sz:4,am:3},]},
 Descriptor{name:"LEAS",	eval:__lea,	reg: Name::S, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x32,clk:4,sz:2,am:2},]},
 Descriptor{name:"LEAU",	eval:__lea,	reg: Name::U, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x33,clk:4,sz:2,am:2},]},
 Descriptor{name:"LEAX",	eval:__lea,	reg: Name::X, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x30,clk:4,sz:2,am:2},]},
 Descriptor{name:"LEAY",	eval:__lea,	reg: Name::Y, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x31,clk:4,sz:2,am:2},]},
 Descriptor{name:"LSL",	    eval:__asl,	reg: Name::Z, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x08,clk:5,sz:2,am:1},M{op:0x68,clk:6,sz:2,am:2},M{op:0x78,clk:6,sz:3,am:3},]},
 Descriptor{name:"LSLA",	eval:__asl,	reg: Name::A, pbt: PBT::NA,  ot:OT::None,md:&[M{op:0x48,clk:1,sz:1,am:4},]},
 Descriptor{name:"LSLB",	eval:__asl,	reg: Name::B, pbt: PBT::NA,  ot:OT::None,md:&[M{op:0x58,clk:1,sz:1,am:4},]},
 Descriptor{name:"LSR",	    eval:__lsr,	reg: Name::Z, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x04,clk:5,sz:2,am:1},M{op:0x64,clk:6,sz:2,am:2},M{op:0x74,clk:6,sz:3,am:3},]},
 Descriptor{name:"LSRA",	eval:__lsr,	reg: Name::A, pbt: PBT::NA,  ot:OT::None,md:&[M{op:0x44,clk:1,sz:1,am:4},]},
 Descriptor{name:"LSRB",	eval:__lsr,	reg: Name::B, pbt: PBT::NA,  ot:OT::None,md:&[M{op:0x54,clk:1,sz:1,am:4},]},
 Descriptor{name:"MUL",	    eval:__mul,	reg: Name::Z, pbt: PBT::NA,  ot:OT::None,md:&[M{op:0x3D,clk:10,sz:1,am:4},]},
 Descriptor{name:"NEG",	    eval:__neg,	reg: Name::Z, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x00,clk:5,sz:2,am:1},M{op:0x60,clk:6,sz:2,am:2},M{op:0x70,clk:6,sz:3,am:3},]},
 Descriptor{name:"NEGA",	eval:__neg,	reg: Name::A, pbt: PBT::NA,  ot:OT::None,md:&[M{op:0x40,clk:1,sz:1,am:4},]},
 Descriptor{name:"NEGB",	eval:__neg,	reg: Name::B, pbt: PBT::NA,  ot:OT::None,md:&[M{op:0x50,clk:1,sz:1,am:4},]},
 Descriptor{name:"NOP",	    eval:__nop,	reg: Name::Z, pbt: PBT::NA,  ot:OT::None,md:&[M{op:0x12,clk:1,sz:1,am:4},]},
 Descriptor{name:"ORA",	    eval:__or,  reg: Name::A, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x8A,clk:2,sz:2,am:0},M{op:0x9A,clk:3,sz:2,am:1},M{op:0xAA,clk:4,sz:2,am:2},M{op:0xBA,clk:4,sz:3,am:3},]},
 Descriptor{name:"ORB",	    eval:__or,  reg: Name::B, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0xCA,clk:2,sz:2,am:0},M{op:0xDA,clk:3,sz:2,am:1},M{op:0xEA,clk:4,sz:2,am:2},M{op:0xFA,clk:4,sz:3,am:3},]},
 Descriptor{name:"ORCC",	eval:__or,  reg: Name::CC, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x1A,clk:2,sz:2,am:0},]},
 Descriptor{name:"PSHS",	eval:__psh,	reg: Name::S, pbt: PBT::PushPull,  ot:OT::Push,md:&[M{op:0x34,clk:4,sz:2,am:0},]},
 Descriptor{name:"PSHU",	eval:__psh,	reg: Name::U, pbt: PBT::PushPull,  ot:OT::Push,md:&[M{op:0x36,clk:4,sz:2,am:0},]},
 Descriptor{name:"PULS",	eval:__pul,	reg: Name::S, pbt: PBT::PushPull,  ot:OT::Push,md:&[M{op:0x35,clk:4,sz:2,am:0},]},
 Descriptor{name:"PULU",	eval:__pul,	reg: Name::U, pbt: PBT::PushPull,  ot:OT::Push,md:&[M{op:0x37,clk:4,sz:2,am:0},]},
 Descriptor{name:"ROL",	    eval:__rol,	reg: Name::Z, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x09,clk:5,sz:2,am:1},M{op:0x69,clk:6,sz:2,am:2},M{op:0x79,clk:6,sz:3,am:3},]},
 Descriptor{name:"ROLA",	eval:__rol,	reg: Name::A, pbt: PBT::NA,  ot:OT::None,md:&[M{op:0x49,clk:1,sz:1,am:4},]},
 Descriptor{name:"ROLB",	eval:__rol,	reg: Name::B, pbt: PBT::NA,  ot:OT::None,md:&[M{op:0x59,clk:1,sz:1,am:4},]},
 Descriptor{name:"ROR",	    eval:__ror,	reg: Name::Z, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x06,clk:5,sz:2,am:1},M{op:0x66,clk:6,sz:2,am:2},M{op:0x76,clk:6,sz:3,am:3},]},
 Descriptor{name:"RORA",	eval:__ror,	reg: Name::A, pbt: PBT::NA,  ot:OT::None,md:&[M{op:0x46,clk:1,sz:1,am:4},]},
 Descriptor{name:"RORB",	eval:__ror,	reg: Name::B, pbt: PBT::NA,  ot:OT::None,md:&[M{op:0x56,clk:1,sz:1,am:4},]},
 Descriptor{name:"RTI",	    eval:__rti,	reg: Name::Z, pbt: PBT::NA,  ot:OT::None,md:&[M{op:0x3B,clk:6,sz:1,am:4},]},
 Descriptor{name:"RTS",	    eval:__rts,	reg: Name::Z, pbt: PBT::NA,  ot:OT::None,md:&[M{op:0x39,clk:1,sz:1,am:4},]},
 Descriptor{name:"SBCA",	eval:__sbc,	reg: Name::A, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x82,clk:2,sz:2,am:0},M{op:0x92,clk:3,sz:2,am:1},M{op:0xA2,clk:4,sz:2,am:2},M{op:0xB2,clk:4,sz:3,am:3},]},
 Descriptor{name:"SBCB",	eval:__sbc,	reg: Name::B, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0xC2,clk:2,sz:2,am:0},M{op:0xD2,clk:3,sz:2,am:1},M{op:0xE2,clk:4,sz:2,am:2},M{op:0xF2,clk:4,sz:3,am:3},]},
 Descriptor{name:"SEX",	    eval:__sex,	reg: Name::Z, pbt: PBT::NA,  ot:OT::None,md:&[M{op:0x1D,clk:1,sz:1,am:4},]},
 Descriptor{name:"STA",	    eval:__st,	reg: Name::A, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x97,clk:3,sz:2,am:1},M{op:0xA7,clk:4,sz:2,am:2},M{op:0xB7,clk:4,sz:3,am:3},]},
 Descriptor{name:"STB",	    eval:__st,	reg: Name::B, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0xD7,clk:3,sz:2,am:1},M{op:0xE7,clk:4,sz:2,am:2},M{op:0xF7,clk:4,sz:3,am:3},]},
 Descriptor{name:"STD",	    eval:__st,	reg: Name::D, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0xDD,clk:4,sz:2,am:1},M{op:0xED,clk:5,sz:2,am:2},M{op:0xFD,clk:5,sz:3,am:3},]},
 Descriptor{name:"STS",	    eval:__st,	reg: Name::S, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x10DF,clk:5,sz:3,am:1},M{op:0x10EF,clk:6,sz:3,am:2},M{op:0x10FF,clk:6,sz:4,am:3},]},
 Descriptor{name:"STU",	    eval:__st,	reg: Name::U, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0xDF,clk:4,sz:2,am:1},M{op:0xEF,clk:5,sz:2,am:2},M{op:0xFF,clk:5,sz:3,am:3},]},
 Descriptor{name:"STX",	    eval:__st,	reg: Name::X, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x9F,clk:4,sz:2,am:1},M{op:0xAF,clk:5,sz:2,am:2},M{op:0xBF,clk:5,sz:3,am:3},]},
 Descriptor{name:"STY",	    eval:__st,	reg: Name::Y, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x109F,clk:5,sz:3,am:1},M{op:0x10AF,clk:6,sz:3,am:2},M{op:0x10BF,clk:6,sz:4,am:3},]},
 Descriptor{name:"SUBA",	eval:__sub,	reg: Name::A, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x80,clk:2,sz:2,am:0},M{op:0x90,clk:3,sz:2,am:1},M{op:0xA0,clk:4,sz:2,am:2},M{op:0xB0,clk:4,sz:3,am:3},]},
 Descriptor{name:"SUBB",	eval:__sub,	reg: Name::B, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0xC0,clk:2,sz:2,am:0},M{op:0xD0,clk:3,sz:2,am:1},M{op:0xE0,clk:4,sz:2,am:2},M{op:0xF0,clk:4,sz:3,am:3},]},
 Descriptor{name:"SUBD",	eval:__sub,	reg: Name::D, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x83,clk:3,sz:3,am:0},M{op:0x93,clk:4,sz:2,am:1},M{op:0xA3,clk:5,sz:2,am:2},M{op:0xB3,clk:5,sz:3,am:3},]},
 Descriptor{name:"SWI",	    eval:__meta,reg: Name::Z, pbt: PBT::NA,  ot:OT::None,md:&[M{op:0x3F,clk:19,sz:1,am:4},]},
 Descriptor{name:"SWI2",	eval:__meta,reg: Name::Z, pbt: PBT::NA,  ot:OT::None,md:&[M{op:0x103F,clk:20,sz:2,am:4},]},
 Descriptor{name:"SWI3",	eval:__meta,reg: Name::Z, pbt: PBT::NA,  ot:OT::None,md:&[M{op:0x113F,clk:20,sz:2,am:4},]},
 Descriptor{name:"SYNC",	eval:__meta,reg: Name::Z, pbt: PBT::NA,  ot:OT::None,md:&[M{op:0x13,clk:1,sz:1,am:4},]},
 Descriptor{name:"TFR",	    eval:__tfr,	reg: Name::Z, pbt: PBT::TransferExchange,  ot:OT::Exch,md:&[M{op:0x1F,clk:4,sz:2,am:0},]},
 Descriptor{name:"TST",	    eval:__tst,	reg: Name::Z, pbt: PBT::NA,  ot:OT::Mode,md:&[M{op:0x0D,clk:4,sz:2,am:1},M{op:0x6D,clk:5,sz:2,am:2},M{op:0x7D,clk:5,sz:3,am:3},]},
 Descriptor{name:"TSTA",	eval:__tst,	reg: Name::A, pbt: PBT::NA,  ot:OT::None,md:&[M{op:0x4D,clk:1,sz:1,am:4},]},
 Descriptor{name:"TSTB",	eval:__tst,	reg: Name::B, pbt: PBT::NA,  ot:OT::None,md:&[M{op:0x5D,clk:1,sz:1,am:4},]},
];
