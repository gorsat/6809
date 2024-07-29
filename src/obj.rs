use super::instructions::*;
use super::parse::{IncDecType, LabelResolver, OperandDescriptor, ValueNode};
use super::*;

/// The assembler translates each assembly language statement into a BinaryObject.
/// When the assembler is done building the program, the result is a list of BinaryObjects.
/// Each BinaryObject is comprised of a contiguous sequence of bytes and an address at which
/// that sequence should begin in the 6809's address space.
#[derive(Debug, Clone)]
pub struct BinaryObject {
    pub addr: u16,
    pub is_static_addr: bool,
    pub size: u16,
    pub data: Option<Vec<u8u16>>,
}
impl BinaryObject {
    pub fn to_bytes(&self, buf: &mut [u8]) -> u16 {
        let mut bytes = 0;
        if let Some(data) = self.data.as_ref() {
            data.iter().for_each(|&u| bytes += u.get_as_bytes(&mut buf[bytes..]));
        }
        bytes as u16
    }
    pub fn calc_size(&mut self) -> u16 {
        let mut size = self.size;
        if let Some(data) = self.data.as_ref() {
            size = 0;
            data.iter().for_each(|&u| size += u.size());
        }
        self.size = size;
        size
    }
}
impl fmt::Display for BinaryObject {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut s = format!("{:04X} ", self.addr);
        if let Some(data) = self.data.as_ref() {
            data.iter().for_each(|u| {
                s.push_str(format!("{:4} ", u).as_str());
            });
        }
        write!(f, "{:width$}", s, width = f.width().unwrap_or(0))
    }
}

/// An ObjectProducer is a struct that is capable of producing a BinaryObject.
pub trait ObjectProducer: std::fmt::Debug + std::fmt::Display {
    // if this object has a static address (e.g. if the object is an ORG) then it is reported here (only after build!)
    fn static_address(&self, _: &dyn LabelResolver) -> Result<Option<u16>, Error> { Ok(None) }
    // given an address and label definitions, provide the upper bound on the size of this object
    fn current_size(&self, _: u16, _: &dyn LabelResolver) -> Result<u16, Error> { Ok(0u16) }
    // given an address and label definitions, produce this object
    fn build(&mut self, addr: u16, lr: &dyn LabelResolver, dp_dirty: bool) -> Result<&BinaryObject, Error>;

    // returns true if the object results in potential DP register change
    fn changes_dp(&self) -> bool { false }

    // get a ref to this producer's object (if there is one)
    fn bob_ref(&self) -> Option<&BinaryObject>;
}

/// Builds a BinaryObject for a given 6809 assembly language instruction.
/// This does the work of translating a statement like "LDA ,X+" into machine code.
#[derive(Debug)]
pub struct Instruction {
    pub id: &'static instructions::Descriptor, // apriori info about the instruction
    pub od: OperandDescriptor,                 // info about the observed operand (if any)
    pub flavor: instructions::Flavor,          // flavor determined by id and od
    bob: BinaryObject,                         // binary representation of this instruction
    dp_changed: bool,
    built: bool,
    trying_direct: bool,
}
impl Instruction {
    pub fn try_new(
        id: &'static instructions::Descriptor, od: OperandDescriptor, addr: u16, lr: &dyn LabelResolver, dp_dirty: bool,
    ) -> Result<Self, Error> {
        // translate from the assembler's addressing mode to the runtime addressing mode
        let mut dp_changed = false;
        let mut trying_direct = false;
        let rt_mode = match od.mode {
            AddressingMode::Register => {
                // the parser uses this designation for any operand that includes a list of registers, i.e. R1,R2[,Rn]*
                // check to see if we're modifying the DP register
                if let Some(regs) = &od.regs {
                    if regs.contains(&"DP".to_string()) {
                        if id.name == "TFR" {
                            dp_changed = regs.len() > 1 && regs[1].eq("DP");
                        } else {
                            dp_changed = id.name == "EXG" || id.name == "PULS" || id.name == "PULU";
                        }
                    }
                }
                // we use OperandType to differentiate
                match id.ot {
                    OperandType::None => {
                        return Err(syntax_err!("illegal register addressing"));
                    }
                    OperandType::Mode => AddressingMode::Indexed,
                    OperandType::Push => AddressingMode::Immediate,
                    OperandType::Exch => AddressingMode::Immediate,
                }
            }
            AddressingMode::Offset => AddressingMode::Indexed,
            AddressingMode::PCRelative => AddressingMode::Indexed,
            AddressingMode::IncDec => AddressingMode::Indexed,
            AddressingMode::Extended => {
                // Indirect extended addressing mode is really just another "indexed" addressing mode
                if od.indirect {
                    AddressingMode::Indexed
                }
                // the parser reports extended mode whenever it finds a non-indirect, non-immediate value in the operand.
                // if this instruction doesn't support extended but it does support relative then relative is the right mode
                else if id.get_mode_detail(AddressingMode::Extended).is_none()
                    && id.get_mode_detail(AddressingMode::Relative).is_some()
                {
                    AddressingMode::Relative
                // if the DP hasn't changed yet and the address fits in 8 bits then try using Direct mode.
                // if it doesn't work then we'll have to change at build time
                } else if !dp_dirty
                    && !od.force_mode
                    && od
                        .value
                        .as_ref()
                        .map_or(false, |v| v.eval(lr, addr, false).map_or(false, |u| u.u16() < 0x100))
                {
                    trying_direct = true;
                    AddressingMode::Direct
                } else {
                    AddressingMode::Extended
                }
            }
            _ => od.mode,
        };
        if let Some(detail) = id.get_mode_detail(rt_mode) {
            let flavor = Flavor {
                desc: id,
                mode: rt_mode,
                detail,
            };
            return Ok(Instruction {
                id,
                od,
                flavor,
                bob: BinaryObject {
                    addr: 0,
                    is_static_addr: false,
                    size: 0,
                    data: None,
                },
                dp_changed,
                built: false,
                trying_direct,
            });
        }
        Err(Error::new(
            ErrorKind::Syntax,
            None,
            "could not identify instruction variant; invalid addressing mode?",
        ))
    }
    pub fn _build_indexed(
        &self, addr: u16, mut val: u8u16, data: &mut Vec<u8u16>, indirect: bool,
    ) -> Result<(), Error> {
        match self.od.mode {
            AddressingMode::Register => {
                let regs = self.od.regs.as_ref().unwrap();
                assert!(regs.len() > 1);

                if regs.len() != 2 {
                    return Err(syntax_err!("two registers required for register offset addressing"));
                }
                let mut post_byte = match regs[0].as_str() {
                    "A" => 0b10000110,
                    "B" => 0b10000101,
                    "D" => 0b10001011,
                    _ => {
                        return Err(syntax_err!(
                            format!("register \"{}\" invalid as offset", regs[0]).as_str()
                        ));
                    }
                };
                self._add_index_register_to_postbyte(&mut post_byte, regs[1].as_str())?;
                if indirect {
                    post_byte |= 0b00010000
                };
                data.push(u8u16::u8(post_byte));
            }
            AddressingMode::Offset | AddressingMode::PCRelative => {
                // val should hold the offset
                // the post-byte varies depending on the register, the size of the offset
                // and whether we're in indirect mode
                let regs = self.od.regs.as_ref().unwrap();
                assert!(regs.len() == 1);
                let mut post_byte = 0x80u8;
                self._add_index_register_to_postbyte(&mut post_byte, regs[0].as_str())?;
                if indirect {
                    post_byte |= 0x10;
                }
                let mut add_offset = true;
                if post_byte & 0b1100 != 0 {
                    // indexing based on PC
                    if self.od.mode == AddressingMode::PCRelative {
                        // PCR mode; determine the offset
                        // try using an 8-bit offset first
                        let pc = addr + self.flavor.detail.sz + 1;
                        let (mut offset, _) = u16::overflowing_sub(val.u16(), pc);
                        let hi = (offset >> 8) as u8;
                        if hi == 0 || (hi == 0xff && (offset & 0x80 == 0x80)) {
                            val = u8u16::u8(offset as u8);
                        } else {
                            // 8-bit offset wasn't big enough; use 16-bit offset instead
                            (offset, _) = u16::overflowing_sub(val.u16(), pc + 1);
                            val = u8u16::u16(offset);
                        }
                    }
                } else {
                    // not indexing based on PC
                    if val.u16() == 0 {
                        // offset is zero
                        post_byte |= 0b100;
                        add_offset = false;
                    } else {
                        // check to see if the offset fits in 5 bits
                        let x = val.sign_extended().u16() & 0xfff0;
                        if !indirect && x == 0xfff0 || x == 0 {
                            // offset fits in 5-bits, mode is not indirect, not indexing based on PC
                            post_byte |= val.u8() & 0b11111; // store offset in bottom 5 bits
                            post_byte &= 0x7f;
                            add_offset = false;
                        }
                    }
                }
                if add_offset && !val.is_u8() {
                    // offset requires 2 bytes
                    post_byte |= 1;
                }
                data.push(u8u16::u8(post_byte));
                if add_offset {
                    data.push(val);
                }
            }
            AddressingMode::IncDec => {
                let regs = self.od.regs.as_ref().unwrap();
                assert!(regs.len() == 1);
                if let Some(incdec) = &self.od.incdec {
                    let mut post_byte: u8 = if indirect { 0b00010000 } else { 0 };
                    post_byte |= match incdec {
                        IncDecType::Dec => {
                            if indirect {
                                return Err(syntax_err!("illegal indirection"));
                            };
                            0b10000010
                        }
                        IncDecType::DecDec => 0b10000011,
                        IncDecType::Inc => {
                            if indirect {
                                return Err(syntax_err!("illegal indirection"));
                            };
                            0b10000000
                        }
                        IncDecType::IncInc => 0b10000001,
                    };
                    self._add_index_register_to_postbyte(&mut post_byte, regs[0].as_str())?;
                    data.push(u8u16::u8(post_byte));
                } else {
                    panic!("missing increment or decrement");
                }
            }
            AddressingMode::Extended => {
                // our object is just a fixed post-byte and a 16-bit address
                data.push(u8u16::u8(0b10011111 | if indirect { 0b00010000 } else { 0 }));
                data.push(u8u16::u16(val.u16()));
            }
            _ => unreachable!(),
        }
        Ok(())
    }
    fn _add_index_register_to_postbyte(&self, post_byte: &mut u8, reg: &str) -> Result<(), Error> {
        *post_byte |= match reg {
            "X" => 0b00000000,
            "Y" => 0b00100000,
            "U" => 0b01000000,
            "S" => 0b01100000,
            "PC" | "PCR" => 0b00001100,
            _ => {
                return Err(syntax_err!(format!("invalid index register \"{}\"", reg).as_str()));
            }
        };
        Ok(())
    }
    pub fn _build_immediate(&self, val: u8u16, data: &mut Vec<u8u16>) -> Result<(), Error> {
        if self.od.mode != AddressingMode::Register {
            data.push(val);
            return Ok(());
        }
        if let Some(regs) = &self.od.regs {
            if self.id.ot == OperandType::Exch {
                // this is a tfr or exg instruction
                if regs.len() != 2 {
                    return Err(syntax_err!("invalid number of registers"));
                }
                // create the postbyte and add it to the object
                if let Some(pb) = TEPostByte::make(&regs[0], &regs[1]) {
                    data.push(u8u16::u8(pb));
                    return Ok(());
                }
            } else {
                // shouldn't be possible to get here if this isn't a psh/pul instruction
                assert!(self.id.ot == OperandType::Push);
                if let Some(pb) = PPPostByte::make(regs) {
                    // check that we didn't try to push U onto the U stack or S onto the S stack
                    let op = self.flavor.detail.op;
                    if ((op == 0x34 || op == 0x35) && regs.contains(&"S".to_string()))
                        || ((op == 0x36 || op == 0x37) && regs.contains(&"U".to_string()))
                    {
                        return Err(syntax_err!("cannot PSH/PUL stack pointer on its own stack"));
                    }
                    // add postbyte to object
                    data.push(u8u16::u8(pb));
                    return Ok(());
                }
            }
        }
        Err(syntax_err!("invalid registers"))
    }
}
impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:width$}", &self.flavor, width = f.width().unwrap_or(0))
    }
}
impl ObjectProducer for Instruction {
    fn current_size(&self, _: u16, _: &dyn LabelResolver) -> Result<u16, Error> {
        Ok(if self.bob.data.is_some() {
            self.bob.size
        } else {
            self.flavor.detail.sz
        })
    }
    fn bob_ref(&self) -> Option<&BinaryObject> {
        if !self.built {
            return None;
        }
        Some(&self.bob)
    }
    /// This is one of the uglier and more confusing functions in the codebase.
    /// It's probably a good candidate for rethinking and refactoring.
    /// On the other hand, it seems to work so I'm not very motivated to mess with it.
    ///
    /// When build is called, self.od.mode is the addressing mode as seen by the assembler parser
    /// but self.flavor.mode is the addressing mode that the CPU will see at run time.
    /// E.g., the assembler would see operand "A,X" as AddressingMode::Register but the CPU will
    /// see it as AddressingMode::Indexed with a postbyte that describes the register offset
    ///
    fn build(&mut self, addr: u16, lr: &dyn LabelResolver, dp_dirty: bool) -> Result<&BinaryObject, Error> {
        let mut val = u8u16::u8(0);
        let mut sval = u8u16::u8(0);
        let mut data: Vec<u8u16> = Vec::new();
        let mut min_size = self.flavor.detail.sz;
        let mut working_size = self.flavor.detail.op_size();

        // Before attempting to build an object, we do some general setup and checks.
        // The checks here are slightly awkward but doing it this way avoids a lot of
        // duplication of the same checks in the code that follows for each addressing mode.

        // Do we have a value in the operand? If so, evaluate it
        if let Some(node) = self.od.value.as_ref() {
            // if we can't evaluate at build time then it's an error
            // get both the unsigned and signed evaluations
            // we'll choose which one to use later
            val = node.eval(lr, addr, false)?;
            sval = node.eval(lr, addr, true)?;
            working_size += val.size();
        } else if self.flavor.mode != AddressingMode::Inherent
            && self.od.mode != AddressingMode::IncDec
            && (self.od.mode != AddressingMode::Register || self.od.regs.is_none())
            && (self.od.mode != AddressingMode::Offset || self.od.regs.is_none())
        {
            // inherent mode has no operand.
            // incdec has no offset (no value)
            // register mode uses the .regs member of self.od rather than .value
            // indexed offset mode can have a non-existent (zero) offset
            // other cases all require a value
            return Err(syntax_err!("missing value in operand"));
        }
        // should we try to optimize for Direct mode addressing?
        if !dp_dirty
            && !self.od.force_mode
            && (val.u16() < 0x100)
            && (self.flavor.mode == AddressingMode::Extended || self.flavor.mode == AddressingMode::Direct)
        {
            if let Some(detail) = self.id.get_mode_detail(AddressingMode::Direct) {
                self.flavor = Flavor {
                    desc: self.id,
                    mode: AddressingMode::Direct,
                    detail,
                };
                val = u8u16::u8(val.lsb());
                min_size = self.flavor.detail.sz;
                working_size = self.flavor.detail.op_size() + 1;
            }
        } else if (self.flavor.mode == AddressingMode::Direct) && (dp_dirty || (val.u16() > 0xff)) {
            if self.trying_direct {
                if let Some(detail) = self.id.get_mode_detail(AddressingMode::Extended) {
                    // failed to optimize into direct mode; switch back to extended
                    self.flavor = Flavor {
                        desc: self.id,
                        mode: AddressingMode::Extended,
                        detail,
                    };
                    val = u8u16::u16(val.u16());
                    min_size = self.flavor.detail.sz;
                    working_size = self.flavor.detail.op_size() + 2;
                    self.trying_direct = false;
                } else {
                    panic!("Is there an instruction that supports Direct mode but not Extended?")
                }
            } else {
                // programmer tried to force direct mode but it failed
                return Err(syntax_err!("invalid use of direct mode addressing"));
            }
        }
        if self.flavor.mode == AddressingMode::Indexed || self.od.mode == AddressingMode::Register {
            // use the signed evaluation of the operand
            val = sval;
        } else if (self.flavor.mode == AddressingMode::Extended) && (val.size() == 1) {
            // we got an 8 bit address value with Extended mode, but we can't switch to Direct mode
            // or we already would have done so above, so here we just need to convert val to 16-bit
            val = u8u16::u16(val.u16());
        } else if working_size != min_size && self.flavor.mode != AddressingMode::Relative {
            if (self.flavor.mode == AddressingMode::Immediate) && (min_size == working_size + 1) && (val.size() == 1) {
                // this is a 16-bit immediate mode instruction, so we need a 16=bit value
                val = u8u16::u16(val.u16());
            } else {
                // for modes other than those we've explicitly checked above,
                // min_size should be equal to actual_size at this point

                // last check before giving up: an 8-bit immediate mode operation for which we have a 16-bit value
                // but that value will fit into 8 bits
                if (self.flavor.mode == AddressingMode::Immediate)
                    && (working_size == min_size + 1)
                    && (val.u16() < 0x100)
                {
                    val = u8u16::u8(val.lsb());
                } else {
                    return Err(syntax_err!("invalid operand size"));
                }
            }
        }

        // start building the object by adding the opcode
        data.push(self.flavor.detail.op_as_u8u16());

        // now do all the AddressingMode-specific build work...
        // note that this is matching on self.flavor.mode (the mode the CPU will see at run time)
        match self.flavor.mode {
            AddressingMode::Immediate => self._build_immediate(val, &mut data)?,
            AddressingMode::Indexed => self._build_indexed(addr, val, &mut data, self.od.indirect)?,
            AddressingMode::Inherent => {
                // there is no more to do in this case; the op code is the entire object
            }
            AddressingMode::Relative => {
                // val holds the address that we're operating relative to.
                // working_size might be bigger than min_size when we get here
                // because, e.g., the address given in the operand is 16-bit
                // but it may resolve into an 8-bit relative offset.
                // So we start by assuming we can use an 8-bit offset.
                working_size = min_size;
                // operation is relative to the program counter which points to the instruction *after* this one
                let pc: u16 = addr + working_size;
                let (diff, _) = u16::overflowing_sub(val.u16(), pc);
                if min_size - self.flavor.detail.op_size() == 1 {
                    // expecting a signed, 8-bit relative offset here
                    let n = diff as i16;
                    if !(-128..=127).contains(&n) {
                        if config::ARGS.lbr_disable {
                            return Err(syntax_err!("relative offset is out of bounds"));
                        } else {
                            verbose_println!("Converting Bxx to LBxx (pc:{:X},addr:{:x},diff:{})", pc, val.u16(), n);
                            // convert this branch instruction to the "long" version
                            let new_name = "L".to_string() + self.id.name;
                            if let Some(desc) = instructions::name_to_descriptor(new_name.as_str()) {
                                // update the significant fields and then call .build() again
                                self.id = desc;
                                self.flavor.desc = desc;
                                self.flavor.detail = desc.get_mode_detail(AddressingMode::Relative).unwrap();
                                return self.build(addr, lr, dp_dirty);
                            }
                        }
                        panic!("failed to convert Branch operation to LongBranch")
                    }
                    data.push(u8u16::u8(diff as u8));
                } else {
                    data.push(u8u16::u16(diff));
                }
            }
            AddressingMode::Direct | AddressingMode::Extended => {
                // for these modes, we just need to add the operand value to the object
                data.push(val);
            }
            _ => {
                // this is not a valid run time mode and we should never get here
                panic!("should not get here!")
            }
        }
        self.bob.addr = addr;
        self.bob.data = Some(data);
        self.bob.calc_size();
        self.built = true;
        Ok(&self.bob)
    }

    fn changes_dp(&self) -> bool { self.dp_changed }
}
/// Builds a BinaryObject given the operand of an RMB (Reserve Memory Bytes) statement.
#[derive(Debug)]
pub struct Rmb {
    node: ValueNode,
    size: Option<u16>,
    bob: BinaryObject,
    built: bool,
}
impl Rmb {
    pub fn new(node: ValueNode) -> Self {
        Rmb {
            node,
            size: None,
            bob: BinaryObject {
                addr: 0,
                is_static_addr: false,
                size: 0,
                data: None,
            },
            built: false,
        }
    }
}
impl ObjectProducer for Rmb {
    fn bob_ref(&self) -> Option<&BinaryObject> {
        if !self.built {
            return None;
        }
        Some(&self.bob)
    }
    fn build(&mut self, addr: u16, lr: &dyn LabelResolver, _: bool) -> Result<&BinaryObject, Error> {
        // if this value node can't be evaluated then it's invalid
        let u = self.node.eval(lr, addr, false)?.u16();
        self.size = Some(u);
        self.bob.addr = addr;
        self.bob.size = u;
        self.built = true;
        Ok(&self.bob)
    }
    fn current_size(&self, addr: u16, lr: &dyn LabelResolver) -> Result<u16, Error> {
        if let Some(size) = self.size {
            return Ok(size);
        }
        // for RMB, max_size == size always
        // but we don't know size unless we can evaluate our operand
        // so this is the same as build (except self is mut, so we can't set self.size)
        let u = self.node.eval(lr, addr, false)?.u16();
        Ok(u)
    }
}
impl fmt::Display for Rmb {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(size) = self.size {
            write!(f, "RMB {} bytes", size)
        } else {
            write!(f, "RMB {}", self.node)
        }
    }
}
/// Builds a BinaryObject given the operand of an FCB or FDB statement.
#[derive(Debug)]
pub struct Fxb {
    nodes: Vec<ValueNode>,
    bytes_per_node: u16,
    bob: BinaryObject,
    built: bool,
}
impl Fxb {
    pub fn new(nodes: Vec<ValueNode>, is_bytes: bool) -> Self {
        Fxb {
            nodes,
            bytes_per_node: if is_bytes { 1u16 } else { 2u16 },
            bob: BinaryObject {
                addr: 0,
                is_static_addr: false,
                size: 0,
                data: None,
            },
            built: false,
        }
    }
}
impl ObjectProducer for Fxb {
    fn bob_ref(&self) -> Option<&BinaryObject> {
        if !self.built {
            return None;
        }
        Some(&self.bob)
    }
    fn current_size(&self, _: u16, _: &dyn LabelResolver) -> Result<u16, Error> {
        Ok(self.bytes_per_node * self.nodes.len() as u16)
    }

    fn build(&mut self, addr: u16, lr: &dyn LabelResolver, _: bool) -> Result<&BinaryObject, Error> {
        // Fxb renders one or more bytes at the current address
        let mut data = Vec::new();
        for node in &self.nodes {
            let val = node.eval(lr, addr, false)?;
            #[allow(clippy::comparison_chain)]
            if val.size() > self.bytes_per_node {
                return Err(syntax_err!("16-bit data in FCB is invalid"));
            } else if val.size() < self.bytes_per_node {
                data.push(u8u16::u16(val.u16()));
            } else {
                data.push(val);
            }
        }
        self.bob.addr = addr;
        self.bob.data = Some(data);
        self.bob.size = 0;
        self.bob.calc_size();
        self.built = true;
        Ok(&self.bob)
    }
}
impl fmt::Display for Fxb {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let op = if self.bytes_per_node == 1 { "FCB" } else { "FDB" };
        write!(f, "{} {}", op, self.nodes[0])?;
        for n in &self.nodes[1..] {
            write!(f, ", {}", n)?;
        }
        Ok(())
    }
}

/// Builds a BinaryObject given the operand of an ORG statement.
#[derive(Debug)]
pub struct Org {
    node: ValueNode,
    bob: BinaryObject,
    built: bool,
}
impl Org {
    pub fn new(node: ValueNode) -> Self {
        Org {
            node,
            bob: BinaryObject {
                addr: 0,
                is_static_addr: true,
                size: 0,
                data: None,
            },
            built: false,
        }
    }
}
impl ObjectProducer for Org {
    fn static_address(&self, lr: &dyn LabelResolver) -> Result<Option<u16>, Error> {
        // Note: org cannot use location reference!
        let addr = self.node.eval(lr, 0, false)?;
        Ok(Some(addr.u16()))
    }
    fn bob_ref(&self) -> Option<&BinaryObject> {
        if !self.built {
            return None;
        }
        Some(&self.bob)
    }
    fn build(&mut self, addr: u16, lr: &dyn LabelResolver, _: bool) -> Result<&BinaryObject, Error> {
        // if this value node can't be evaluated then it's invalid
        self.bob.addr = self.node.eval(lr, addr, false)?.u16();
        self.built = true;
        Ok(&self.bob)
    }
}
impl fmt::Display for Org {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "ORG {}", self.node) }
}
/// Builds a BinaryObject given the operand of an FCC statement.
#[derive(Debug)]
pub struct Fcc {
    source: String, // saving a copy of the source string for debugging/printing
    bob: BinaryObject,
    built: bool,
}
impl Fcc {
    pub fn new(s: &str) -> Self {
        let size = s.len() as u16;
        let mut data = Vec::with_capacity(size as usize);
        for b in s.bytes() {
            data.push(u8u16::u8(b));
        }
        Fcc {
            source: s.to_string(),
            bob: BinaryObject {
                addr: 0,
                is_static_addr: false,
                size,
                data: Some(data),
            },
            built: false,
        }
    }
}
impl ObjectProducer for Fcc {
    fn bob_ref(&self) -> Option<&BinaryObject> {
        if !self.built {
            return None;
        }
        Some(&self.bob)
    }
    fn build(&mut self, addr: u16, _: &dyn LabelResolver, _: bool) -> Result<&BinaryObject, Error> {
        self.bob.addr = addr;
        self.built = true;
        Ok(&self.bob)
    }
}
impl fmt::Display for Fcc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "FCC {}", self.source) }
}
