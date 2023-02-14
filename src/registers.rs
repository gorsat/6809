#![allow(dead_code)]
/// MC6809E register set helpers
use super::*;

/// Enumeration of the condition code register bits
#[derive(Copy, Clone)]
pub enum CCBit {
    C = 0,
    V = 1,
    Z = 2,
    N = 3,
    I = 4,
    H = 5,
    F = 6,
    E = 7,
}

/// Representation of the condition code register.
/// The implementation of this struct is effectively the ALU, i.e.,
/// the fundamental math operations are implemented here.
#[derive(Clone, Copy, Default, Debug, PartialEq, Eq)]
pub struct CCBits {
    pub reg: u8,
}

/// Helper struct to map metadata about condition code register bits.
pub struct CCInfo {
    bit: CCBit,
    mask: u8,
    short: char,
    name: &'static str,
}
macro_rules! sign_bit_8 {
    ($b:ident) => {
        (($b & 0x80) == 0x80)
    };
}
macro_rules! sign_bit_16 {
    ($w:ident) => {
        (($w & 0x8000) == 0x8000)
    };
}
macro_rules! signed_sub_overflow {
    ($a:ident,$b:ident, $t:ty) => {{
        let (_, _o) = ($a as $t).overflowing_sub(($b as $t));
        _o
    }};
}
macro_rules! signed_add_overflow {
    ($a:ident,$b:ident, $t:ty) => {{
        let (_, _o) = ($a as $t).overflowing_add(($b as $t));
        _o
    }};
}

macro_rules! twos_comp {
    ($a:ident) => {{
        let (z, _) = $a.overflowing_neg();
        z
    }};
}
/// Metadata for each condition code register bit.
#[rustfmt::skip]
static CC_TABLE: [CCInfo;8] = [
    CCInfo {bit: CCBit::C, mask: 0x01, short: 'C', name: "carry"},
    CCInfo {bit: CCBit::V, mask: 0x02, short: 'V', name: "overflow"},
    CCInfo {bit: CCBit::Z, mask: 0x04, short: 'Z', name: "zero"},
    CCInfo {bit: CCBit::N, mask: 0x08, short: 'N', name: "negative"},
    CCInfo {bit: CCBit::I, mask: 0x10, short: 'I', name: "IRQ mask"},
    CCInfo {bit: CCBit::H, mask: 0x20, short: 'H', name: "half carry"},
    CCInfo {bit: CCBit::F, mask: 0x40, short: 'F', name: "FIRQ mask"},
    CCInfo {bit: CCBit::E, mask: 0x80, short: 'E', name: "entire flag"}
];

impl CCBit {
    pub fn info(&self) -> &CCInfo { &CC_TABLE[*self as usize] }
}

impl CCBits {
    pub fn reset(&mut self) {
        self.reg = 0x50; /* disable IRQ and FIRQ on reset */
    }
    pub fn set_from_byte(&mut self, byte: u8) { self.reg = byte; }
    pub fn or_with_byte(&mut self, byte: u8 ) { self.reg |= byte; }
    pub fn get_as_byte(&self) -> u8 { self.reg }
    pub fn set(&mut self, bit: CCBit, val: bool) {
        let mask: u8 = 1u8 << bit as usize;
        if val {
            self.reg |= mask;
        } else {
            self.reg &= !mask;
        }
    }
    pub fn is_set(&self, bit: CCBit) -> bool { CC_TABLE[bit as usize].mask & self.reg != 0 }
    pub fn get_set_bits(&self) -> Vec<CCBit> {
        let mut v: Vec<CCBit> = Vec::new();
        for t in &CC_TABLE {
            if 0 != (self.reg & t.mask) {
                v.push(t.bit)
            }
        }
        v
    }
    // condition code struct doubles as ALU
    pub fn add_u8(&mut self, a: u8, b: u8, with_carry: bool) -> u8 {
        let carry_in = u8::from(with_carry && self.is_set(CCBit::C));
        let (a1, c1) = a.overflowing_add(carry_in);
        let (result, c2) = a1.overflowing_add(b);
        self.set(CCBit::C, c1 || c2);
        self.set(
            CCBit::V,
            signed_add_overflow!(a, carry_in, i8) || signed_add_overflow!(a1, b, i8),
        );
        self.set(CCBit::Z, result == 0);
        self.set(CCBit::N, sign_bit_8!(result));
        self.set(CCBit::H, ((a & 0xf) + (b & 0xf) + carry_in) & 0x10 == 0x10);
        result
    }
    pub fn add_u16(&mut self, a: u16, b: u16) -> u16 {
        let (result, c) = a.overflowing_add(b);
        self.set(CCBit::C, c);
        self.set(CCBit::V, signed_add_overflow!(a, b, i16));
        self.set(CCBit::Z, result == 0);
        self.set(CCBit::N, sign_bit_16!(result));
        result
    }

    pub fn sub_u16(&mut self, a: u16, b: u16) -> u16 {
        let (result, c) = a.overflowing_sub(b);
        self.set(CCBit::C, c);
        self.set(CCBit::V, signed_sub_overflow!(a, b, i16));
        self.set(CCBit::Z, result == 0);
        self.set(CCBit::N, sign_bit_16!(result));
        result
    }
    pub fn sub_u8(&mut self, a: u8, b: u8, with_carry: bool) -> u8 {
        let borrow = u8::from(with_carry && self.is_set(CCBit::C));
        let (a1, c1) = a.overflowing_sub(borrow);
        let (result, c2) = a1.overflowing_sub(b);
        self.set(CCBit::C, c1 || c2);
        self.set(
            CCBit::V,
            signed_sub_overflow!(a, borrow, i8) || signed_sub_overflow!(a1, b, i8),
        );
        self.set(CCBit::Z, result == 0);
        self.set(CCBit::N, sign_bit_8!(result));
        result
    }
    // note: DEC does not affect the carry flag
    pub fn dec_u8(&mut self, val: u8) -> u8 {
        let (result, v) = val.overflowing_sub(1);
        self.set(CCBit::V, v);
        self.set(CCBit::Z, result == 0);
        self.set(CCBit::N, result & 0x80 != 0);
        result
    }
    // note INC does not affect the carry flag
    pub fn inc_u8(&mut self, val: u8) -> u8 {
        let (result, v) = val.overflowing_add(1);
        self.set(CCBit::V, v);
        self.set(CCBit::Z, result == 0);
        self.set(CCBit::N, result & 0x80 != 0);
        result
    }

    pub fn and_u8(&mut self, val1: u8, val2: u8) -> u8 {
        let result = val1 & val2;
        self.set(CCBit::N, result & 0x80 != 0);
        self.set(CCBit::Z, result == 0);
        self.set(CCBit::V, false);
        result
    }
    pub fn and_u16(&mut self, val1: u16, val2: u16) -> u16 {
        let result = val1 & val2;
        self.set(CCBit::N, result & 0x8000 != 0);
        self.set(CCBit::Z, result == 0);
        self.set(CCBit::V, false);
        result
    }
    pub fn or_u8(&mut self, val1: u8, val2: u8) -> u8 {
        let result = val1 | val2;
        self.set(CCBit::N, result & 0x80 != 0);
        self.set(CCBit::Z, result == 0);
        self.set(CCBit::V, false);
        result
    }
    pub fn xor_u8(&mut self, val1: u8, val2: u8) -> u8 {
        let result = val1 ^ val2;
        self.set(CCBit::N, result & 0x80 != 0);
        self.set(CCBit::Z, result == 0);
        self.set(CCBit::V, false);
        result
    }
    pub fn com_u8(&mut self, val: u8) -> u8 {
        let result = !val;
        self.set(CCBit::C, true);
        self.set(CCBit::V, false);
        self.set(CCBit::N, 0x80 & result != 0);
        self.set(CCBit::Z, result == 0);
        result
    }
    pub fn neg_u8(&mut self, val: u8) -> u8 {
        let result = twos_comp!(val);
        self.set(CCBit::C, val != 0);
        self.set(CCBit::V, val == 0x80);
        self.set(CCBit::N, 0x80 & result != 0);
        self.set(CCBit::Z, result == 0);
        result
    }
    pub fn cmp_u8(&mut self, val1: u8, val2: u8) { self.sub_u8(val1, val2, false); }
    pub fn cmp_u16(&mut self, val1: u16, val2: u16) { self.sub_u16(val1, val2); }
    pub fn shl_u8(&mut self, val: u8) -> u8 {
        let c = sign_bit_8!(val);
        let result = val << 1;
        let n = sign_bit_8!(result);
        self.set(CCBit::C, c);
        self.set(CCBit::V, xor!(c, n));
        self.set(CCBit::Z, result == 0);
        self.set(CCBit::N, n);
        result
    }
    pub fn shr_u8(&mut self, val: u8, preserve_sign: bool) -> u8 {
        let c = val & 1 == 1;
        let mut result = val >> 1;
        if preserve_sign {
            result |= val & 0x80;
        }
        self.set(CCBit::C, c);
        self.set(CCBit::Z, result == 0);
        self.set(CCBit::N, sign_bit_8!(result));
        result
    }
    pub fn rol_u8(&mut self, val: u8) -> u8 {
        let c = sign_bit_8!(val);
        let mut result = val << 1;
        if self.is_set(CCBit::C) {
            result |= 1;
        }
        let n = sign_bit_8!(result);
        self.set(CCBit::C, c);
        self.set(CCBit::V, xor!(c, n));
        self.set(CCBit::Z, result == 0);
        self.set(CCBit::N, n);
        result
    }
    pub fn ror_u8(&mut self, val: u8) -> u8 {
        let c = val & 1 == 1;
        let mut result = val >> 1;
        if self.is_set(CCBit::C) {
            result |= 0x80;
        }
        self.set(CCBit::C, c);
        self.set(CCBit::Z, result == 0);
        self.set(CCBit::N, sign_bit_8!(result));
        result
    }
    pub fn mul(&mut self, a: u8, b: u8) -> u16 {
        let d: u16 = (a as u16) * (b as u16);
        // carry = bit 7 (8th bit) of result
        self.set(CCBit::C, d & 0x80 != 0);
        self.set(CCBit::Z, d == 0);
        d
    }
}
use std::fmt;
impl fmt::Display for CCBits {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            concat!(
                green!("{}:"),
                "{} ",
                green!("{}:"),
                "{} ",
                green!("{}:"),
                "{} ",
                green!("{}:"),
                "{} ",
                green!("{}:"),
                "{} ",
                green!("{}:"),
                "{} ",
                green!("{}:"),
                "{} ",
                green!("{}:"),
                "{}"
            ),
            CC_TABLE[0].short,
            self.is_set(CC_TABLE[0].bit) as usize,
            CC_TABLE[1].short,
            self.is_set(CC_TABLE[1].bit) as usize,
            CC_TABLE[2].short,
            self.is_set(CC_TABLE[2].bit) as usize,
            CC_TABLE[3].short,
            self.is_set(CC_TABLE[3].bit) as usize,
            CC_TABLE[4].short,
            self.is_set(CC_TABLE[4].bit) as usize,
            CC_TABLE[5].short,
            self.is_set(CC_TABLE[5].bit) as usize,
            CC_TABLE[6].short,
            self.is_set(CC_TABLE[6].bit) as usize,
            CC_TABLE[7].short,
            self.is_set(CC_TABLE[7].bit) as usize,
        )
    }
}

/// Enumeration of all registers and a placeholder, invalid register called 'Z'.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Name {
    A,
    B,
    D,
    X,
    Y,
    U,
    S,
    PC,
    DP,
    CC,
    Z, // Error case; non-existent register
}
const REG_NAMES: &[&str] = &["A", "B", "D", "X", "Y", "U", "S", "PC", "DP", "CC", "Z"];

impl Name {
    pub fn to_str(self) -> &'static str { REG_NAMES[self as usize] }
    pub fn from_str(s: &str) -> Self {
        match s.to_ascii_uppercase().as_str() {
            "A" => Name::A,
            "B" => Name::B,
            "D" => Name::D,
            "X" => Name::X,
            "Y" => Name::Y,
            "U" => Name::U,
            "S" => Name::S,
            "PC" => Name::PC,
            "DP" => Name::DP,
            "CC" => Name::CC,
            _ => Name::Z,
        }
    }
}
pub fn reg_size(reg: Name) -> u16 {
    match reg {
        Name::A | Name::B | Name::DP | Name::CC => 1,
        Name::D | Name::X | Name::Y | Name::U | Name::S | Name::PC => 2,
        Name::Z => 0,
    }
}

/// Provides storage and helpers for the full set of 6809 registers.
#[derive(Clone, Copy, Default)]
pub struct Set {
    pub a: u8,      // accumulator
    pub b: u8,      // accumulator
    pub d: u16,     // accumulator
    pub x: u16,     // index register
    pub y: u16,     // index register
    pub u: u16,     // user stack pointer
    pub s: u16,     // hardware stack pointer
    pub pc: u16,    // program counter
    pub dp: u8,     // direct page register
    pub cc: CCBits, // condition code register
}
impl Set {
    pub fn reset(&mut self) {
        self.x = 0;
        self.y = 0;
        self.u = 0;
        self.s = 0;
        self.pc = 0;
        self.a = 0;
        self.b = 0;
        self.d = 0;
        self.dp = 0;
        self.cc.reset();
    }
    pub fn set_register(&mut self, reg: Name, val: u8u16) {
        match reg {
            Name::A => {
                self.a = val.u8();
                self.sync_d();
            }
            Name::B => {
                self.b = val.u8();
                self.sync_d();
            }
            Name::D => {
                self.d = val.u16();
                self.sync_ab();
            }
            Name::X => self.x = val.u16(),
            Name::Y => self.y = val.u16(),
            Name::U => self.u = val.u16(),
            Name::S => self.s = val.u16(),
            Name::PC => self.pc = val.u16(),
            Name::DP => self.dp = val.u8(),
            Name::CC => self.cc.set_from_byte(val.u8()),
            Name::Z => panic!("invalid register"),
        }
    }
    pub fn get_register(&self, reg: Name) -> u8u16 {
        match reg {
            Name::A => u8u16::u8(self.a),
            Name::B => u8u16::u8(self.b),
            Name::D => u8u16::u16(self.d),
            Name::X => u8u16::u16(self.x),
            Name::Y => u8u16::u16(self.y),
            Name::U => u8u16::u16(self.u),
            Name::S => u8u16::u16(self.s),
            Name::PC => u8u16::u16(self.pc),
            Name::DP => u8u16::u8(self.dp),
            Name::CC => u8u16::u8(self.cc.get_as_byte()),
            Name::Z => panic!("invalid register"),
        }
    }
    fn sync_d(&mut self) { self.d = ((self.a as u16) << 8) | (self.b as u16); }
    fn sync_ab(&mut self) {
        self.a = (self.d >> 8) as u8;
        self.b = (self.d & 0xff) as u8;
    }
}
impl fmt::Debug for Set {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { <Set as fmt::Display>::fmt(self, f) }
}
impl fmt::Display for Set {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            concat!(
                blue!("X:"),
                "{:04x} ",
                blue!("Y:"),
                "{:04x} ",
                blue!("U:"),
                "{:04x} ",
                blue!("S:"),
                "{:04x} ",
                blue!("PC:"),
                "{:04x} ",
                blue!("A:"),
                "{:02x} ",
                blue!("B:"),
                "{:02x} ",
                blue!("D:"),
                "{:04x} ",
                blue!("DP:"),
                "{:02x} ",
                blue!("CC:"),
                "{:02x}"
            ),
            self.x, self.y, self.u, self.s, self.pc, self.a, self.b, self.d, self.dp, self.cc.reg
        )
    }
}
