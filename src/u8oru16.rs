use std::fmt;

/// u8u16 is a helper enum for dealing with values that may be either u8 or u16.
/// It allows code that cares only about the value to focus on the value while
/// code that cares about the size of the type can also get clear size information.
#[allow(non_camel_case_types)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum u8u16 {
    u8(u8),
    u16(u16),
}
impl fmt::Display for u8u16 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let s = match self {
            u8u16::u8(val) => format!("{:02X}", val),
            u8u16::u16(val) => format!("{:04X}", val),
        };
        write!(f, "{:width$}", s, width = f.width().unwrap_or(0))
    }
}

impl u8u16 {
    pub fn new(lsb: u8, msb: Option<u8>) -> Self {
        if let Some(hi) = msb {
            u8u16::u16(((hi as u16) << 8) | (lsb as u16))
        } else {
            u8u16::u8(lsb)
        }
    }
    pub fn size(&self) -> u16 {
        match self {
            u8u16::u8(_) => 1,
            u8u16::u16(_) => 2,
        }
    }
    pub fn from_u16_shrink(val: u16) -> u8u16 {
        if val < 256 {
            u8u16::u8(val as u8)
        } else {
            u8u16::u16(val)
        }
    }
    pub fn is_u8(&self) -> bool {
        match self {
            u8u16::u8(_) => true,
            u8u16::u16(_) => false,
        }
    }
    pub fn u16(&self) -> u16 {
        match self {
            u8u16::u8(val) => *val as u16,
            u8u16::u16(val) => *val,
        }
    }
    pub fn u8(&self) -> u8 {
        match self {
            u8u16::u8(val) => *val,
            u8u16::u16(val) => (val & 0xff) as u8,
        }
    }
    pub fn lsb(&self) -> u8 { self.u8() }
    pub fn msb(&self) -> Option<u8> {
        match self {
            u8u16::u8(_) => None,
            u8u16::u16(w) => Some((w >> 8) as u8),
        }
    }
    pub fn get_as_bytes(&self, buf: &mut [u8]) -> usize {
        let mut bytes = 0usize;
        if let Some(b) = self.msb() {
            buf[0] = b;
            bytes = 1;
        }
        buf[bytes] = self.lsb();
        bytes + 1
    }
    pub fn i16(self) -> i16 { self.sign_extended().u16() as i16 }
    pub fn twos_complement(self) -> Self {
        match self {
            u8u16::u8(b) => {
                let (u, _) = u8::overflowing_add(!b, 1);
                u8u16::u8(u)
            }
            u8u16::u16(w) => {
                let (u, _) = u16::overflowing_add(!w, 1);
                u8u16::u16(u)
            }
        }
    }
    pub fn overflowing_negate(self) -> (Self, bool) {
        match self {
            u8u16::u8(b) => {
                if b > 0x80 {
                    let u = u8u16::u16(b as u16);
                    (u.twos_complement(), true)
                } else {
                    (self.twos_complement(), false)
                }
            }
            u8u16::u16(w) => (self.twos_complement(), w > 0x8000),
        }
    }
    pub fn force_signed(self, negative: bool) -> (Self, bool) {
        let (neg, overflow) = self.overflowing_negate();
        (if negative { neg } else { self }, overflow)
    }

    pub fn sign_extended(self) -> Self {
        match self {
            u8u16::u16(_) => self,
            u8u16::u8(b) => Self::new(b, Some(if b & 0x80 == 0 { 0u8 } else { 0xffu8 })),
        }
    }
    pub fn signed_offset(self, rhs: Self) -> Self {
        assert!(!self.is_u8());
        if rhs.is_u8() {
            let (val, _) = self.u16().overflowing_add(rhs.sign_extended().u16());
            u8u16::u16(val)
        } else {
            let (val, _) = self.u16().overflowing_add(rhs.u16());
            u8u16::u16(val)
        }
    }
    #[allow(clippy::should_implement_trait)]
    pub fn div(self, rhs: Self) -> Self {
        match (self.is_u8(), rhs.is_u8()) {
            (true, true) => {
                let lval = self.u8();
                let rval = rhs.u8();
                u8u16::u8(lval / rval)
            }
            _ => u8u16::u16(self.u16() / rhs.u16()),
        }
    }

    pub fn modulo(self, rhs: Self) -> Self {
        match (self.is_u8(), rhs.is_u8()) {
            (true, true) => {
                let lval = self.u8();
                let rval = rhs.u8();
                u8u16::u8(lval % rval)
            }
            _ => u8u16::u16(self.u16() % rhs.u16()),
        }
    }
}
