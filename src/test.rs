//! TestCriterion lines included in an assembly language program enable
//! automated testing of the program by the 6809 simulator
//!
//! Each result line contains an assertion of the form:
//! ```text
//! ;! <identifier-expression> = <value-expression>
//! ```
//! where:
//! ```text
//! identifier-expression evaluates to an ident
//! value-expression evaluates to a value
//! ident := register | address
//! value := constant | address
//! constant := '#' valexpr
//! address := valexpr
//! ```
//!
//! Bit-width rules when RHS is an address:  
//!
//!| LHS | Result |  
//!| --- | --- |  
//!| 8-bit register | 8-bit comparison of register contents with address contents |  
//!| 16-bit register | 16-bit comparison of register contents with address contents |  
//!| address/label | 16-bit comparision of value at lhs address with value at rhs address |  
//!
//! Examples:
//! - `;! a = #$55` Passes if register A contains the value 55 hex when the program is done
//! - `;! $100 = $101` Passes if address 100 (hex) contains the 8-bit value in address 0x101 when the program is done
//! - `;! d = %10000000` Passes if register D equals the 16-bit contents of address 0x80 when the program is done
//! - `;! label = other_label+12` Passes if 16-bit value at _label_ equals the 16-bit value at address _other_label+12_
//! - `;! label+1 = #10` Passes if byte at address _label+1_ equals value 10 (decimal)
//! - `;! label = a` Passes if byte at address _label_ equals value of register A
//! - `;! b = #'C` Passes if register B holds the value of ascii char 'C' (0x43)
//!
use super::*;
#[derive(Debug)]
pub enum RegOrAddr {
    Reg(registers::Name),
    Addr(u16),
}
impl fmt::Display for RegOrAddr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &RegOrAddr::Reg(r) => write!(f, "{:?}", r),
            &RegOrAddr::Addr(a) => write!(f, "${:04X}", a),
        }
    }
}
#[derive(Debug)]
pub enum AddrOrVal {
    Addr(u16),
    Val(u8u16),
}
impl fmt::Display for AddrOrVal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &AddrOrVal::Addr(a) => write!(f, "${:04X}", a),
            &AddrOrVal::Val(u) => write!(f, "#${}", u),
        }
    }
}

#[derive(Debug)]
pub struct TestCriterion {
    pub line_number: usize,
    pub lhs_src: String,
    pub lhs: Option<RegOrAddr>, // A valid register, e.g. A, pc, or X (i.e. registers::Name::X)
    // or a memory location, e.g. $0100 or a label
    pub rhs_src: String,
    pub rhs: Option<AddrOrVal>, // A constant, e.g. #$ff, or #0 or #%0110
                                // or an address, e.g. $0100 or a label
}
impl TestCriterion {
    pub fn new(line_number: usize, lhs_src: &str, rhs_src: &str) -> Self {
        TestCriterion {
            line_number,
            lhs_src: lhs_src.to_string(),
            lhs: None,
            rhs_src: rhs_src.to_string(),
            rhs: None,
        }
    }
    pub fn eval(&self, core: &Core) -> Result<(), Error> {
        let mut lhs_size = 1u16;
        let lhs = self.lhs.as_ref().ok_or(general_err!("TestCriterion missing LHS"))?;
        let rhs = self.rhs.as_ref().ok_or(general_err!("TestCriterion missing RHS"))?;
        let lhs_val = match lhs {
            RegOrAddr::Reg(reg) => {
                lhs_size = registers::reg_size(*reg);
                core.reg.reg_to_val(*reg)
            }
            RegOrAddr::Addr(addr) => {
                if let AddrOrVal::Val(val) = rhs {
                    lhs_size = val.size();
                }
                core._read_u8u16(memory::AccessType::Generic, *addr, lhs_size)?
            }
        };
        let rhs_val = match rhs {
            AddrOrVal::Addr(addr) => core._read_u8u16(memory::AccessType::Generic, *addr, lhs_size)?,
            AddrOrVal::Val(val) => {
                if lhs_size == 2 && val.size() == 1 {
                    u8u16::new(val.u8(), Some(0))
                } else {
                    *val
                }
            }
        };
        if lhs_val == rhs_val {
            Ok(())
        } else {
            Err(Error::new(
                ErrorKind::Test,
                Some(core.reg),
                format!("{} ({}) != {} ({})", lhs, lhs_val, rhs, rhs_val).as_str(),
            ))
        }
    }
}
impl fmt::Display for TestCriterion {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(lhs) = &self.lhs {
            if let Some(rhs) = &self.rhs {
                return write!(f, "{} = {}", lhs, rhs);
            }
        }
        write!(f, "<{} = {}>?", self.lhs_src, self.rhs_src)
    }
}
