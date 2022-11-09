use crate::registers;
use std::{convert::From, fmt};

/// Simple custom Error for the 6809 project
pub struct Error {
    pub kind: ErrorKind,
    pub ctx: Option<registers::Set>,
    pub msg: String,
}

#[derive(Debug, PartialEq)]
pub enum ErrorKind {
    /// error in syntax of assembly code
    Syntax,
    /// error accessing the 6809's memory
    Memory,
    /// underlying io error
    IO,
    /// unresolved reference (e.g. undefined label)
    Reference,
    /// TestCriterion evaluated to false
    Test,
    /// error encountered due to the machine code program
    Runtime,
    /// normal exit (not really an error)
    Exit,
    /// catch-all for other errors
    General,
}

impl Error {
    pub fn new(kind: ErrorKind, ctx: Option<registers::Set>, message: &str) -> Error {
        Error {
            kind,
            ctx,
            msg: String::from(message),
        }
    }
}

impl From<std::io::Error> for Error {
    fn from(e: std::io::Error) -> Self { Error::new(ErrorKind::IO, None, e.to_string().as_str()) }
}

impl fmt::Debug for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}: {}", red!("cpu::Error"), self.msg) }
}
impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut res = write!(f, "{}", self.msg);
        if res.is_ok() {
            if let Some(ctx) = self.ctx {
                res = write!(f, "\nContext: {} -> ({})", ctx, ctx.cc);
            }
        }
        res
    }
}
impl std::error::Error for Error {}
