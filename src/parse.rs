use super::instructions::AddressingMode;
use super::test::{AddrOrVal, RegOrAddr, TestCriterion};

use super::*;

use regex::Regex;
use std::{iter::Peekable, str::Chars, vec::IntoIter};

type TokenIter = Peekable<IntoIter<Token>>;

#[derive(Debug, PartialEq, Eq)]
pub enum TokenType {
    Number,
    Label,
    Register,
    Add,
    Sub,
    Star, // used for both multiplication and location reference
    Div,
    Mod,
    Pow,
    Hash,
    Comma,
    LBracket,
    RBracket,
    LParen,
    RParen,
    LAngle,
    RAngle,
}
impl From<Token> for TokenType {
    fn from(item: Token) -> Self { item.ttype }
}

/// The first step in parsing an operand is tokenization. This struct holds
/// all the relevant info about an individual token.
#[derive(Debug)]
pub struct Token {
    /// the type of this token
    ttype: TokenType,
    /// raw source string that resulted in creation of this token
    raw: String,
    /// if token is numeric then this holds the value
    value: Option<u8u16>,
}
impl Token {
    pub fn clean(&self) -> String {
        match self.ttype {
            TokenType::Number => self.value.as_ref().unwrap().to_string(),
            TokenType::Register => self.raw.clone().to_uppercase(),
            _ => self.raw.clone(),
        }
    }
    pub fn new(ttype: TokenType, raw: String, value: Option<u8u16>) -> Self { Token { ttype, raw, value } }
}
impl std::cmp::PartialEq<TokenType> for Token {
    fn eq(&self, other: &TokenType) -> bool { self.ttype == *other }
}
impl std::cmp::PartialEq<Token> for TokenType {
    fn eq(&self, other: &Token) -> bool { *self == other.ttype }
}

use fmt::Display;
impl Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}\"{}\"<--\"{}\"", self.ttype, self.clean(), self.raw)
    }
}
pub trait LabelResolver {
    fn resolve(&self, label: &str) -> Option<u8u16>;
}

/// Each value expression is parsed and converted into a tree of ValueNode objects.
/// The tree could consist of only a root ValueNode containing a value or it may be
/// arbitrarily deep, in which case the root ValueNode's token will be an operator.
#[derive(Debug)]
pub struct ValueNode {
    /// either the node's value (e.g. label or number) or an operation
    pub token: Token,
    /// true if the node's value should be negated after resolution
    pub negate: bool,
    /// child nodes
    left: Option<Box<ValueNode>>,
    right: Option<Box<ValueNode>>,
}
impl ValueNode {
    pub fn new(val_or_op: Token, negate: bool, left: Option<ValueNode>, right: Option<ValueNode>) -> Self {
        ValueNode {
            token: val_or_op,
            negate,
            left: left.map(Box::new),
            right: right.map(Box::new),
        }
    }
    /// Evaluate this ValueNode given an address and LabelResolver.
    /// If this ValueNode (or its children) require a label that the LabelResolver can't resolve
    /// then the result will be an Error of ErrorKind::Reference.
    // Indicate signed = true if the outcome of an evaluation MIGHT be signed.
    pub fn eval(&self, lr: &dyn LabelResolver, addr: u16, signed: bool) -> Result<u8u16, Error> {
        match self.token.ttype {
            TokenType::Number => {
                let v = self.token.value.unwrap();
                if signed || self.negate {
                    let (u, _) = v.force_signed(self.negate);
                    Ok(u)
                } else {
                    Ok(v)
                }
            }
            TokenType::Label => {
                if let Some(v) = lr.resolve(self.token.clean().as_str()) {
                    if signed || self.negate {
                        let (u, _) = v.force_signed(self.negate);
                        Ok(u)
                    } else {
                        Ok(v)
                    }
                } else {
                    Err(Error::new(
                        ErrorKind::Reference,
                        None,
                        format!("unresolved label \"{}\"", self.token.clean()).as_str(),
                    ))
                }
            }
            TokenType::Add | TokenType::Sub | TokenType::Star | TokenType::Div | TokenType::Mod | TokenType::Pow => {
                if let Some(left) = &self.left {
                    if let Some(right) = &self.right {
                        return self._eval_binary(lr, addr, signed, left, right);
                    }
                } else if self.token.ttype == TokenType::Star {
                    // this is a location reference; it has no child nodes
                    // use the supplied addr as the value for the special label "*"
                    return Ok(u8u16::u16(addr));
                }
                Err(Error::new(
                    ErrorKind::Syntax,
                    None,
                    format!("missing operand(s) for binary operation \"{}\"", self.token.clean()).as_str(),
                ))
            }
            _ => Err(Error::new(
                ErrorKind::Syntax,
                None,
                format!("can't evaluate \"{}\"", self.token).as_str(),
            )),
        }
    }
    fn _eval_binary(
        &self, lr: &dyn LabelResolver, addr: u16, signed: bool, left: &ValueNode, right: &ValueNode,
    ) -> Result<u8u16, Error> {
        let lhs = left.eval(lr, addr, signed)?;
        let rhs = right.eval(lr, addr, signed)?;
        // TODO: warn about overflow in these operations
        match self.token.ttype {
            TokenType::Add => {
                let (u, f) = lhs.u16().overflowing_add(rhs.u16());
                if f {
                    Err(syntax_err!("addition overflow"))
                } else {
                    Ok(u8u16::from_u16_shrink(u))
                }
            }
            TokenType::Sub => {
                let (u, f) = lhs.u16().overflowing_sub(rhs.u16());
                if f {
                    Err(syntax_err!("subtraction overflow"))
                } else {
                    Ok(u8u16::from_u16_shrink(u))
                }
            }
            TokenType::Star => {
                let (u, f) = lhs.u16().overflowing_mul(rhs.u16());
                if f {
                    Err(syntax_err!("multiplication overflow"))
                } else {
                    Ok(u8u16::from_u16_shrink(u))
                }
            }
            TokenType::Div => Ok(lhs.div(rhs)),
            TokenType::Mod => Ok(lhs.modulo(rhs)),
            TokenType::Pow => {
                let (u, f) = lhs.u16().overflowing_pow(rhs.u16() as u32);
                if f {
                    Err(syntax_err!("exponential overflow"))
                } else {
                    Ok(u8u16::from_u16_shrink(u))
                }
            }
            _ => unreachable!(),
        }
    }
}
impl Display for ValueNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let r;
        if self.left.is_some() && self.right.is_some() {
            r = write!(
                f,
                "({}{} {} {})",
                if self.negate { "-" } else { "" },
                self.left.as_ref().unwrap(),
                self.token.clean(),
                self.right.as_ref().unwrap()
            );
        } else if self.left.is_some() {
            r = write!(
                f,
                "ERROR? {} LEFT: {} ",
                self.token.clean(),
                self.left.as_ref().unwrap()
            );
        } else if self.negate {
            r = write!(f, "-{}", self.token.clean());
        } else {
            r = write!(f, "{}", self.token.clean());
        }
        r
    }
}
#[derive(Debug)]
pub enum IncDecType {
    Dec = 1,
    DecDec = 2,
    Inc = 3,
    IncInc = 4,
}

/// An OperandDescriptor is the end result of parsing an operand. The AddressingMode is
/// identified and, if there is a value associated with it, an evaluation tree for
/// the value is provided in the value field. If there are no unresolved labels then
/// value contains a ValueNode with token of TokenType::Number. Any unresolved labels are
/// listed in label_refs.
#[derive(Debug)]
pub struct OperandDescriptor {
    pub indirect: bool,
    pub mode: AddressingMode,
    pub force_mode: bool,
    pub value: Option<ValueNode>,
    pub label_refs: Option<Vec<String>>,
    pub regs: Option<Vec<String>>,
    pub incdec: Option<IncDecType>,
}
impl OperandDescriptor {
    pub fn new() -> Self {
        OperandDescriptor {
            indirect: false,
            mode: AddressingMode::Inherent,
            force_mode: false,
            value: None,
            label_refs: None,
            regs: None,
            incdec: None,
        }
    }
}
impl Display for OperandDescriptor {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<{:?}> ", self.mode,)?;
        let value = if self.value.is_some() {
            format!("{}", self.value.as_ref().unwrap())
        } else {
            "".to_string()
        };
        if self.indirect {
            write!(f, "[")?;
        }
        match self.mode {
            AddressingMode::Direct => {
                write!(f, "{}", value)?;
            }
            AddressingMode::Extended => {
                write!(f, "{}", value)?;
            }
            AddressingMode::Immediate => {
                write!(f, "{}", value)?;
            }
            AddressingMode::IncDec => {
                let reg = self.regs.as_ref().unwrap().get(0).unwrap();
                match self.incdec.as_ref().unwrap() {
                    IncDecType::Dec => write!(f, ", -{}", reg),
                    IncDecType::DecDec => write!(f, ", --{}", reg),
                    IncDecType::Inc => write!(f, ", {}+", reg),
                    IncDecType::IncInc => write!(f, ", {}++", reg),
                }?;
            }
            AddressingMode::Register => {
                let mut comma = "";
                for r in self.regs.as_ref().unwrap() {
                    write!(f, "{}{}", comma, r)?;
                    comma = ", ";
                }
            }
            AddressingMode::Offset => {
                write!(f, "{}, {}", value, self.regs.as_ref().unwrap().get(0).unwrap())?;
            }
            _ => {}
        }
        if self.indirect {
            write!(f, "]")?;
        }
        Ok(())
    }
}
/// The container for parsing methods.
pub struct Parser {
    re_registers: Regex,
}
impl Parser {
    pub fn new() -> Self {
        Parser {
            re_registers: Regex::new(r"(?i)^(A|B|CC|DP|D|PC|S|U|X|Y)$").unwrap(),
        }
    }
    /// Parses an operand and returns an OperandDescriptor on success.
    ///
    /// Grammar for operands:
    /// ```text
    ///  operand = "#" valexpr | "<" valexpr | "[" opexpr "]" | opexpr
    ///  opexpr = valexpr | valexpr, reg | reg, reg | ,incdec
    ///  valexpr (pemdas) = mulexpr [addop mulexpr]
    ///  valexpr (l-to-r) = powexpr [<addop|mulop> powexpr]
    ///  mulexpr = powexpr [mulop powexpr]
    ///  powexpr = "-" powexpr | "+" powexpr | atom ["^" powexpr]
    ///  atom = label | number | "*" | "(" valexpr ")"
    ///  number = /\d{1,5}/ | /$\x{1,4}/ | /%{1,16}/
    ///  label = /[a-zA-Z][$_a-zA-Z0-9]+/
    ///  incdec = reg "+" | reg "++" | "-" reg | "--" reg
    /// ```
    ///
    /// Note: Since labels are allowed in expressions, values can't be resolved until
    /// labels are resolved which, in turn, means that instruction lengths can't be known
    /// until labels are resolved. Thus, the assembler may require 2 or more passes. First all label
    /// definitions are found and then all label references are resolved.
    ///
    pub fn parse_operand(&self, val_str: &str) -> Result<OperandDescriptor, Error> {
        let tokens = self.tokenize(val_str)?;
        let mut token_iter = tokens.into_iter().peekable();
        let mut od = OperandDescriptor::new();
        let token = token_iter.peek();
        if token.is_none() {
            // no operand?!
            return Ok(od);
        }
        match token.unwrap().ttype {
            TokenType::Hash => {
                token_iter.next();
                // this is immediate mode
                let value = self.parse_valexpr(&mut token_iter)?;
                od.mode = AddressingMode::Immediate;
                od.value = Some(value);
            }
            TokenType::LAngle => {
                token_iter.next();
                // this is direct mode
                let value = self.parse_valexpr(&mut token_iter)?;
                od.mode = AddressingMode::Direct;
                od.value = Some(value);
            }
            TokenType::RAngle => {
                token_iter.next();
                // this is extended mode
                let value = self.parse_valexpr(&mut token_iter)?;
                od.mode = AddressingMode::Extended;
                od.force_mode = true;
                od.value = Some(value);
            }
            TokenType::LBracket => {
                // this may be "extended" mode but also might be indexed
                // in any case, it is some form of "indirect"
                od.indirect = true;
                // consume the LBracket token
                token_iter.next();
                self.parse_opexpr(&mut token_iter, &mut od)?;
                // check to make sure there is a TokenType::RBracket at the end of token_iter
                // also consume the token (whether or not it's the right one)
                if token_iter.next().filter(|t| t.ttype == TokenType::RBracket).is_none() {
                    return Err(Error::new(ErrorKind::Syntax, None, "closing ']' not found"));
                }
            }
            _ => {
                // parse_opexpr
                self.parse_opexpr(&mut token_iter, &mut od)?;
            }
        }
        if token_iter.peek().is_some() {
            // we successfully parsed the operand but found another token
            return Err(syntax_err!(format!(
                "unexpected token \"{}\"",
                token_iter.peek().unwrap().raw
            )));
        }
        Ok(od)
    }
    /// Parses an opexpr.  
    /// ```text
    ///     opexpr ::= valexpr | valexpr, reg | reg [,reg]+ | ,incdec  
    /// ```
    ///
    /// AddressingMode:  Extended |    Offset    | Register |  IncDec
    fn parse_opexpr(&self, token_iter: &mut TokenIter, od: &mut OperandDescriptor) -> Result<(), Error> {
        // look for: valexpr | valexpr, reg
        // Note: including TokenType::Sub here for negative offsets; including TokenType::Star for location reference
        if token_iter
            .peek()
            .filter(|t| {
                t.ttype == TokenType::Label
                    || t.ttype == TokenType::Number
                    || t.ttype == TokenType::Sub
                    || t.ttype == TokenType::Star
            })
            .is_some()
        {
            let value = self.parse_valexpr(token_iter)?;
            // if there are no more tokens or the next token is NOT a comma then we're done
            // note we can't consume the token yet!
            if token_iter.peek().filter(|t| t.ttype == TokenType::Comma).is_none() {
                od.value = Some(value);
                od.mode = AddressingMode::Extended;
                return Ok(());
            }
            // now that we confirmed the token is a Comma, we can consume it
            token_iter.next();
            // this should be constant offset addressing which means we need a register
            // we can consume the next token because it's an error if it isn't a register
            if let Some(reg1) = token_iter.next().filter(|t| t.ttype == TokenType::Register) {
                od.value = Some(value);
                od.mode = AddressingMode::Offset;
                od.regs = Some(vec![reg1.clean()]);
                return Ok(());
            }
            return Err(Error::new(ErrorKind::Syntax, None, "register missing after offset"));
        }
        // look for: reg [,reg]+
        if let Some(rt) = token_iter.peek().filter(|t| t.ttype == TokenType::Register) {
            // save the 1st register and consume the token
            let mut regs = Vec::new();
            regs.push(rt.clean());
            // consume register token
            token_iter.next();
            // look for [,R]
            loop {
                // look for comma
                if token_iter.peek().filter(|t| t.ttype == TokenType::Comma).is_none() {
                    // if there's no comma then we're done
                    break;
                }
                // consume the comma token
                token_iter.next();
                // grab the next register. we can consume the token since it's an error if
                // we don't find a register here.
                if let Some(reg) = token_iter.next().filter(|t| t.ttype == TokenType::Register) {
                    regs.push(reg.clean());
                } else {
                    return Err(syntax_err!("invalid or missing register"));
                }
            }
            od.mode = AddressingMode::Register;
            od.regs = Some(regs);
            return Ok(());
        }
        // look for: , reg | , -[-]reg | , reg+[+]
        if token_iter.peek().filter(|t| t.ttype == TokenType::Comma).is_some() {
            // consume the comma token
            token_iter.next();

            // look for auto-decrement
            if token_iter.peek().filter(|t| t.ttype == TokenType::Sub).is_some() {
                token_iter.next();
                od.incdec = Some(IncDecType::Dec);
                if token_iter.peek().filter(|t| t.ttype == TokenType::Sub).is_some() {
                    token_iter.next();
                    od.incdec = Some(IncDecType::DecDec);
                }
            }
            // we must find a register here so consume the token
            let reg1_opt = token_iter.next().filter(|t| t.ttype == TokenType::Register);
            if reg1_opt.is_none() {
                return Err(Error::new(
                    ErrorKind::Syntax,
                    None,
                    "register missing in \", R\" expression",
                ));
            }
            // save the register in our descriptor
            od.regs = Some(vec![reg1_opt.unwrap().clean()]);
            if token_iter.peek().filter(|t| t.ttype == TokenType::Add).is_some() {
                if od.incdec.is_some() {
                    return Err(Error::new(
                        ErrorKind::Syntax,
                        None,
                        "found both increment and decrement",
                    ));
                }
                token_iter.next();
                od.incdec = Some(IncDecType::Inc);
                if token_iter.peek().filter(|t| t.ttype == TokenType::Add).is_some() {
                    token_iter.next();
                    od.incdec = Some(IncDecType::IncInc);
                }
            }
            od.mode = if od.incdec.is_some() {
                AddressingMode::IncDec
            } else {
                AddressingMode::Offset
            };
            return Ok(());
        }
        Err(Error::new(ErrorKind::Syntax, None, "cannot parse operand"))
    }
    /// Parse a valexpr. If order of operations is PEMDAS then:
    /// ```text
    ///      valexpr ::= mulexpr [addop mulexpr]
    /// ```
    /// See parse_valexpr_lr for handling of left-to-right operation ordering.
    fn parse_valexpr(&self, token_iter: &mut TokenIter) -> Result<ValueNode, Error> {
        if !config::ARGS.pemdas {
            // order of operations is set to left-to-right instead of PEMDAS
            return self.parse_valexpr_lr(token_iter);
        }
        let mut node = self.parse_mulexpr(token_iter)?;
        while token_iter.peek().is_some() {
            match token_iter.peek().unwrap().ttype {
                TokenType::Add | TokenType::Sub => {
                    // consume the operation token and get the next mulexpr
                    let op_token = token_iter.next().unwrap();
                    let add_node = self.parse_mulexpr(token_iter)?;
                    // make a parent node for both our new nodes
                    node = ValueNode::new(op_token, false, Some(node), Some(add_node));
                }
                _ => {
                    // this function doesn't understand this token
                    // so leave it alone and return the node(s) we have
                    return Ok(node);
                }
            }
        }
        Ok(node)
    }
    /// Parse a valexpr using left-to-right order of operations.  
    /// ```text
    ///      valexpr ::= powexpr [addop|mulop powexpr]
    /// ```
    /// So it's not *strictly* left-to-right because parantheses are still honored
    /// and exponents are still evaluated before other operators, but it's close
    /// enough to support the old code bases I've found thus far.
    fn parse_valexpr_lr(&self, token_iter: &mut TokenIter) -> Result<ValueNode, Error> {
        let mut node = self.parse_mulexpr(token_iter)?;
        while token_iter.peek().is_some() {
            match token_iter.peek().unwrap().ttype {
                TokenType::Add | TokenType::Sub | TokenType::Star | TokenType::Div | TokenType::Mod => {
                    // consume the operation token and get the next powexpr
                    let op_token = token_iter.next().unwrap();
                    let right = self.parse_powexpr(token_iter)?;
                    // make a parent node for both our new nodes
                    node = ValueNode::new(op_token, false, Some(node), Some(right));
                }
                _ => {
                    // this function doesn't understand this token
                    // so leave it alone and return the node(s) we have
                    return Ok(node);
                }
            }
        }
        Ok(node)
    }
    /// Parse a mulexpr.  
    /// ```text
    ///     mulexpr ::= powexpr [mulop powexpr]
    /// ```
    fn parse_mulexpr(&self, token_iter: &mut TokenIter) -> Result<ValueNode, Error> {
        let mut left = self.parse_powexpr(token_iter)?;
        while token_iter.peek().is_some() {
            match token_iter.peek().unwrap().ttype {
                TokenType::Star | TokenType::Div | TokenType::Mod => {
                    // consume the operation token and get the next mulexpr
                    let op_token = token_iter.next().unwrap();
                    // if the operator is an asterisk and there are no more tokens then assume this is a comment
                    if op_token.ttype == TokenType::Star && token_iter.peek().is_none() {
                        return Ok(left);
                    }
                    let right = self.parse_powexpr(token_iter)?;
                    // make a parent node for both our new nodes
                    left = ValueNode::new(op_token, false, Some(left), Some(right));
                }
                _ => {
                    // this function doesn't understand this token
                    return Ok(left);
                }
            }
        }
        Ok(left)
    }
    /// Parse a powexpr.  
    /// ```text
    ///     powexpr ::= "-" powexpr | "+" powexpr | atom ["^" powexpr]
    /// ```
    fn parse_powexpr(&self, token_iter: &mut TokenIter) -> Result<ValueNode, Error> {
        // make sure we have tokens to parse
        if token_iter.peek().is_none() {
            return Err(Error::new(ErrorKind::Syntax, None, "missing number or label"));
        }
        let mut negate = false;
        // is there a sign designator (+ or -) before the atom?
        if token_iter
            .peek()
            .filter(|t| t.ttype == TokenType::Sub || t.ttype == TokenType::Add)
            .is_some()
        {
            // consume any number of + or - tokens and keep track of the sign
            while token_iter
                .peek()
                .filter(|t| t.ttype == TokenType::Sub || t.ttype == TokenType::Add)
                .is_some()
            {
                if token_iter.next().unwrap() == TokenType::Sub {
                    negate = !negate;
                };
            }
            // now we can finally get the powexpr
            let mut node = self.parse_powexpr(token_iter)?;
            node.negate = xor!(node.negate, negate);
            return Ok(node);
        }
        // if we get here then we expect to parse an atom
        let left = self.parse_atom(token_iter)?;
        // does it have an exponent?
        if token_iter.peek().filter(|t| t.ttype == TokenType::Pow).is_none() {
            // no exponent. we're done.
            return Ok(left);
        }
        // consume the Pow token
        let pow_token = token_iter.next().unwrap();
        // get exponent node
        let right = self.parse_powexpr(token_iter)?;
        // return a Pow node
        Ok(ValueNode::new(pow_token, false, Some(left), Some(right)))
    }
    /// Parse an atom.
    /// ```text
    ///     atom ::= label | number | '*' | '(' valexpr ')'
    /// ```
    fn parse_atom(&self, token_iter: &mut TokenIter) -> Result<ValueNode, Error> {
        if let Some(token) = token_iter.next() {
            match token.ttype {
                TokenType::Number | TokenType::Label | TokenType::Star => {
                    return Ok(ValueNode::new(token, false, None, None));
                }
                TokenType::LParen => {
                    let node = self.parse_valexpr(token_iter)?;
                    if let Some(rparen) = token_iter.next() {
                        if rparen.ttype == TokenType::RParen {
                            return Ok(node);
                        }
                    }
                    return Err(syntax_err!("missing closing parenthesis"));
                }
                _ => {}
            }
        }
        Err(Error::new(ErrorKind::Syntax, None, "missing label or value"))
    }
    pub fn str_to_value_node(&self, expr: &str) -> Result<ValueNode, Error> {
        let tokens = self.tokenize(expr)?;
        self.parse_valexpr(&mut tokens.into_iter().peekable())
    }
    /// Parse a string for a test criterion and populate the given TestCriterion object.
    /// ErrorKind::Reference is returned when unresolved labels are encountered
    ///
    pub fn parse_test_criterion(&self, tc: &mut TestCriterion, lr: &dyn LabelResolver) -> Result<(), Error> {
        let mut tokens = self.tokenize(&tc.lhs_src)?;
        let mut token_iter = tokens.into_iter().peekable();
        // try to get the lhs; start by looking for a register
        if token_iter.peek().filter(|t| t.ttype == TokenType::Register).is_some() {
            // consume the register token
            let reg = token_iter.next().unwrap();
            tc.lhs = Some(RegOrAddr::Reg(registers::Name::from_str(&reg.clean())));
        } else if let Ok(node) = self.parse_valexpr(&mut token_iter) {
            // value token(s) consumed by parse_valexpr; now evaluate and store the addr
            // Note: test criteria cannot use location reference (obviously, right?)
            let addr = node.eval(lr, 0, true)?;
            tc.lhs = Some(RegOrAddr::Addr(addr.u16()));
        } else {
            return Err(syntax_err!(
                format!("Invalid LHS \"{}\" in test criterion", &tc.lhs_src).as_str()
            ));
        }
        let mut rhs_is_value = false;
        tokens = self.tokenize(&tc.rhs_src)?;
        token_iter = tokens.into_iter().peekable();
        // get the rhs; start by looking for '#'
        if token_iter.peek().filter(|t| t.ttype == TokenType::Hash).is_some() {
            // found "#" token; consume it; rhs is a value
            token_iter.next();
            rhs_is_value = true;
        }
        // parse and evaluate the rhs
        let node = self.parse_valexpr(&mut token_iter)?;
        let val = node.eval(lr, 0, true)?;
        if rhs_is_value {
            tc.rhs = Some(AddrOrVal::Val(val));
        } else {
            tc.rhs = Some(AddrOrVal::Addr(val.u16()));
        }
        Ok(())
    }

    /// Tokenize the given string and return a Vec<Token>.
    fn tokenize(&self, input: &str) -> Result<Vec<Token>, Error> {
        let mut chars = input.chars();
        let mut current = chars.next();
        let mut err_msg: Option<String> = None;
        let mut output = Vec::new();
        while let Some(ch) = current {
            if ch.is_whitespace() {
                // Give up if we hit any whitespace.
                //    Old assemblers didn't support any whitespace within operands.
                //    e.g., an expresssion like "(128 - 32)" would have produced an error.
                //    And it was common to write comments delimited by only a single space
                //    after the operand like this:
                //    "LABEL: LDA ,X This is a comment"
                //    So we just provide that same behavior here.
                break;
            }
            match ch.to_ascii_lowercase() {
                '%' => {
                    current = chars.next();
                    if current.is_none() {
                        err_msg = Some("unexpected end of string following `%`".to_string());
                        break;
                    }
                    if !current.unwrap().is_digit(2) {
                        output.push(Token::new(TokenType::Mod, "%".to_string(), None));
                    } else {
                        let r = self.get_number_from_binary(&mut current, &mut chars);
                        if r.is_err() {
                            err_msg = r.err();
                            break;
                        }
                        output.push(r.unwrap());
                    }
                }
                '$' => {
                    current = chars.next();
                    if current.is_none() {
                        err_msg = Some("unexpected end of string following `$`".to_string());
                        break;
                    }
                    let r = self.get_number_from_hex(&mut current, &mut chars);
                    if r.is_err() {
                        err_msg = r.err();
                        break;
                    }
                    output.push(r.unwrap());
                }
                '\'' => {
                    current = chars.next();
                    // if the previous token was a number then assume this is a closing quote and ignore it
                    // For example, the programmer may have written #'C' instead of just #'C
                    if output.is_empty() || output[output.len() - 1].ttype != TokenType::Number {
                        // otherwise, we need to parse a char here and convert it to a number
                        if current.is_none() {
                            err_msg = Some("unexpected end of string following `'`".to_string());
                            break;
                        }
                        let r = self.get_number_from_char(&mut current, &mut chars);
                        if r.is_err() {
                            err_msg = r.err();
                            break;
                        }
                        output.push(r.unwrap());
                    }
                }
                '0'..='9' => {
                    let r = self.get_number_from_decimal(&mut current, &mut chars);
                    if r.is_err() {
                        err_msg = r.err();
                        break;
                    }
                    output.push(r.unwrap());
                }
                'a'..='z' => {
                    let r = self.get_label_or_register(&mut current, &mut chars);
                    if r.is_err() {
                        err_msg = r.err();
                        break;
                    }
                    output.push(r.unwrap());
                }
                '+' => {
                    output.push(Token::new(TokenType::Add, "+".to_string(), None));
                    current = chars.next();
                }
                '-' => {
                    output.push(Token::new(TokenType::Sub, "-".to_string(), None));
                    current = chars.next();
                }
                '*' => {
                    output.push(Token::new(TokenType::Star, "*".to_string(), None));
                    current = chars.next();
                }
                '/' => {
                    output.push(Token::new(TokenType::Div, "/".to_string(), None));
                    current = chars.next();
                }
                '^' => {
                    output.push(Token::new(TokenType::Pow, "^".to_string(), None));
                    current = chars.next();
                }
                '#' => {
                    output.push(Token::new(TokenType::Hash, "#".to_string(), None));
                    current = chars.next();
                }
                ',' => {
                    output.push(Token::new(TokenType::Comma, ",".to_string(), None));
                    current = chars.next();
                }
                '[' => {
                    output.push(Token::new(TokenType::LBracket, "[".to_string(), None));
                    current = chars.next();
                }
                ']' => {
                    output.push(Token::new(TokenType::RBracket, "]".to_string(), None));
                    current = chars.next();
                }
                '<' => {
                    output.push(Token::new(TokenType::LAngle, "<".to_string(), None));
                    current = chars.next();
                }
                '>' => {
                    output.push(Token::new(TokenType::RAngle, ">".to_string(), None));
                    current = chars.next();
                }
                '(' => {
                    output.push(Token::new(TokenType::LParen, "(".to_string(), None));
                    current = chars.next();
                }
                ')' => {
                    output.push(Token::new(TokenType::RParen, ")".to_string(), None));
                    current = chars.next();
                }
                _ => {
                    err_msg = Some(format!("unexpected character '{}'", current.unwrap()));
                    break;
                }
            }
        }
        if let Some(err_msg) = err_msg {
            Err(Error::new(ErrorKind::Syntax, None, err_msg.as_str()))
        } else {
            Ok(output)
        }
    }
    fn get_number_from_binary(&self, current: &mut Option<char>, chars: &mut Chars) -> Result<Token, String> {
        let mut num: u16 = 0;
        let mut digit_count = 0;
        let mut raw = "%".to_string();
        while let Some(c) = current {
            let digit = c.to_digit(2);
            if digit.is_none() {
                if digit_count == 0 {
                    return Err("missing binary digit(s)".to_string());
                }
                break;
            }
            digit_count += 1;
            raw.push(*c);
            if digit_count > 16 {
                return Err("binary constant too large".to_string());
            }
            num = (num << 1) + digit.unwrap() as u16;
            *current = chars.next();
        }
        let u = if digit_count > 8 {
            u8u16::u16(num)
        } else {
            u8u16::u8(num as u8)
        };
        Ok(Token::new(TokenType::Number, raw, Some(u)))
    }
    fn get_number_from_hex(&self, current: &mut Option<char>, chars: &mut Chars) -> Result<Token, String> {
        let mut num: u16 = 0;
        let mut digit_count = 0;
        let mut raw = "$".to_string();
        let mut negative = false;
        while let Some(c) = current {
            let digit = c.to_digit(16);
            if digit.is_none() {
                if digit_count == 0 {
                    match *c {
                        '-' => {
                            negative = !negative;
                            *current = chars.next();
                            raw.push('-');
                            continue;
                        }
                        '\'' => {
                            *current = chars.next();
                            if current.is_some() {
                                return self.get_number_from_char(current, chars);
                            }
                            return Err("missing character constant".to_string());
                        }
                        _ => return Err("missing hexadecimal digit(s)".to_string()),
                    }
                }
                break;
            }
            digit_count += 1;
            raw.push(*c);
            if digit_count > 4 {
                return Err("hexadecimal constant too large".to_string());
            }
            num = (num << 4) + digit.unwrap() as u16;
            *current = chars.next();
        }
        let mut u = if digit_count > 2 {
            u8u16::u16(num)
        } else {
            u8u16::u8(num as u8)
        };
        if negative {
            let (v, _) = u.force_signed(negative);
            u = v;
        }
        Ok(Token::new(TokenType::Number, raw, Some(u)))
    }
    fn get_number_from_decimal(&self, current: &mut Option<char>, chars: &mut Chars) -> Result<Token, String> {
        let mut num: u32 = 0;
        let mut raw = String::new();
        while let Some(c) = current {
            let digit = c.to_digit(10);
            if digit.is_none() {
                break;
            }
            num = (num * 10) + digit.unwrap();
            raw.push(*c);
            if num > 0xffff {
                return Err("decimal constant too large".to_string());
            }
            *current = chars.next();
        }
        let u = if num > 255 {
            u8u16::u16(num as u16)
        } else {
            u8u16::u8(num as u8)
        };
        Ok(Token::new(TokenType::Number, raw, Some(u)))
    }
    fn get_number_from_char(&self, current: &mut Option<char>, chars: &mut Chars) -> Result<Token, String> {
        let ch = current.unwrap();
        *current = chars.next();
        Ok(Token::new(TokenType::Number, ch.to_string(), Some(u8u16::u8(ch as u8))))
    }
    fn get_label_or_register(&self, current: &mut Option<char>, chars: &mut Chars) -> Result<Token, String> {
        let mut raw = String::new();
        while let Some(ref ch) = current {
            if !ch.is_ascii_digit() && !ch.is_alphabetic() && !ch.eq(&'_') && !ch.eq(&'$') {
                break;
            }
            raw.push(*ch);
            *current = chars.next();
        }
        let ttype = if self.re_registers.is_match(raw.as_str()) {
            TokenType::Register
        } else {
            TokenType::Label
        };
        Ok(Token::new(ttype, raw, None))
    }
}
