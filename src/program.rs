use crate::hex::{HexRecord, HexRecordCollection};

use super::obj::*;
use super::parse::{LabelResolver, ValueNode};
use super::test::TestCriterion;
use super::*;

use lazy_static::lazy_static;
use regex::Regex;
use std::collections::BTreeMap;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;
lazy_static! {
    static ref RE_PARAM: Regex = Regex::new(r"[@](\d+)").unwrap();
}

#[derive(Debug)]
pub enum DirectPage {
    Zero,
    Value(ValueNode),
    None,
}
use std::sync::Mutex;
lazy_static! {
    pub static ref DP_LIST: Mutex<Vec<DirectPage>> = Mutex::new(Vec::new());
}
impl DirectPage {
    pub fn is_zero(&self) -> bool { matches!(self, DirectPage::Zero) }
    pub fn is_none(&self) -> bool { matches!(self, DirectPage::None) }
    pub fn __matches_value_node(&self, other: &ValueNode, lr: &dyn LabelResolver, addr: u16) -> bool {
        if let Ok(rhs) = other.eval(lr, addr, false) {
            return self.__matches_value(rhs, lr, addr);
        }
        false
    }
    pub fn __matches_value(&self, value: u8u16, lr: &dyn LabelResolver, addr: u16) -> bool {
        let v = value.msb().unwrap_or(0u8);
        match self {
            DirectPage::None => false,
            DirectPage::Zero => v == 0,
            DirectPage::Value(n) => n
                .eval(lr, addr, false)
                .ok()
                .map(|u| u.msb().unwrap_or(0) == v)
                .unwrap_or(false),
        }
    }
    pub fn add(dp: DirectPage) { DP_LIST.lock().unwrap().push(dp) }
    pub fn current_index() -> usize { DP_LIST.lock().unwrap().len() - 1 }
    pub fn matches_value(dpi: usize, value: u8u16, lr: &dyn LabelResolver, addr: u16) -> bool {
        DP_LIST
            .lock()
            .unwrap()
            .get(dpi)
            .expect("invalid DP index?!")
            .__matches_value(value, lr, addr)
    }
    pub fn reset() {
        let mut list = DP_LIST.lock().unwrap();
        list.clear();
        list.push(DirectPage::Zero);
    }
}

#[derive(Debug)]
struct MacroLineSegment {
    pub s: String,        // the text fragment
    pub n: Option<usize>, // the index of the arg that should be prepended to this fragment
}
impl MacroLineSegment {
    pub fn new(raw_fragment: &str) -> Result<Self, Error> {
        let mut n = 0usize;
        let mut i = 0usize;
        for c in raw_fragment.chars() {
            match c {
                '0'..='9' => n = n * 10 + c.to_digit(10).unwrap() as usize,
                c if c.is_whitespace() => break,
                _ => return Err(syntax_err!("invalid macro parameter")),
            }
            i += 1;
        }
        let (_, r) = raw_fragment.split_at(i);
        Ok(MacroLineSegment {
            s: r.to_string(),
            n: if i > 0 { Some(n) } else { None },
        })
    }
    pub fn hydrate(&self, args: &[&str]) -> Result<String, Error> {
        match self.n {
            Some(n) => {
                if n > args.len() {
                    Err(syntax_err!("macro arg index out of bounds"))
                } else {
                    Ok(format!("{}{}", args[n], self.s))
                }
            }
            None => Ok(self.s.to_string()),
        }
    }
}
#[derive(Debug)]
pub struct Macro {
    pub name: String,                  // name assigned to macro by programmer
    pub _src_line_num: usize,          // line in the source on which macro defn begins
    pub arg_count: usize,              // number of args required by macro
    lines: Vec<Vec<MacroLineSegment>>, // the non-empty lines of the macro (excluding .macro and .endm lines)
}
impl Macro {
    pub fn new(name: &str, line: usize) -> Self {
        Macro {
            name: name.to_string(),
            _src_line_num: line,
            arg_count: 0,
            lines: Vec::new(),
        }
    }
    pub fn add_line(&mut self, line: &str) -> Result<(), Error> {
        let s = line.split('@');
        let mut v: Vec<MacroLineSegment> = Vec::new();
        for (i, raw) in s.enumerate() {
            let f = MacroLineSegment::new(raw)?;
            if let Some(n) = f.n {
                if n >= self.arg_count {
                    self.arg_count = n + 1
                }
            } else if i > 0 {
                return Err(syntax_err!("missing macro parameter number"));
            }
            v.push(f);
        }
        self.lines.push(v);
        Ok(())
    }
    pub fn hydrate_instance(&self, args: Vec<&str>) -> Result<Vec<String>, Error> {
        let mut m = Vec::new();
        if args.len() != self.arg_count {
            return Err(syntax_err!(format!("wrong number of args for macro \"{}\"", self.name)));
        }
        for lsv in self.lines.iter() {
            m.push(
                lsv.iter()
                    .map(|lf| lf.hydrate(&args))
                    .collect::<Result<Vec<String>, Error>>()?
                    .concat(),
            );
        }
        Ok(m)
    }
}

#[derive(Debug)]
pub struct ProgramLine {
    pub _prog_line_num: usize,     // line number in program
    pub src_line_num: usize,       // corresponding line number in source
    pub src: String,               // verbatim line from source
    pub label: Option<String>,     // label defined on this line
    pub operation: Option<String>, // operation (mnemonic or directive) used on this line
    pub operand: Option<String>,   // operand used on this line
    pub obj: Option<Box<dyn ObjectProducer>>,
    pub obj_size: u16, // keep track of object size between passes
    pub addr: u16,     // the program address corresponding to this line (whether the line produces an object or not)
}
impl ProgramLine {
    pub fn get_label(&self) -> &str { self.label.as_ref().map_or("", String::as_str) }
    pub fn get_operation(&self) -> &str { self.operation.as_ref().map_or("", String::as_str) }
    pub fn get_operand(&self) -> &str { self.operand.as_ref().map_or("", String::as_str) }
    pub fn is_inert(&self) -> bool { self.label.is_none() && self.operation.is_none() }
}
impl fmt::Display for ProgramLine {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_inert() {
            write!(f, "{}", &self.src)
        } else {
            write!(
                f,
                "{:8} {:8} {}",
                self.get_label(),
                self.get_operation(),
                self.get_operand(),
            )
        }
    }
}
#[derive(Debug)]
pub struct Label {
    pub name: String,         // name of label; Note: these are case sensitive!
    pub line: usize,          // line number where the label is defined
    addr: u16,                // address (location) of this label
    node: Option<ValueNode>,  // if this label is defined by EQU then it has a ValueNode
    pub _refs: Vec<usize>,    // the lines that reference this label
    val_cache: Option<u8u16>, // cache of last value received from node.eval()
}
impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let val = self.node.as_ref().map_or("<n/a>".to_string(), |v| format!("{}", v));
        write!(f, "{:04} {:04x} {:10} {:4}", self.line, self.addr, self.name, val)
    }
}

#[derive(Debug)]
pub struct ProgramLabels {
    map: HashMap<String, Label>,
}
impl LabelResolver for ProgramLabels {
    fn resolve(&self, label: &str) -> Option<u8u16> { self.get_value(label) }
}
impl ProgramLabels {
    pub fn new() -> ProgramLabels { ProgramLabels { map: HashMap::new() } }
    pub fn dump(&self) {
        if self.map.is_empty() {
            println!("No symbols.")
        } else {
            let mut labels: Vec<_> = self.map.values().collect();
            labels.sort_by(|&l1, &l2| l1.line.cmp(&l2.line));
            println!("{} symbols defined/referenced:", self.map.len());
            println!(blue!("{:4} {:4} {:10} {:4}"), "LINE", "ADDR", "LABEL", "VAL");
            for label in labels {
                println!("{}", label);
            }
        }
    }
    pub fn new_definition(&mut self, name: &str, line: usize, addr: u16, node: Option<ValueNode>) -> Result<(), Error> {
        if self.map.contains_key(name) {
            return Err(Error::new(
                ErrorKind::Syntax,
                None,
                format!("Duplicate label \"{}\"", name).as_str(),
            ));
        }
        let label = Label {
            name: name.to_string(),
            line,
            addr,
            node,
            _refs: Vec::new(),
            val_cache: None,
        };
        self.map.insert(label.name.clone(), label);
        Ok(())
    }

    pub fn set_address(&mut self, name: &str, new_addr: u16) -> Result<u16, Error> {
        if let Some(label) = self.map.get_mut(name) {
            // Note: NOT checking for overwriting an existing value!
            let old_addr = label.addr;
            label.addr = new_addr;
            Ok(old_addr)
        } else {
            Err(Error::new(
                ErrorKind::General,
                None,
                format!("Cannot set address for undefined label \"{}\"", name).as_str(),
            ))
        }
    }
    pub fn get_value(&self, name: &str) -> Option<u8u16> {
        // if a label has a ValueNode then its value is defined as the .eval of that node
        // otherwise, the value of the label is its address
        if let Some(label) = self.map.get(name) {
            if let Some(node) = label.node.as_ref() {
                node.eval(self, label.addr, false).ok()
            } else {
                Some(u8u16::u16(label.addr))
            }
        } else {
            None
        }
    }
    pub fn set_node(&mut self, name: &str, node: ValueNode) -> Result<(), Error> {
        if let Some(label) = self.map.get_mut(name) {
            // Note: NOT checking for overwriting an existing node!
            label.node = Some(node);
            Ok(())
        } else {
            Err(syntax_err!(
                format!("Cannot set node for undefined label \"{}\"", name).as_str()
            ))
        }
    }
    pub fn eval_all_nodes(&mut self) -> Result<usize, Error> {
        let mut unresolved_count = self.map.len();
        let mut changes = 0usize;
        loop {
            let (latest_changes, still_unresolved) = self._try_eval_all_nodes();
            changes += latest_changes;
            if still_unresolved == 0 {
                break;
            }
            if still_unresolved >= unresolved_count {
                self.dump();
                return Err(syntax_err!("unabled to resolve labels; recursive definition?"));
            }
            unresolved_count = still_unresolved;
        }
        Ok(changes)
    }
    fn _try_eval_all_nodes(&mut self) -> (usize, usize) {
        let mut unresolved = 0usize;
        let mut changes = 0usize;
        // do some unsafe nonsense to get around the borrow checker below
        let lr: &dyn LabelResolver;
        unsafe {
            let s = self as *const dyn LabelResolver;
            lr = &*s as &dyn LabelResolver;
        }
        for label in self.map.values_mut() {
            if let Some(node) = label.node.as_ref() {
                if let Ok(val) = node.eval(lr, label.addr, true) {
                    changes += label.val_cache.map_or(1, |v| if v == val { 0 } else { 1 });
                    label.val_cache = Some(val);
                } else {
                    unresolved += 1;
                }
            }
        }
        (changes, unresolved)
    }
    // the provided range (left,right) is *inclusive*
    pub fn adjust_label_addresses(&mut self, (left, right): (u16, u16), delta: i16) {
        for label in self.map.values_mut() {
            // is its value in the given range?
            if label.addr >= left && label.addr <= right {
                // adjust the value by the given delta
                let (new_addr, _) = label.addr.overflowing_add(delta as u16);
                label.addr = new_addr;
            }
        }
    }
}
#[derive(Debug)]
pub struct ProgramSegment {}
#[derive(Debug)]
pub struct ProgramSegments {
    pub map: BTreeMap<u16, ProgramSegment>,
}
impl ProgramSegments {
    pub fn new() -> Self {
        let mut segs = ProgramSegments { map: BTreeMap::new() };
        // we default to having a segment starting at 0
        _ = segs.add(0u16);
        segs
    }
    pub fn add(&mut self, addr: u16) -> Result<(), Error> {
        if self.map.insert(addr, ProgramSegment {}).is_some() && addr != 0u16 {
            // we allow duplicate segments at address 0 (because of our default segment there)
            // but flag other duplicates as errors
            return Err(syntax_err!(
                format!("duplicate segment at address {:04x}", addr).as_str()
            ));
        }
        Ok(())
    }

    pub fn segment_containing_address(&self, addr: u16) -> (u16, u16) {
        let mut left = 0u16;
        let mut right = 0xffffu16;
        for &seg_start in self.map.keys() {
            if addr >= seg_start {
                left = seg_start;
            } else if addr < seg_start {
                right = seg_start - 1;
                break;
            }
        }
        (left, right)
    }
}

#[derive(Debug)]
pub struct Program {
    pub addr: u16,                       // current address
    pub line_number: usize,              // current line number
    pub lines: Vec<ProgramLine>,         // program lines
    pub labels: ProgramLabels,           // all labels
    pub _macros: HashMap<String, Macro>, // all macros
    pub results: Vec<TestCriterion>,     // expected results for test criteria
    pub segs: ProgramSegments,           // program segments (defined by ORG directive)
}
impl LabelResolver for Program {
    fn resolve(&self, label: &str) -> Option<u8u16> { self.labels.get_value(label) }
}
impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(
            f,
            "Program: {} lines, {} labels, {} segments",
            self.line_number,
            self.labels.map.len(),
            self.segs.map.len(),
        )
    }
}
impl Program {
    pub fn new(lines: Vec<ProgramLine>, macros: HashMap<String, Macro>) -> Self {
        DirectPage::reset();
        Program {
            addr: 0,
            line_number: 0,
            lines,
            labels: ProgramLabels::new(),
            _macros: macros,
            results: Vec::new(),
            segs: ProgramSegments::new(),
        }
    }
    pub fn write_listing(&self, f: &mut dyn io::Write) -> Result<(), io::Error> {
        for line in &self.lines {
            if config::ARGS.code_only && line.is_inert() {
                continue;
            }
            write!(f, "{:4} ", line.src_line_num)?;
            if let Some(bob) = line.obj.as_ref().and_then(|op| op.bob_ref()) {
                write!(f, "{:28} ", bob)?;
            } else if let Some(op) = line.obj.as_ref() {
                write!(f, "{:28} ", op)?;
            } else {
                write!(f, "{:28} ", format!("{:04X}", line.addr),)?;
            }
            writeln!(f, " {line}")?;
        }
        Ok(())
    }
    pub fn write_output_files(&self, parent_filename: &str) -> Result<(), Error> {
        let path = Path::new(parent_filename);
        let basename = path
            .file_stem()
            .and_then(|s| s.to_str())
            .ok_or_else(|| general_err!("bad filename"))?;
        let mut pb = path.to_path_buf();
        pb.set_file_name(basename);
        // write out the listing file
        pb.set_extension("lst");
        let mut file = File::create(&pb)?;
        self.write_listing(&mut file)?;
        println!("wrote listing file: {}", pb.display());
        // now symbols...
        // first create a collection of (name,addr) label tuples
        let mut labels: Vec<(&String, u16)> = self.labels.map.iter().map(|(s, l)| (s, l.addr)).collect();
        // sort them by address
        labels.sort_by(|a, b| a.1.cmp(&b.1));
        // now try to write them out to a *.sym file
        pb.set_extension("sym");
        file = File::create(&pb)?;
        for label in labels {
            writeln!(file, "{:04X},{}", label.1, label.0)?;
        }
        println!("wrote symbol file: {}", pb.display());
        // now the binary...
        let mut hf = HexRecordCollection::new();
        const MAX_DATA: usize = 32;
        let mut addr = 16;
        let mut buf = [0u8; MAX_DATA + 1];
        let mut i = 0;
        for line in &self.lines {
            if let Some(bob) = line.obj.as_ref().and_then(|o| o.bob_ref()) {
                if bob.data.is_some() && bob.addr as usize != addr as usize + i {
                    // discontinguous object; write previous record (if any)
                    if i > 0 {
                        hf.add_record(HexRecord::from_data(addr, &buf[0..i]))?;
                        i = 0;
                    }
                    addr = bob.addr;
                }
                if let Some(data) = &bob.data {
                    // this object contains some data; write it into our buffer
                    for u in data.iter() {
                        if let Some(b) = u.msb() {
                            buf[i] = b;
                            i += 1;
                        }
                        buf[i] = u.lsb();
                        i += 1;
                        if i >= MAX_DATA {
                            // we have accumulated a full record's worth of data; fill a record and add it to our collection
                            hf.add_record(HexRecord::from_data(addr, &buf[0..MAX_DATA]))?;
                            (addr, _) = addr.overflowing_add(MAX_DATA as u16);
                            if i > MAX_DATA {
                                buf[0] = buf[MAX_DATA];
                                i = 1
                            } else {
                                i = 0
                            }
                        }
                    }
                }
            }
        }
        if i > 0 {
            // we have some data remaining in the buffer; write it to a record
            hf.add_record(HexRecord::from_data(addr, &buf[0..i]))?;
        }
        // add an EOF record to the collection
        hf.add_eof();
        // write out the *.hex file
        pb.set_extension("hex");
        file = File::create(&pb)?;
        hf.write_to_file(&mut file)?;
        println!("wrote hex (binary) file: {}", pb.display());
        Ok(())
    }
}
