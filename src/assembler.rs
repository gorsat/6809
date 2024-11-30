//! Building a 6809 assembly language program is a multi-step process:  
//!
//!  1. Identify all label definitions and references
//!  2. Identify binary objects and their max sizes
//!  3. Using objects' (estimated or calculated) sizes, resolve all labels to addresses
//!  4. Using resolved labels, build objects and assign addresses to them
//!
//! \[_repeat steps 3 and 4 until no size or location changes occur_\]
//!
//! The size of individual instructions can change through this process due
//! to movement of other labels/objects. For instance, if an instruction was
//! written as a relative branch but the location it branches to moves to
//! more than 127 bytes away, then the relative branch will be converted to an
//! absolute (long) branch which adds a byte to its size. This change may, in
//! turn, cause some other instruction to change, and so on.
//!   
//! This module along with parse.rs provide most of the work required to translate
//! from assembly language to machine code.
use super::obj::*;
use super::parse::{OperandDescriptor, Parser};
use super::pathid::*;
use super::test::TestCriterion;
use super::*;

use regex::Regex;
use std::fs::File;
use std::io::{self, BufRead};
use std::path::PathBuf;

/// The container for our assembler methods.
pub struct Assembler {
    parser: Parser,
    re_result_line: Regex,           // matches test criterion
    re_comment_or_blank_line: Regex, // matches a line that is blank or only contains a comment
    re_statement: Regex, // matches a generic assembly statement line ([label] operation [operand [comment]])
    re_macro_args: Regex, // matches a comma delimited list of parameters for a macro
}
impl Assembler {
    pub fn new() -> Assembler {
        instructions::init();
        Assembler {
            parser: Parser::new(),
            re_result_line: Regex::new(r"^\s*;![ \t]*([^\s]+)[ \t]*=[ \t]*([^\s]+)[ \t]*.*$").unwrap(),
            re_comment_or_blank_line: Regex::new(r"^(?:[ \t]*[*;].*)|^[ \t]*$").unwrap(),
            re_macro_args: Regex::new(r"^(?:(?:[^\s,;*]+)(?:(?:[,][ ]*)(?:[^\s,]+))*)").unwrap(),
            re_statement: Regex::new(
                r"(?i)^(?:([$._a-z0-9]{1,15})[:]?)?(?:(?:[ \t]+([^\s;*]+))?(?:[ \t]+(?:([^\s].*)|.*))?)?$",
            )
            .unwrap(),
        }
    }

    /*
    TODO: Support for INC (include) directive
    - global src_files: Vec of filenames where index in src_files is the src_file_id
    - every line in a program and every macro definition includes src_file_id along with src_line_num
    - stack of src_file_id we use to check for INC recursion
    - load_asm_file(prog_lines: &mut Vec<ProgramLine>, macros: &mut HashMap<)
    - error reporting needs to provide the relevant file name and line number(s)
    */
    fn load_asm_file(
        &self, file_name: &str, path_stack: &mut Vec<PathBuf>, prog_lines: &mut Vec<ProgramLine>,
        macros: &mut HashMap<String, Macro>,
    ) -> Result<(), Error> {
        // canonicalize the path based on the previous file (if there was one)
        let path = path_stack
            .last()
            .map_or_else(
                || PathBuf::from(file_name),
                |pb| pb.to_owned().with_file_name(file_name),
            )
            .canonicalize()?;
        info!("Loading file: {}", path.display());
        // make sure we don't recurse via inclusion
        if path_stack.contains(&path) {
            return Err(Error::new(
                ErrorKind::Recursion,
                None,
                &format!(
                    ".include file recursion: file \"{}\" included file \"{}\"",
                    path_stack.last().unwrap().file_name().unwrap().to_string_lossy(),
                    file_name
                ),
            ));
        }
        let src = io::BufReader::new(File::open(&path)?)
            .lines()
            .collect::<Result<Vec<String>, io::Error>>()?;
        let src_file_id = create_id_for_path(&path);
        path_stack.push(path);
        let mut mo: Option<Macro> = None;
        let mut src_line_num = 0usize;
        let add_line = |pls: &mut Vec<ProgramLine>,
                        src_line_num: usize,
                        src: String,
                        label: Option<String>,
                        operation: Option<String>,
                        operand: Option<String>| {
            let pl = ProgramLine {
                _prog_line_num: pls.len() + 1,
                src_file_id,
                src_line_num,
                src,
                label,
                operation,
                operand,
                obj: None,
                obj_size: 0,
                addr: 0,
            };
            pls.push(pl);
        };
        // read each line of the program and process any macro definitions and expansions along the way
        for line in src {
            src_line_num += 1;
            // let line = line.into();
            let mut label = None;
            let mut operation = None;
            let mut operand = None;
            if let Some(statement) = self.re_statement.captures(line.as_str()) {
                // the source line looks like a statement; try to extract the fields
                label = statement.get(1).map(|s| s.as_str().to_string());
                // operations (including macro names!) are case insensitive
                operation = statement.get(2).map(|m| m.as_str().to_ascii_uppercase());
                operand = statement.get(3).map(|s| s.as_str().to_string());
            }
            if operation.as_deref() == Some(".INCLUDE") {
                // found an ".include" line
                if mo.is_some() {
                    return Err(syntax_err_line!(
                        src_file_id,
                        src_line_num,
                        ".INCLUDE not allowed in macro"
                    ));
                }
                if let Some(file) = operand {
                    // try to load the file
                    self.load_asm_file(&file, path_stack, prog_lines, macros)?;
                    continue;
                }
                // no filename found for .include
                return Err(syntax_err_line!(
                    src_file_id,
                    src_line_num,
                    "no file specified for .INCLUDE"
                ));
            }
            if operation.as_deref() == Some(".MACRO") {
                // found a ".macro" (begin macro defn) statement
                if mo.is_some() {
                    return Err(syntax_err_line!(src_file_id, src_line_num, "illegal nested macro"));
                }
                // get the macro's name (case insensitive!)
                if let Some(name) = operand.map(|s| s.to_ascii_uppercase()) {
                    // make sure the name hasn't already been used
                    if macros.contains_key(&name) {
                        return Err(syntax_err_line!(
                            src_file_id,
                            src_line_num,
                            format!("duplicate definition of macro \"{}\"", &name)
                        ));
                    }
                    // create a new Macro object and hold it in the mo Option
                    mo = Some(Macro::new(&name, src_file_id, src_line_num));
                    add_line(prog_lines, src_line_num, format!("; {}", &line), None, None, None);
                    continue;
                }
                // no name found for this macro
                return Err(syntax_err_line!(src_file_id, src_line_num, "missing macro name"));
            }
            if operation.as_deref() == Some(".ENDM") {
                // found a ".endm" (end macro defn) statement; add completed macro
                if let Some(m) = mo {
                    mo = None;
                    macros.insert(m.name.clone(), m);
                } else {
                    return Err(syntax_err_line!(src_file_id, src_line_num, "invalid macro end"));
                }
                add_line(prog_lines, src_line_num, format!("; {}", &line), None, None, None);
                continue;
            }
            if let Some(mut m) = mo {
                // we're in a macro definition; add this line to the macro
                m.add_line(&line)
                    .map_err(|e| line_err!(src_file_id, src_line_num, e.kind, e.msg))?;
                // also add this line as a comment in the program
                add_line(prog_lines, src_line_num, format!("; {}", &line), None, None, None);
                mo = Some(m);
                continue;
            }
            if let Some(m) = operation.as_ref().and_then(|s| macros.get(s)) {
                // there is a macro to expand on this line
                if label.is_some() {
                    // there is also a label on this line; preserve it (on its own line) before expanding the macro
                    add_line(
                        prog_lines,
                        src_line_num,
                        format!("{}:", label.as_ref().unwrap()),
                        label,
                        None,
                        None,
                    );
                }
                // add a comment with some metadata about this macro instance
                add_line(
                    prog_lines,
                    src_line_num,
                    format!(
                        "; Begin macro \"{}\" from line {} of original source",
                        m.name, src_line_num
                    ),
                    None,
                    None,
                    None,
                );
                // collect any/all args for the macro
                let args = if let Some(a) = operand.as_ref().and_then(|s| self.re_macro_args.captures(s.as_str())) {
                    a.get(0)
                        .unwrap()
                        .as_str()
                        .split(',')
                        .map(|s| s.trim())
                        .collect::<Vec<&str>>()
                } else {
                    Vec::new()
                };
                // expand the macro and add the resulting lines to the program
                m.hydrate_instance(args)?.into_iter().for_each(|s| {
                    let (a, b, c) = self.re_statement.captures(&s).map_or((None, None, None), |c| {
                        (
                            c.get(1).map(|m| m.as_str().to_string()),
                            c.get(2).map(|m| m.as_str().to_ascii_uppercase()),
                            c.get(3).map(|m| m.as_str().to_string()),
                        )
                    });
                    add_line(prog_lines, src_line_num, s, a, b, c);
                });
                continue;
            }
            // the line doesn't include a macro instance, so just add it as a potential statement
            add_line(prog_lines, src_line_num, line.to_string(), label, operation, operand);
        }
        // we've read through all the supplied source lines
        if let Some(m) = mo {
            // a macro definition was begun but never ended
            return Err(syntax_err_line!(
                m.src_file_id,
                m.src_line_num,
                format!("no end found for macro \"{}\"", m.name)
            ));
        }
        path_stack.pop();
        Ok(())
    }

    /// Attempt to load and build an assembly language program from a file with the given path.
    pub fn assemble_from_file(&self, path: &str) -> Result<Program, Error> {
        let mut macros = HashMap::new();
        let mut prog_lines = Vec::new();
        let mut file_stack = Vec::new();

        self.load_asm_file(path, &mut file_stack, &mut prog_lines, &mut macros)?;
        let mut program = Program::new(prog_lines, macros);
        self.assemble_program(&mut program)?;
        if config::ARGS.write_files {
            _ = program.write_output_files(path);
        }
        Ok(program)
    }

    /// Performs the full build process to create a machine code program from the
    /// assembly language in the given Program object.
    ///
    fn assemble_program(&self, program: &mut Program) -> Result<(), Error> {
        println!("Pre-processing...");
        self.pre_build(program)?;
        let mut pass_count = 0;
        println!("Building...");
        loop {
            pass_count += 1;
            println!("Build pass {}...", pass_count);
            if self.build(program)? == 0 {
                break;
            }
            // is the proper build pass limit actually 2 rather than 3?
            if pass_count > 3 {
                // self.full_dump(program);
                return Err(Error::new(
                    ErrorKind::General,
                    None,
                    "Too many build passes. Aborting...",
                ));
            }
        }
        println!("Post-processing...");
        self.post_build(program)?;
        println!("Build complete.");
        if config::ARGS.list {
            program.write_listing(&mut io::stdout())?;
        }
        Ok(())
    }
    /// Perform the intial phase of the build process in which all labels are tracked and
    /// ObjectProducer instances are created for all instructions and directives.
    fn pre_build(&self, program: &mut Program) -> Result<(), Error> {
        let mut pre_build_one_line = |line: &mut ProgramLine| -> Result<(), Error> {
            line.addr = program.addr;
            // Does the line contain a label?
            if let Some(label) = line.label.as_ref() {
                program
                    .labels
                    .new_definition(label, line.src_line_num, line.addr, None)?;
            }
            // Does the line contain an operation (or assembler directive)?
            if line.operation.is_some() {
                // parse the operation and potentially create the corresponding binary object
                self.process_op_line(&mut program.segs, &mut program.labels, line)?;
                // get any/all object address and size info
                if let Some(obj) = line.obj.as_ref() {
                    // check to see if this object has a static address assignment
                    if let Some(addr) = obj.static_address(&program.labels)? {
                        program.addr = addr;
                        line.addr = addr;
                    }
                    line.obj_size = obj.current_size(program.addr, &program.labels)?;
                    let (new_addr, _) = program.addr.overflowing_add(line.obj_size);
                    // reserve space for the object
                    program.addr = new_addr;
                    // if this line has a label then update it to reflect the latest address
                    if let Some(label) = line.label.as_ref() {
                        program.labels.set_address(label, line.addr)?;
                    }
                }
            } else if line.label.is_none() {
                // the line contains neither label nor operation
                // is it a result line? (i.e. lines of the form ";! <reg|addr> = <val>")
                if let Some(c) = self.re_result_line.captures(line.src.as_str()) {
                    if c.get(1).is_none() || c.get(2).is_none() {
                        return Err(syntax_err_line!(
                            line.src_file_id,
                            line.src_line_num,
                            "malformed test criterion"
                        ));
                    }
                    program
                        .results
                        .push(TestCriterion::new(line.src_file_id, line.src_line_num, &c[1], &c[2]));
                    return Ok(());
                }
                // ...or is it just a whole line of comments or whitespace?
                if self.re_comment_or_blank_line.is_match(line.src.as_str()) {
                    // nothing to do; move on to the next line
                    return Ok(());
                }
                // theoretically, we shouldn't be able to reach this point
                unreachable!()
            }
            Ok(())
        };
        for line in program.lines.iter_mut() {
            pre_build_one_line(line).map_err(|e| line_err!(line.src_file_id, line.src_line_num, e.kind, e.msg))?;
        }
        Ok(())
    }
    /// Perform the main phase of the build process. This is called repeatedly until no
    /// more changes occur. These changes represent movement of objects and labels as
    /// object sizes grow and shrink. The number of changes made is returned in ```Ok(usize)```
    fn build(&self, program: &mut Program) -> Result<usize, Error> {
        let mut changes = 0usize;
        // reset the address of the program (this is often referred to as "current location")
        program.addr = 0u16;
        // process for each line
        let mut build_one_line = |line: &mut ProgramLine| -> Result<(), Error> {
            let mut expected_addr = program.addr;
            // assign the expected address to this line; it may change below
            line.addr = expected_addr;
            if let Some(op) = line.obj.as_mut() {
                // try to build the object
                let res = op.build(expected_addr, &program.labels);
                if let Err(e) = res {
                    return Err(line_err!(line.src_file_id, line.src_line_num, e.kind, e.msg.as_str()));
                }
                let bob = res.unwrap();
                // set our next program address based on the binary object we just built
                let (new_addr, _) = bob.addr.overflowing_add(bob.size);
                program.addr = new_addr;
                // the line address should be the same as the object's address
                line.addr = bob.addr;
                // also, if the object has a static address, then that is our expected address
                if bob.is_static_addr {
                    expected_addr = bob.addr;
                }
                if bob.size != line.obj_size {
                    // object size changed; need to ripple the change through the rest of the segment
                    let (_, seg_end) = program.segs.segment_containing_address(program.addr);
                    let (delta, _) = bob.size.overflowing_sub(line.obj_size);
                    // update any passive labels that come after this object in the same segment
                    program
                        .labels
                        .adjust_label_addresses((program.addr, seg_end), delta as i16);
                    // also count object size changes in our general changes counter
                    changes += 1;
                    line.obj_size = bob.size;
                }
            }
            // if this line has a label, make sure its address is up to date
            if let Some(label) = line.label.as_ref() {
                // update the label's address to match the line's address
                let old_addr = program.labels.set_address(label, line.addr)?;
                if old_addr != line.addr {
                    // the label's address changed; note the change in our counter
                    changes += 1;
                }
            }
            if line.addr != expected_addr {
                // count address changes in our general changes counter
                changes += 1;
            }
            Ok(())
        };
        for line in program.lines.iter_mut() {
            if let Err(e) = build_one_line(line) {
                return Err(line_err!(line.src_file_id, line.src_line_num, e.kind, e.msg));
            }
        }
        changes += program.labels.eval_all_nodes()?;
        Ok(changes)
    }
    /// Perform final phase of the build process. For now, this only entails parsing
    /// any test criteria that the program contains.
    fn post_build(&self, program: &mut Program) -> Result<(), Error> {
        for tc in &mut program.results {
            // Each TestCriterion must be parsed AFTER build is complete so that all labels can be resolved.
            if let Err(e) = self.parser.parse_test_criterion(tc, &program.labels) {
                return Err(line_err!(tc.src_file_id, tc.src_line_num, e.kind, e.msg));
            }
        }
        Ok(())
    }
    /// Process a program line that looks like an operation. The line must be a statement
    /// that contains either an assembler directive or an assembly language instruction.
    /// Otherwise, an Error is returned. On success, an ObjectProducer for the operation
    /// is added to the provided ProgramLine.
    fn process_op_line(
        &self, segs: &mut ProgramSegments, labels: &mut ProgramLabels, line: &mut ProgramLine,
    ) -> Result<(), Error> {
        // first see if this is actually an assembler directive
        if self.process_directive_line(segs, labels, line)? {
            // the line contains an assembler directive and it was processed without error
            return Ok(());
        }
        // it's not a directive; see if it's an instruction
        // using ok_or_else to avoid executing the format! every time this next line is executed.
        let desc = instructions::name_to_descriptor(line.get_operation()).ok_or_else(|| {
            syntax_err_line!(
                line.src_file_id,
                line.src_line_num,
                format!("Invalid operation: \"{}\"", line.get_operation())
            )
        })?;
        let od = if line.operand.is_none() || desc.is_inherent() {
            // the instruction uses only inherent addressing or there is no operand
            OperandDescriptor::new()
        } else {
            // there may be an operand and it may be required so try to parse it
            self.parser.parse_operand(line.get_operand())?
        };
        line.obj = Some(Box::new(Instruction::try_new(desc, od)?));
        Ok(())
    }

    /// Process a program line and, if it contains a valid directive, then create an
    /// ObjectProducer for that directive and add it to the given ProgramLine.
    ///   
    /// Results:
    ///  - ```Ok(true)``` line was succesfully processed as a directive
    ///  - ```Ok(false)``` line is not a directive
    ///  - ```Err(Error)``` line is a directive but is invalid
    ///
    fn process_directive_line(
        &self, segs: &mut ProgramSegments, labels: &mut ProgramLabels, line: &mut ProgramLine,
    ) -> Result<bool, Error> {
        match line.get_operation() {
            "ORG" => {
                if line.operand.is_none() {
                    return Err(syntax_err_line!(
                        line.src_file_id,
                        line.src_line_num,
                        "no address specified for ORG"
                    ));
                }
                // org only sets the code location; parse the address expression
                let node = self.parser.str_to_value_node(line.get_operand())?;
                // must be able to evaluate this expression right now!
                // Note: org cannot use location reference ('*'); supplying false addr = 0 here
                let addr = node.eval(labels, 0, false)?;

                // org defines the start of a segment
                segs.add(addr.u16())?;

                // save an ObjectProducer for this org
                line.obj = Some(Box::new(Org::new(node)));

                // set the line address to the value of the operand
                line.addr = addr.u16();

                // if there is a label on this line then set its address
                if let Some(label) = line.label.as_ref() {
                    labels.set_address(label, addr.u16())?;
                }
            }
            "EQU" => {
                // define a label; value can be expression
                if line.operand.is_none() {
                    return Err(syntax_err_line!(
                        line.src_file_id,
                        line.src_line_num,
                        "no value provided for EQU"
                    ));
                }
                let node = self.parser.str_to_value_node(line.get_operand())?;

                // save the ValueNode so we can (re)evaluate it later
                labels.set_node(line.get_label(), node)?;
            }
            "FCB" | "FDB" => {
                if line.operand.is_none() {
                    return Err(syntax_err_line!(
                        line.src_file_id,
                        line.src_line_num,
                        "missing data for FCB/FDB"
                    ));
                }
                let is_bytes = line.get_operation() == "FCB";
                // operand should a comma delimited sequence of expressions that
                // all evaluate to either byte or word depending on operation
                // todo: does anything weird happen if ValueNode contains location reference?
                let mut nodes = Vec::new();
                for val in line.get_operand().split(',') {
                    let node = self.parser.str_to_value_node(val)?;
                    nodes.push(node);
                }
                line.obj = Some(Box::new(Fxb::new(nodes, is_bytes)));
            }
            "FCC" => {
                // The string following an FCC directive can be delimited by any non-whitespace char
                // The first non-whitespace char defines the delimiter.
                // The next occurance of that char marks the end of the string.
                // Any characters thereafter are ignored.
                if line.operand.is_none() {
                    return Err(syntax_err_line!(
                        line.src_file_id,
                        line.src_line_num,
                        "no string provided for FCC"
                    ));
                }
                let mut t: Option<char> = None;
                let mut s = String::with_capacity(line.get_operand().len());
                for c in line.get_operand().chars() {
                    if t.is_none() {
                        if c.is_ascii_whitespace() {
                            continue;
                        }
                        t = Some(c);
                        continue;
                    }
                    if t == Some(c) {
                        line.obj = Some(Box::new(Fcc::new(&s)));
                        break;
                    }
                    s.push(c)
                }
                if line.obj.is_none() {
                    return Err(syntax_err_line!(
                        line.src_file_id,
                        line.src_line_num,
                        "invalid string provided for FCC"
                    ));
                }
            }
            "RMB" => {
                if line.operand.is_none() {
                    return Err(syntax_err_line!(
                        line.src_file_id,
                        line.src_line_num,
                        "no size specified for RMB"
                    ));
                }
                let node = self.parser.str_to_value_node(line.get_operand())?;
                line.obj = Some(Box::new(Rmb::new(node)));
            }
            "END" => {
                if line.operand.is_some() {
                    return Err(syntax_err_line!(
                        line.src_file_id,
                        line.src_line_num,
                        "invalid operand for END"
                    ));
                }
                // do nothing...
            }
            "SETDP" => {
                // SETDP sets the current value the assembler should use for DP when determining
                // whether to automatically employ direct mode addressing.
                // Note: This has no effect on the value of DP in the processor. The developer must
                // ensure that the DP register is set in accordance with whatever value they've provide via SETDP.
                let dp = line.get_operand();
                if dp.is_empty() || self.re_comment_or_blank_line.is_match(dp) {
                    DirectPage::add(DirectPage::None)
                } else {
                    DirectPage::add(DirectPage::Value(self.parser.str_to_value_node(dp)?));
                }
            }
            _ => return Ok(false),
        }
        Ok(true)
    }
}
