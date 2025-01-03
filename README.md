# 6809 Assembler, Simulator, Debugger (in rust)

The venerable [Motorola 6809](https://en.wikipedia.org/wiki/Motorola_6809) was the first microprocessor I knew.
I have great fondness for it and when I was looking for a project through which to learn Rust,
I figured a 6809 assembler might be just the ticket. 
But then I wanted to be able to run the output on something, so I decided to write a simple simulator, too. 
And then I needed to debug both the assembler and the simulator so I bolted on a simple debugger as well.

I ended up with this little 6809 swiss army knife and the world got yet another piece of software focused on a processor from 40+ years ago.
It's not optimal, or cycle-accurate, or a good example of Rust code. 
But it will build and run some substantial assembly language programs (like Microsoft Extended Basic), 
and run them much faster than they ever ran on the real processor.
It might be useful for someone doing some retro work and it's definitely entertaining for those who want to relive the magic of early 1980s computing.

> Note: If you're looking for a TRS-80 Color Computer emulator then check out [gorsat/coco](https://github.com/gorsat/coco). 
> Using the coco emulator, you can load the EDTASM+ cartridge and directly edit/build/run 6809 assembler in the old coco console.
> It's arguably not as developer friendly as this 6809 simulator, but it's lots of fun :grin:

## Getting Started
This program runs on Mac, Windows and Linux (only tested on Ubuntu). To sanity check it, you can run a quick test: 
```
cargo test
```
...and after several lines of output it should say something like:
```
test result: ok. 3 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.04s
```

To assemble and run a program:
```
cargo run -- -r /path/to/program.asm
```
...or if you've already built the binary then just...
```
6809 -r /path/to/program.asm
```
## Options
Help for command line options is available using -h or --help.
There are a lot of options and I won't cover all of them here.
Hopefully most of them are sufficiently self explanatory.
```
MC6809 assembler, simulator and debugger written in Rust.

Usage: 6809 [OPTIONS] <FILE>

Arguments:
  <FILE>  Assembly (.asm, .s) or Hex (.hex) file to assemble/run/debug

Options:
      --acia-disable
          Disable ACIA emulation
      --acia-addr <ACIA_ADDR>
          Address at which to map the ACIA (hex ok with '0x') [default: 65488]
      --acia-port <ACIA_PORT>
          TCP port on which to expose ACIA [default: 6809]
      --acia-debug
          Print ACIA debug information
      --acia-case
          Swap the case of alpha ASCII characters received via ACIA (a->A;A->a)
  -b, --break-start
          Break into the debugger before running the program (only if debugger enabled)
  -c, --code-only
          Remove blank and comment-only lines from program listing
  -d, --debug
          Run the program with debugger enabled (if both -r and -d are specified then -d wins)
      --history <HISTORY>
          The number of instructions to keep in the execution history when debugging [default: 200]
  -l, --list
          If there is a program listing then dump it to stdout
      --lbr-disable
          Disable automatic branch->long_branch conversion
  -n, --no-auto-sym
          No automatic loading of symbols
  -p, --pemdas
          Automatically evaluate expressions using PEMDAS rather than left-to-right
      --perf
          Display perf data (only interesting for longer-running programs)
      --ram-top <RAM_TOP>
          Set the top RAM address [default: 32767]
      --reset-vector <RESET_VECTOR>
          Override the reset vector
  -r, --run
          Run the program with debugger disabled and evaluate any test criteria
  -t, --trace
          Trace each machine instruction as it is executed
  -v, --verbose
          Enable verbose output
  -w, --write-files
          Write output files after assembly (.lst, .sym, .hex)
  -h, --help
          Print help information
  -V, --version
          Print version information
```
> **NOTE**: The program can load/run/debug a hex file (in I8HEX format) rather than an assembly 
> language file but it requires the file to have the .hex extension. 
> You still have to specify that you want to `--run` the program, i.e., `6809 -r my_program.hex`


 ## Assembler
The assembler supports a number of features including:
- **simple macros** - See below. Macros are expanded prior to the build process and parameters are supported.
- **test criteria** - See below. Test criteria are evaluated after the completion of a `--run`.
- **direct mode addressing** - you can force direct mode by prepending a '\<' to an 8-bit address
- **current location reference** - you can use '\*' to refer to the "current location" in the program, so that, for example, the statement `jmp *` is an infinite loop.
- **automatic branch extension** - if you use a Bxx instruction but the destination is too far away then it will be automatically converted to an LBxx instruction (and you can turn this off with the `--lbr-disable` option)
- **multiple literal types** - in addition to decimal you can use hex values (`$ff`) and binary (`%0101`) and character literals (`'Z`) in operands
- **left-to-right and pemdas** - 
Assembly language programs (like Microsoft Extended Basic) relied on the fact that 
expressions in operands would be evaluated left-to-right rather than following the 
regular PEMDAS rule. Thus, the assembler uses left-to-right as the default order
of operations. In other words, the expression `2+8/2` evaluates to 5 rather than 6.
However, you can just specify `-p` or `--pemdas` to change this. Also, whether in
PEMDAS mode or not, the assembler supports parentheses so that `2+(8/2)` will 
always evaluate to 6.

### Macros
 There are a few examples of assembly language programs in the [test](./test) directory that show several features of the assembler.
 The program [mbadd.asm](./test/mbadd.asm) includes the following trivial macro definition:
 ```
    .macro my_bne
        bne @0
    .endm
```
Once defined, this macro can be used anywhere you'd use a regular instruction and it will be expanded with parameters, so that, for example, the statement
```
     my_bne LOOP
```
would be replaced with
```
     bne LOOP
```
prior to the build process. Macros are not limited in terms of number of lines or parameters. I will caveat this by saying that I have not got around to adding signficant macro tests to the test suite.

### Including Files
A simple ```.include <asm_file_path>``` statement enables code reuse and basic modularity by literally inserting the contents of the referenced .asm file within the current file. So, for example, if this is file1.asm:
```
    lda #1
    .include include/file2.asm
    swi
```
...and ```./include/file2.asm``` is the following:
```
; this is file2
    ldb #2
```
...loading file1.asm results in the following combined listing:
```
    lda #1
; this is file2
    ldb #2
    swi
```
...which is then assembled. You can include at multiple levels of depth, but not recursively (so, for example, file2.asm could include a file3.asm, but it would cause an error if file2.asm or file3.asm included file1.asm).

### Directives
The following assembler directives are suppported:


- **EQU**
<br>Syntax: ```Label EQU <expression>```
<br>Example: ```ANSWER EQU 21*2```
<br>Sets Label equal to a constant. The constant can be provided as an expression.
- **FCB**
<br>Syntax: ```[Label] FCB <expression> [,<expression>...]```
<br>Example: ```hex_ans fcb $42```
<br>Sets a byte (or sequence of bytes) in the program binary equal to the evaluated expression(s). Each evaluated expression must fit into 8 bits. The list of expressions must not contain spaces. The optional Label will be equal to the address where the first byte is stored in the binary.
- **FDB**
<br>Syntax: ```[Label] FDB <expression> [,<expression>...]```
<br>Example: ```Magic FDB 378+ANSWER,$face,$45,'E```
<br>Sets a 16-bit word (or sequence of words) in the program binary equal to the evaluated expression(s). Each evaluated expression must fit into 16 bits. The list of expressions must not contain spaces. The optional Label will be equal to the address where the first word is stored in the binary.
- **FCC**
<br>Syntax: ```[Label] FCC <delim><ascii_string><delim>```
<br>Example: ```Hello FCC "Hello, world!"```
<br>Stores an ascii string as bytes within the program binary. The deliminators can be any character but they must match one another. Any characters following the second deliminator are ignored. The optional Label will be equal to the address where the first byte of the string is stored in the binary.
- **ORG**
<br>Syntax: ```[Label] ORG <expression>```
<br>Example: ```Start org $1000```
<br>Sets the current address within the program binary. The next instruction or data directive (e.g. FCB) will begin at the address defined by the expression. The optional Label will be equal to this address.
- **RMB**
<br>Syntax:```[Label] RMB <expression>```
<br>Example:```Table rmb 13*14```
<br>Reserves a block of bytes whose size is given by *expression*. The current program address is increased by this number of bytes, thus the next instruction or data directive will begin at the address just after the reserved bytes. The optional Label will be equal to the address of the first byte in the reserved block.
- **SETDP**
<br>Syntax:```      SETDP [<expression>]```
<br>Example:```     setdp Table/256```
<br>Tells the assembler to assume that the DP register will be set to the value given by *expression* at runtime. This allows the assembler to optimize for direct mode addressing. By default, DP is assumed to be 0. You can turn off direct mode optimization by using SETDP without an expression. If the *expression* evaluates to an 8-bit number then DP is assumed to be equal to that number. If *expression* evaluates to a 16-bit number then the LSB must be equal to 0 and DP is assumed to be equal to the MSB.
- **END**
<br>Syntax: ```     END```
<br>Does nothing. Use the SWI instruction to cleanly terminate a program.


### Test Criteria
Test criteria are really a feature of the runtime rather than the assembler, but it feels like an assembler feature so I'm covering it here.
This provides a very quick and simple way to test assembly language programs without having to get into the debugger. 
As an example, you can quickly confirm left-to-right expression evaluation with the following program:
```
    lda #2+8/2
    ldb #2+(8/2)
    swi
;! a = #5
;! b = #6
``` 
The comments that start with a bang are called test criteria. The program cleanly exits when it executes the software interrupt instruction (`swi`). At that point, the runtime checks to see if there are any test criteria in the program and, if there are, they are evaluated. 
So, if you run this little program:
```
cargo run -- test/ooo.asm -r
```
you'll get the following output:
```
INFO: Executing ooo.asm
INFO: Encountered SWI. Program execution terminated.
INFO: Validating 2 test criteria
	A = #$05 --> PASS
	B = #$06 --> PASS
```
Here are some rules for using test criteria:
- Each test criterion must start with `;! `.
- There can be only one test criterion on a line.
- Nothing but whitespace can precede the test criterion on the line.
```
    lda #10
; the comment after the following instruction is ignored
    sta foo  ;! foo = #10
; but the test criterion on the next line works just fine
             ;! foo = #10
```
- Test criteria can appear on any line of an asm file.
- The lhs of a test criterion must evaluate to a register name or an address and the value contained within it at the end of the program will be used in the comparison.
- The rhs of a test criterion must evaluate to an address or a value.
- If either side of a test criterion evaluates to a 16-bit value then a 16-bit comparison is performed. Otherwise, an 8-bit comparison is performed. Note that address reads are 8-bit by default so when you compare the contents of two addresses, you're comparing one byte.
```
; For this program:
        ldx #$fa
        stx $fa
        swi
foo     org     $f9
; These are all valid, passing test criteria:
;! x = #$fa       <-- compare X to the number 00fa
;! x = $fa        <-- compare X to the 16-bits at address 00fa
;! foo+2 = #$fa   <-- compare the 8-bits at address foo+2 (00fb) to the value fa
;! foo+1 = #$00fa <-- compare the 16-bits at address foo+1 (00fa) to the value 00fa
;! x = foo+1      <-- compare X to the 16-bits at address foo+1 (00fa)
```

Even though you can place test criteria on any line of an ASM file, they will only be evaluated after the program has completed execution. Hence, if you want to keep comparing, say, the current contents of a register, then you should store those values sequentially in a list and then compare individual list entries via test criteria (e.g. see the use of `table` in [issue4.asm](test/issue4.asm)). 

There is more documentation on test criteria syntax and rules in [test.rs](src/test.rs).

## Output Files
By default, the assembler just compiles the given .asm/.s file into 6809 machine code,
reports success or failure, and exits. 
To produce output files, specify the ```-w``` (--write-files) flag. 
This will cause the assembler to generate three files (for a given input file named *filename.asm*):
1. A symbol file named *filename.sym*. This is a text file listing all symbols in the program along with their associated addresses. It is used by the debugger.
2. A listing file named *filename.lst*. This is a text file that contains a full listing of the program including both the machine code and its associated assembly language. It is intended for human use in debugging.
3. A hex file named *filename.hex*. This is a text file in [Intel I8HEX format](https://en.wikipedia.org/wiki/Intel_HEX) containing the program in memory-mapped machine code. It is used to move the program to a device.


> **WARNING**: The assembler will not check for the prior existence of any of these files and will simply try to overwrite them if they already exist.


## Interacting with programs
The simulator includes a basic ACIA (UART) so that terminal-based programs (like Basic) can run interactively. The default address for the ACIA is FFD0 but this can be changed on the command line using the --acia-addr option, e.g.:
```
6809 -r --acia-addr=0xa000 program.asm
```
The I/O is sent/rcvd as raw bytes via TCP on localhost. The default port is 6809 but this can be changed using the option --acia-port. You can connect to it using telnet or putty. Here's what it looks like via telnet when the simulator is running Basic:
```
% telnet localhost 6809
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
6809 EXTENDED BASIC
(C) 1982 BY MICROSOFT

OK
```
A program like Basic will echo its input one character at a time, so you'll want to turn off local echo in putty or, if using telnet, put it in character mode after you connect:
```
^]
telnet> mode character
```
## Running Basic Programs
Basic was the first programming language I learned on my TRS-80 Color Computer. 
One of my goals for this project was to be able to run Microsoft Extended Basic in the simulator and then run some Basic programs within it.
That works quite well as long as the version of Basic your program is written in matches the version of basic you're running on the simulator.
When I was a kid I was blissfully unaware of all of the flavors of Basic in the world, 
and it turns out that there were a _lot_ of them.  

I know that Microsoft has released one version of their old Basic source code as open source,
but I haven't found a version of Basic for the 6809 that is clearly in the open source domain,
so I'm not including the Basic code (or binary) in this repo. 
But there are several versions available out there that you can use (check the [resources section](#resources) below). 
Follow the examples above to run Basic (and note that you may need to specify `--acia-addr`) and then you can just paste a Basic program into your terminal.  

I'm including a couple of (non-copyright) Basic programs in the [basic](./basic) directory. 
Try copy-pasting [amazing.bas](./basic/amazing.bas) into the basic terminal. 
Then you can type 'LIST' and 'RUN' (note that these _must be in uppercase_).
The amazing.bas program takes two comma-delimited numbers as its input. 
Try entering "9,9" to see what it does.


## Debugging
When you run with the ```-d``` (--debug) option, the debugger is enabled. 
If you want the simulator to break into the debugger prior to executing the program then add the ```-b``` (--break-start) option.  
If a program is running with the debugger enabled you can press any key to pause the program and bring up the debug prompt.
The debugger is fairly crude but has several useful features. You can get a list of commands by typing ```h``` at the debug prompt:
```
Debug> h
g - Go; Resume execution at PC
his - Show recent history of executed instructions
c - Context; Display the state of all registers
ba <loc> [<notes>] - Breakpoint Add; add break at <loc>
bw <loc> [<notes>] - Add Watch Breakpoint on <loc>
bt - Breakpoint Toggle; active/inactive toggle for breakpoint <num>
bd <num> - Breakpoint Delete; delete breakpoint #<num>
bl - Breakpoint List; list all breakpoints
bn <num> <notes> - Breakpoint Notes; change notes for breakpoint <num>
dm [<loc>] [<num>] - Dump Memory; show <num> bytes at <loc>
ds [<num>] - Dump Stack; show <num> bytes of system stack
l [<loc>] [<num>] - List <num> instructions at <loc>
q - Quit; terminate this application
r - Restart program at original Program Counter address
rs - Restart Step; restart in step mode
s - Step; enter step mode (press esc to exit)
so - Step Over current instruction, then enter step mode
t - Trace; toggle tracing on/off
wd - Working Directory; display the current working directory
load <file> - Load Symbols; load symbols from .sym file
h - Help; display this help text
sym [<loc>] - List all symbols or show symbols at <loc>
<loc> syntax: Hex address (e.g. FF0A) or '?' followed by symbol (e.g. "?START")
```
### Step Mode
When using step mode in the debugger, press ```<esc>``` to exit back to the debug prompt, press ```<enter>``` to step over the next instruction, or press any other key to step into the next instruction.

### Trace, Step and History Output
To see each instruction as it is executed by the simulator, run with the ```-t``` (--trace) option. 
To begin stepping in the debugger use the ```s``` (step) or ```rs``` (restart-step) commands. And you can see all the recently executed instructions using the ```his``` (history) command.
Trace, step and history provide a lot of info. Here's an example of two lines of output:
```
1028:            JSR      $1000      push+      [X:1040 Y:103c U:0800 S:0ffe PC:1000 A:00 B:00 D:0000 DP:00 CC:00 -> (C:0 V:0 Z:0 N:0 I:0 H:0 F:0 E:0)]
1000: push+      LDD      ,X         head       [X:1040 Y:103c U:0800 S:0ffe PC:1002 A:00 B:00 D:0000 DP:00 CC:04 -> (C:0 V:0 Z:1 N:0 I:0 H:0 F:0 E:0)]
```

The columns are as follows:  
|address|symbol1|operation|operand|symbol2|context| 
|---|---|---|---|---|---|
|1000:|push+|LDD|,X|head|(etc.)|
- **address** - The address at which the instruction begins.  
- **symbol1** - A symbol<sup>*</sup> associated with this address (if any).  
- **operation** - The mnemonic of the operation performed.   
- **operand** - The operand of the instruction.  
- **symbol2** - A symbol<sup>*</sup> associated with the operand (if any).  
- **context** - The register set _after_ the instruction executed.  

<sup>*</sup>Note that a '+' is appended to a symbol to indicate that there are other symbols associated with the same address. Use the debugger command ```sym``` to see the others (e.g. ```sym ?push``` would show information about the "push" symbol including all other symbols associated with the same address.)

## Performance
The simulator was not intended to be high performance or cycle accurate. 
But thanks to the high speed of modern processors, it still outpaces its physical counterpart.
With the retail build of the simulator running on a Mac mini i3, I typically get a virtual 6809 running with an effective clock at more than 7 MHz and executing more than 3 MIPS.
I believe that's at least 7 times faster than the original 6809 and it's good enough for my purposes.
You can measure performance yourself using the ```--perf``` option along with ```-r```.
For example, here we're running the retail build in perf mode against one of the simple tests in the test/perf directory:
```
% cargo run -r -- test/perf/countdown.asm -r --perf
```
This gives the following in the app's output: 
```
INFO: Executed 262911 instructions in 0.07 sec; 3.855 MIPS; effective clock: 7.707 MHz
```
In the current design, every operation is executed virtually first and then committed to the simulator's registers and memory thereafter. 
This allows for maximum control in the simulator and debugger but comes with a performance cost. 
For example, the full set of the CPU's registers is copied at least twice for each instruction executed.
The fact is, I'm not taking advantage of this extra level of virtualization and it could be removed.
That said, I'm too lazy to change it right now, so I've left it as-is.

### Debugger Performance
When debugging (or tracing), a lot of cost is incurred by the creation of formatted strings for each instruction executed. 
Debugging also requires a lot of interstitial logic. 
I haven't had the need to speed up debugging substantially, so I haven't devoted any time to optimizing it.

But the debugger does have a huge impact on performance (more than an order of magnitude). 
And using a debug build of the executable also reduces perf by a factor of two or more. 
So if you're trying to do something like run a big Basic program on top of a 6809 Basic implementation then I recommend 
that you use the retail build of the executable (```cargo run -r```) and definitely use
the ```-r``` (--run) option for the simulator.
```
% cargo run -r -- Basic.asm -r
```
## Documentation
I've included some rustdoc comments throughout the source.
These are most useful when you use `cargo doc` to read them in the browser, i.e.:
```
cargo doc --no-deps --open
```
This command builds the html doc pages and opens the default browser to display them.

## Resources
You can find a lot of documentation and enthusiasm for the 6809 on the web.
Here are just a few resources that may be useful in conjunction with this project:
- [Grant Searle's site](http://searle.x10host.com/6809/Simple6809.html) has a wealth of info including the source code for Basic [as a .zip download](http://searle.x10host.com/6809/ExBasRom.zip)
- [Jeff Tranter's github](https://github.com/jefftranter) includes a lot of goodies for the 6809. The source code for Basic along with the binary in hex format are in [this directory](https://github.com/jefftranter/6809/tree/master/sbc/exbasrom). _Note that this version uses A000 as the ACIA address_.
- [jmatzen's github](https://github.com/jmatzen) has a copy of the source code for Basic and the binary in hex format in [this directory](https://github.com/jmatzen/exrombas). He also has [several example programs](https://github.com/jmatzen/leventhal-6809) from Leventhal's definitive book on 6809 assembly language.
- [The Color Computer Archive](https://colorcomputerarchive.com) has a ton of resources including the [official datasheet](https://colorcomputerarchive.com/repo/Documents/Datasheets/MC6809E%20HMOS%208%20Bit%20Microprocessor%20(Motorola).pdf) and an impressive collection of [source code](https://colorcomputerarchive.com/repo/Programming/Source/) for all kinds of things.
- [The Zippster Zone](https://thezippsterzone.com/) has some good stuff including a copy of [6809 Assembly Language Programming](https://thezippsterzone.com/wp-content/uploads/2018/06/6809-Assembly-Language-Programming-Lance-Leventhal.pdf) by Lance. A. Leventhal. I have a physical copy of it and it's a prized possession. 
## Todos
It would be fun to build and run more substantial programs (e.g., OS9). On the build side, this would require the addition of linking as well as a good way to integrate with make.
The runtime would require support for more device emulation.

~~Another fun project would be creating a full emulation of the TRS-80 Color Computer that could run old ROMs. I'd love to play some of those games I had as a kid. I imagine this would be quite a bit of work but is probably pretty straightforward using bevy.~~ [This one is done!](https://github.com/gorsat/coco)

The obsessive engineer in me would love to generalize this thing, but it is currently written with knowledge of the 6809 throughout the codebase. 
It would be interesting to try to add support for another processor -- like the 6502 -- just to see how difficult that would be. If that worked out okay then I'd try adding support for others.  

## Bugs, Features, Issues
There are undoubtedly a lot of bugs in this pile of code. 
Setting aside general logic problems, there are likely some subtle simulator bugs where I've missed an instruction's side-effects (which, in the case of this tiny MPU, typically just means effects on the CC register).  

If you spot any of these, or any other bugs, then please let me know. 
I'd like for the simulator to be as true as possible to its namesake. 

Feature requests are also welcome, but I can't guarantee I'll get around to implementing them.

Any other comments or ideas? Just open an issue.
