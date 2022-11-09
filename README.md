# A 6809 Assembler, Simulator and Debugger written in Rust.

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

 ## Assembler

 ...
 There are a few examples of assembly language programs in the [test](./test) directory that show several features of the assembler.
 The program [mbadd.asm](./test/mbadd.asm) demonstrates macros:
 ```
 * Simple macros are supported
    .macro	clc
        andcc	#$fe
    .endm
* Here's an example with a parameter
    .macro my_bne
        bne @0
    .endm
```
...as well as test criteria (for which there is some documentation in [test.rs](src/test.rs))
```
;
; The lines beginning with ";!" each define a TestCriterion (see test.rs)
; When the program is run with the --run (and not the --debug) option
; these criteria will be checked after the program is run.
;! output = #0
;! output+1 = #0
;! output+2 = #0
;! output+3 = #$80
;! x = #heap+4
;! s = #heap
```

## Output Files
By default, the assembler just compiles the given .asm/.s file into 6809 machine code,
reports success or failure, and exits. 
To produce output files, specify the ```-w``` (--write-files) flag. 
This will cause the assembler to generate three files (for a given input file named *filename.asm*):
1. A symbol file named *filename.sym*. This is a text file listing all symbols in the program along with their associated addresses. It is used by the debugger.
2. A listing file named *filename.lst*. This is a text file that contains a full listing of the program including both the machine code and its associated assembly language. It is intended for human use in debugging.
3. A hex file named *filename.hex*. This is a text file in [Intel I8HEX format](https://en.wikipedia.org/wiki/Intel_HEX) containing the program in memory-mapped machine code. It is used to move the program to a device.


**WARNING**: The assembler will not check for the prior existence of any of these files and will simply try to overwrite them if they already exist.

**NOTE**: The program can load/run/debug a hex file rather than an assembly language file but it requires the file to have the .hex extension. You still have to specify that you want to ```--run``` the program:
```
6809 -r my_program.hex
```


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

Another fun project would be creating a full emulation of the TRS-80 Color Computer that could run old ROMs. I'd love to play some of those games I had as a kid. I imagine this would be quite a bit of work but is probably pretty straightforward using bevy.

The obsessive engineer in me would love to generalize this thing, but it is currently written with knowledge of the 6809 throughout the codebase. 
It would be interesting to try to add support for another processor -- like the 6502 -- just to see how difficult that would be. If that worked out okay then I'd try adding support for others.  

## Bugs, Features, Issues
There are undoubtedly a lot of bugs in this pile of code. 
Setting aside general logic problems, there are likely some subtle simulator bugs where I've missed an instruction's side-effects (which, in the case of this tiny MPU, typically just means effects on the CC register).  

If you spot any of these, or any other bugs, then please let me know. 
I'd like for the simulator to be as true as possible to its namesake. 

Feature requests are also welcome, but I can't guarantee I'll get around to implementing them.

Any other comments or ideas? Just open an issue.
