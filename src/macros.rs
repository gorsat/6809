#![allow(unused_macros, dead_code)]
macro_rules! verbose_println {
    ($($p:expr),+) => {
        if (config::ARGS.verbose) {
            println!($($p),+);
        }
    }
}
// Adding explicit carriage returns to some of these because in testing (at least on mac)
// I found that CR would occasionally be elided when only LF was used.
macro_rules! info {
    ($($p:expr),+) => {
        println!(concat!(blue!("INFO"),": {}\r"),format_args!($($p),+))
    }
}

macro_rules! warn {
    ($($p:expr),+) => {
        println!(concat!(red!("WARNING"),": {}\r"),format_args!($($p),+))
    }
}
macro_rules! acia_dbg {
    ($($e:expr),+) => {
        if config::ARGS.acia_debug {
            println!("{}\r",format_args!($($e),+));
        }
    };
}
macro_rules! line_err {
    ($line:expr, $kind:expr, $msg:expr) => {
        Error::new($kind, None, format!("line {} {}", $line, $msg).as_str())
    };
}
macro_rules! general_err {
    ($msg:expr) => {
        Error::new(crate::ErrorKind::General, None, format!("{}", $msg).as_str())
    };
}

macro_rules! syntax_err {
    ($msg:expr) => {
        Error::new(
            crate::ErrorKind::Syntax,
            None,
            format!("{} {}", red!("Syntax Error"), $msg).as_str(),
        )
    };
}
macro_rules! syntax_err_line {
    ($line:expr, $msg:expr) => {
        Error::new(
            ErrorKind::Syntax,
            None,
            format!("{}, line {}: {}", red!("Syntax Error"), $line, $msg).as_str(),
        )
    };
}
macro_rules! syntax_err_ctx {
    ($ctx:expr,$msg:expr) => {
        Error::new(
            ErrorKind::Syntax,
            $ctx,
            format!("{} {}", red!("Syntax Error"), $msg).as_str(),
        )
    };
}
macro_rules! runtime_err {
    ($ctx:expr,$($msg:expr),*) => {
        Error::new(
            ErrorKind::Runtime,
            $ctx,
            format!("{} {}", red!("Runtime Error"), format!($($msg),*)).as_str(),
        )
    };
}
macro_rules! within_usize_bound {
    ($val:expr,$bound:expr) => {
        ((($val) as usize) < (($bound) as usize))
    };
}
macro_rules! break_on_error {
    ($result: expr) => {
        if ($result).is_err() {
            break;
        }
    };
}
macro_rules! alt_screen_buffer {
    () => {
        print!("\x1b[?1049h") //ESC [ ? 1 0 4 9 h
    };
}

macro_rules! main_screen_buffer {
    () => {
        print!("\x1b[?1049l") //ESC [ ? 1 0 4 9 l
    };
}

macro_rules! xor {
    ($a: expr, $b: expr) => {
        ((($a) && !($b)) || (!($a) && ($b)))
    };
}
macro_rules! bit {
    ($a: expr, $b: expr) => {
        (((($a) as u32) & (1 << ($b) as u32)) != 0)
    };
}
macro_rules! clear_screen {
    () => {
        print!("\x1b[2J\x1b[H")
    };
}
macro_rules! color {
    ($color: literal, $msg: expr) => {
        concat!("\x1b[", $color, "m", $msg, "\x1b[0m")
    };
}
macro_rules! red {
    ($msg:expr) => {
        color!(91, $msg)
    };
}
macro_rules! green {
    ($msg:expr) => {
        color!(92, $msg)
    };
}
macro_rules! yellow {
    ($msg:expr) => {
        color!(93, $msg)
    };
}
macro_rules! blue {
    ($msg:expr) => {
        color!(94, $msg)
    };
}
