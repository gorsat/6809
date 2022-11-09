/*
Some of the code below was derived from https://github.com/console-rs with the following
license, copyright and permissions:

    The MIT License (MIT)

    Copyright (c) 2017 Armin Ronacher <armin.ronacher@active-4.com>

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    SOFTWARE.
*/
use std::fs;
use std::os::unix::io::AsRawFd;

fn io_option<F: FnOnce() -> libc::c_int>(f: F) -> Option<i32> { if f() == 0 { Some(0) } else { None } }

fn tty_fd() -> Option<i32> {
    unsafe {
        if libc::isatty(libc::STDIN_FILENO) == 1 {
            Some(libc::STDIN_FILENO)
        } else if let Ok(fd) = fs::File::open("/dev/tty") {
            Some(fd.as_raw_fd())
        } else {
            None
        }
    }
}
pub fn get_keyboard_input(block: bool, flush: bool) -> Option<u8> {
    let fd = tty_fd()?;
    // configure the tty for raw mode and save the original config
    let mut temp = core::mem::MaybeUninit::uninit();
    io_option(|| unsafe { libc::tcgetattr(fd, temp.as_mut_ptr()) })?;
    let mut termios = unsafe { temp.assume_init() };
    let original = termios.clone();
    unsafe { libc::cfmakeraw(&mut termios) };
    io_option(|| unsafe { libc::tcsetattr(fd, libc::TCSADRAIN, &termios) })?;

    // now read the keyboard input
    let mut rv = read_key_inner(fd, block, flush);

    // and reset the tty config to the original
    io_option(|| unsafe { libc::tcsetattr(fd, if flush { libc::TCSAFLUSH } else { libc::TCSADRAIN }, &original) })?;

    // if we encounterd ctrl-c then raise SIGINT
    if rv == Some(3) {
        rv = None;
        unsafe {
            libc::raise(libc::SIGINT);
        }
    }
    rv
}

fn read_key_inner(fd: i32, block: bool, drain: bool) -> Option<u8> {
    return match read_single_char(fd) {
        Some('\x1b') => {
            // Escape was read
            if let Some(c1) = read_single_char(fd) {
                if c1 == '[' {
                    let c2 = read_single_char(fd)?;
                    match c2 {
                        'A' | 'B' | 'C' | 'D' | 'H' | 'F' | 'Z' => None,
                        _ => read_single_char(fd),
                    };
                }
                // it is some form of escape sequence; ignore it
                None
            } else {
                // just the escape key; return it
                Some(27)
            }
        }
        Some(c) => {
            let byte = c as u8;
            let mut buf: [u8; 4] = [byte, 0, 0, 0];
            if byte & 224u8 == 192u8 {
                // a two byte unicode character
                read_bytes(fd, &mut buf[1..], 1)?;
                byte_from_utf8(&buf[..2])
            } else if byte & 240u8 == 224u8 {
                // a three byte unicode character
                read_bytes(fd, &mut buf[1..], 2)?;
                byte_from_utf8(&buf[..3])
            } else if byte & 248u8 == 240u8 {
                // a four byte unicode character
                read_bytes(fd, &mut buf[1..], 3)?;
                byte_from_utf8(&buf[..4])
            } else {
                Some(match c {
                    '\n' | '\r' => 13, // anything like 'enter' or 'return' is mapped to CR
                    '\x7f' => 8,       // backspace
                    '\t' => 9,         // tab
                    _ => c as u8,
                })
            }
        }
        None if block => {
            // there is no subsequent byte ready to be read, block and wait for input
            let mut pollfd = libc::pollfd {
                fd,
                events: libc::POLLIN,
                revents: 0,
            };
            // negative timeout means that it will block indefinitely
            let ret = unsafe { libc::poll(&mut pollfd as *mut _, 1, -1) };
            if ret < 0 {
                return None;
            }
            read_key_inner(fd, block, drain)
        }
        _ => None,
    };
}

fn read_single_char(fd: i32) -> Option<char> {
    let mut pollfd = libc::pollfd {
        fd,
        events: libc::POLLIN,
        revents: 0,
    };
    // timeout of zero means that it will not block
    let ret = unsafe { libc::poll(&mut pollfd as *mut _, 1, 0) };
    if ret < 0 {
        return None;
    }
    let is_ready = pollfd.revents & libc::POLLIN != 0;
    if is_ready {
        // if there is something to be read, take 1 byte from it
        let mut buf: [u8; 1] = [0];
        read_bytes(fd, &mut buf, 1)?;
        Some(buf[0] as char)
    } else {
        //there is nothing to be read
        None
    }
}

// Similar to libc::read. Read count bytes into slice buf from descriptor fd.
// If successful, return Some(number_of_bytes_read) otherwise None
fn read_bytes(fd: i32, buf: &mut [u8], count: u8) -> Option<u8> {
    let read = unsafe { libc::read(fd, buf.as_mut_ptr() as *mut _, count as usize) };
    if read < 0 { None } else { Some(read as u8) }
}

pub fn byte_from_utf8(buf: &[u8]) -> Option<u8> {
    if let Ok(s) = std::str::from_utf8(buf) {
        if let Some(c) = s.chars().next() {
            // Note: this is probably incorrect but shouldn't matter for our use case
            return Some(c as u8);
        }
    }
    None
}

pub fn init() {
    // intentionally left blank 
}

pub fn flush_keyboard_input() {
    if let Some(fd) = tty_fd() {
        while read_single_char(fd).is_some() {}
    }
}
