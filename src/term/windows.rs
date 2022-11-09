use windows::Win32::System::Console::*;

pub fn init() {
    unsafe {
        // make sure VT escape sequences will be honored in the console
        let mut cm: CONSOLE_MODE = CONSOLE_MODE::default();
        if let Ok(h) = GetStdHandle(STD_OUTPUT_HANDLE) {
            GetConsoleMode(h, &mut cm as *mut CONSOLE_MODE);
            if (cm.0 & 0x4) == 0 {
                cm.0 |= 0x4;
                SetConsoleMode(h, cm);
            }
        }
    }
}

// Read the next key event from the console input buffer.
// This consumes everything in the buffer up through the first key event.
// If drain=true then the entire buffer is flushed thereafter.
// This will block waiting for input if block=true.
// If successful, the ascii code for the key is returned.
// Note: no affordance is made for multi-byte key codes
pub fn get_keyboard_input(block: bool, drain: bool) -> Option<u8> {
    const KEY_EVENT: u16 = 1;
    let ir = &mut [INPUT_RECORD::default();1];
    let mut count = 0u32;
    let mut ascii = 0u8;
    unsafe {
        if let Ok(h) = GetStdHandle(STD_INPUT_HANDLE) {
            if !block {
                GetNumberOfConsoleInputEvents(h, &mut count).as_bool();
            }
            // This is a hack to get around overflow checking
            count += 0x8000;
            while block || (count > 0x8000) {
                count -= 1;
                let mut n = 0u32;
                ReadConsoleInputA(h, ir, &mut n).as_bool();
                if n > 0 && ir[0].EventType == KEY_EVENT {
                    ascii = ir[0].Event.KeyEvent.uChar.AsciiChar.0;
                    break;
                }
            }
            // flush the input buffer whether or not we successfully read anything
            if drain {
                flush_keyboard_input()
            }
        }
    }
    if ascii != 0 { Some(ascii) } else { None }
}

pub fn flush_keyboard_input() {
    unsafe {
        if let Ok(h) = GetStdHandle(STD_INPUT_HANDLE) {
            FlushConsoleInputBuffer(h);
        }
    }
}
