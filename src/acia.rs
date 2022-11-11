use super::*;
use std::cell::RefCell;
use std::io::prelude::*;
use std::net::TcpListener;
use std::sync::mpsc::{channel, Receiver, Sender};
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::Duration;

// status register bits
const RDRF: u8 = 0b00000001; // receive data register full
const TDRE: u8 = 0b00000010; // transmit data register empty

pub struct Acia {
    pub addr: u16,
    pub handle: Option<thread::JoinHandle<Result<(), Error>>>,
    txout: Sender<u8>,
    rxin: Receiver<u8>,
    recv_cache: RefCell<Option<u8>>,
    tty_count: Arc<Mutex<i32>>,
}

impl Acia {
    pub fn control_register_address(&self) -> u16 { self.addr }
    pub fn status_register_address(&self) -> u16 { self.addr }
    pub fn data_register_address(&self) -> u16 { self.addr + 1 }
    pub fn owns_address(&self, addr: u16) -> bool { addr == self.addr || addr == (self.addr + 1) }
    pub fn write(&mut self, addr: u16, byte: u8) -> Result<(), Error> {
        if addr == self.control_register_address() {
            // ignore control register writes
            return Ok(());
        } else if addr == self.data_register_address() {
            // ignore error here
            _ = self.txout.send(byte);
        }
        Ok(())
    }
    pub fn read(&self, addr: u16) -> Result<u8, Error> {
        let mut flags = 0u8;
        if addr == self.status_register_address() {
            // if there is some data ready to read then set the RDRF bit
            let pending_data = self.recv_cache.borrow().or_else(|| self.rxin.try_recv().ok());
            if pending_data.is_some() {
                acia_dbg!("ACIA status - pending data {:02X}", pending_data.unwrap());
                *self.recv_cache.borrow_mut() = pending_data;
                flags |= RDRF;
            }
            // if we have a TTY connected then set the TDRE flag
            let ttyc = self.tty_count.lock().unwrap();
            if *ttyc > 0 {
                flags |= TDRE;
            }
            Ok(flags)
        } else if addr == self.data_register_address() {
            // try to get a byte from our cache or from the comms thread
            let pending_data = self.recv_cache.borrow().or_else(|| self.rxin.try_recv().ok());
            if let Some(pending_data) = pending_data {
                *self.recv_cache.borrow_mut() = self.rxin.try_recv().ok();
                let byte = pending_data;
                acia_dbg!("ACIA read {:02X}", byte);
                Ok(byte)
            } else {
                // user read the data register when there was no data available.
                // result is undefined? just return a 0?
                Ok(0)
            }
        } else {
            panic!("invalid ACIA read address")
        }
    }
}

impl Acia {
    pub fn new(addr: u16) -> Result<Acia, Box<dyn std::error::Error>> {
        let (txout, rxout): (Sender<u8>, Receiver<u8>) = channel();
        let (txin, rxin): (Sender<u8>, Receiver<u8>) = channel();
        let tty_count = Arc::new(Mutex::new(0));
        const MSEC_10: Duration = Duration::from_millis(10);

        let thread_tty_count = Arc::clone(&tty_count);
        let handle = Some(thread::spawn(move || {
            let listener = TcpListener::bind(format!("127.0.0.1:{}", config::ARGS.acia_port))
                .map_err(|e| Error::new(ErrorKind::General, None, e.to_string().as_str()))?;
            info!(
                "ACIA instantiated at address {:04X}, listening at {}",
                addr,
                listener.local_addr().unwrap()
            );
            while let Ok((mut stream, client_addr)) = listener.accept() {
                info!("ACIA accepted connection from {}", client_addr);
                _ = stream.set_nodelay(true);
                _ = stream.set_read_timeout(Some(MSEC_10));
                _ = stream.set_write_timeout(Some(MSEC_10));
                let mut in_buf = [0u8; 256];
                let mut out_buf = [0u8; 3];
                {
                    let mut ttyc = thread_tty_count.lock().unwrap();
                    *ttyc += 1;
                }
                'io_loop: loop {
                    // read any input from client
                    let mut r = stream.read(&mut in_buf);
                    if let Err(e) = r {
                        if e.kind() != std::io::ErrorKind::WouldBlock && e.kind() != std::io::ErrorKind::TimedOut {
                            acia_dbg!(red!("ACIA TCP read error: {}"), e);
                            break;
                        }
                    } else {
                        let size = r.unwrap();
                        if size == 0 {
                            // connection closed
                            break;
                        }
                        // forward input to Core
                        #[allow(clippy::needless_range_loop)]
                        for i in 0..size {
                            let b: u8 = match in_buf[i] {
                                0x41..=0x5a if config::ARGS.acia_case => in_buf[i] + 0x20,
                                0x61..=0x7a if config::ARGS.acia_case => in_buf[i] - 0x20,
                                0x7f => 8, // delete --> backspace
                                _ => in_buf[i],
                            };
                            _ = txin.send(b);
                            acia_dbg!(green!("ACIA recv {:02X}"), in_buf[i]);
                        }
                    }
                    // get any output from Core
                    while let Ok(byte) = rxout.try_recv() {
                        // forward output to the client
                        out_buf[0] = byte;
                        if byte == 8 {
                            out_buf[1] = 0x20;
                            out_buf[2] = 8;
                            r = stream.write(&out_buf[..3]);
                            acia_dbg!(yellow!("ACIA send 0x08, 0x20, 0x08"));
                        } else {
                            acia_dbg!(yellow!("ACIA send {:02X}"), byte);
                            r = stream.write(&out_buf[..1]);
                        }
                        if let Err(e) = r {
                            if e.kind() != std::io::ErrorKind::WouldBlock {
                                acia_dbg!(red!("ACIA TCP write error: {}"), e);
                                break 'io_loop;
                            }
                        }
                        _ = stream.flush();
                    }
                }
                {
                    let mut ttyc = thread_tty_count.lock().unwrap();
                    *ttyc -= 1;
                }

                acia_dbg!(yellow!("ACIA TCP connection terminated. Listening at {}..."), addr);
            }
            Ok(())
        }));
        Ok(Acia {
            addr,
            handle,
            txout,
            rxin,
            recv_cache: RefCell::new(None),
            tty_count,
        })
    }
}
