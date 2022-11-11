use super::*;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]

pub enum AccessType {
    Program,
    UserStack,
    SystemStack,
    Generic,
    System,
}

impl Core {
    // reads one byte from RAM
    pub fn _read_u8(&self, atype: AccessType, addr: u16, data: Option<&mut u8>) -> Result<u8, Error> {
        if let Some(acia) = self.acia.as_ref() {
            if acia.owns_address(addr) {
                return acia.read(addr);
            }
        }
        if addr as usize >= self.mem.len() {
            return Err(Error::new(
                ErrorKind::Memory,
                None,
                format!(
                    "Out of bounds read. AccessType={:?}, Address={:04x}, RAM={:04x} bytes",
                    atype,
                    addr,
                    self.mem.len() as u16
                )
                .as_str(),
            ));
        }
        let byte = self.mem[addr as usize];
        if let Some(data) = data {
            *data = byte;
        }
        Ok(byte)
    }
    // helper version of _read_u8 that reads a byte into a u16
    pub fn _read_u8_as_u16(&self, atype: AccessType, addr: u16, data: Option<&mut u16>) -> Result<u16, Error> {
        let byte = self._read_u8(atype, addr, None)?;
        let word = byte as u16;
        if let Some(data) = data {
            *data = word
        }
        Ok(word)
    }
    // version of _read... for u16
    // reads two bytes as a u16 (high order byte first)
    pub fn _read_u16(&self, atype: AccessType, addr: u16, data: Option<&mut u16>) -> Result<u16, Error> {
        let mut b: [u8; 2] = [0, 0];
        self._read_u8(atype, addr, Some(&mut b[0]))?;
        self._read_u8(atype, addr + 1, Some(&mut b[1]))?;
        let word = (b[0] as u16) << 8 | (b[1] as u16);
        if let Some(data) = data {
            *data = word;
        }
        Ok(word)
    }
    // version of _read... for u8u16
    pub fn _read_u8u16(&self, atype: AccessType, addr: u16, size: u16) -> Result<u8u16, Error> {
        match size {
            1 => {
                let b = self._read_u8(atype, addr, None)?;
                Ok(u8u16::u8(b))
            }
            2 => {
                let w = self._read_u16(atype, addr, None)?;
                Ok(u8u16::u16(w))
            }
            _ => panic!("invalid read size for _read_u8u16"),
        }
    }
    //
    // writes
    //
    pub fn _write_u8u16(&mut self, atype: AccessType, addr: u16, data: u8u16) -> Result<(), Error> {
        let mut offset = 0u16;
        if let Some(msb) = data.msb() {
            self._write_u8(atype, addr, msb)?;
            offset += 1;
        }
        self._write_u8(atype, addr + offset, data.lsb())
    }
    pub fn _write_u8(&mut self, atype: AccessType, addr: u16, data: u8) -> Result<(), Error> {
        if let Some(acia) = self.acia.as_mut() {
            if acia.owns_address(addr) {
                return acia.write(addr, data);
            }
        }

        if addr > self.ram_top && atype != AccessType::System {
            warn!(
                "attempted write to read-only address: {:4x} (PC={:4x})",
                addr, self.reg.pc
            );
            return Ok(());
        }

        if addr as usize >= self.mem.len() {
            return Err(Error::new(
                ErrorKind::Memory,
                None,
                format!(
                    "Out of bounds write. AccessType={:?}, Address={:04x}, RAM={:04x} bytes",
                    atype,
                    addr,
                    self.mem.len() as u16
                )
                .as_str(),
            ));
        }
        if config::debug() {
            // if the debugger is enabled then check to see if this write should trigger a breakpoint
            self.debug_check_for_watch_hit(addr);
        }
        self.mem[addr as usize] = data;
        Ok(())
    }
}
