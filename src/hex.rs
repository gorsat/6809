#![allow(non_snake_case, non_upper_case_globals, unused)]

//! The hex file format seems to be a popular way to move microprocessor machine code around
//! so I've added this quick and dirty implementation for reading and writing these files.
//!   
//! This implementation is based on the specification of I8HEX described in
//! [this wikipedia article](https://en.wikipedia.org/wiki/Intel_HEX).

use regex::Regex;
use std::fmt::{self, Display};
use std::fs::File;
use std::io;
use std::io::{BufRead, BufReader};
use std::path::Path;

pub mod HexRecordType {
    // This is an implementation of I8HEX so only the Data and EndOffile record types are supported
    pub const Data: u8 = 0;
    pub const EndOfFile: u8 = 1;
    pub const ExSegAddr: u8 = 2;
    pub const StartSegAddr: u8 = 3;
    pub const ExLinAddr: u8 = 4;
    pub const StartLinAddr: u8 = 5;
}
pub struct HexRecord {
    pub data_size: u8,
    pub address: u16,
    pub record_type: u8,
    pub data: Option<Vec<u8>>,
    pub checksum: u8,
}
impl Display for HexRecord {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut dstr = String::new();
        if let Some(data) = &self.data {
            use fmt::Write;
            data.iter().for_each(|&b| _ = write!(dstr, "{:02x}", b));
        }
        writeln!(
            f,
            ":{:02x}{:04x}{:02x}{dstr}{:02x}",
            self.data_size, self.address, self.record_type, self.checksum
        )
    }
}
use super::Error;
impl HexRecord {
    pub fn from_data(address: u16, data: &[u8]) -> Self {
        let mut h = HexRecord {
            data_size: data.len() as u8,
            address,
            record_type: HexRecordType::Data,
            data: Some(data.to_vec()),
            checksum: 0,
        };
        h.checksum = h.calc_checksum().expect("should be impossible");
        h
    }
    pub fn from_str<S: AsRef<str>>(s: S) -> Result<Option<Self>, ()> {
        let re = Regex::new(r"(?i)^.*:([0-9a-f]{2})([0-9a-f]{4})([0-9a-f]{2})((?:[0-9a-f]{2})*)([0-9a-f]{2})")
            .map_err(|_| ())?;
        if let Some(c) = re.captures(s.as_ref()) {
            Ok(Some(Self::from_captures(&c).ok_or(())?))
        } else {
            Ok(None)
        }
    }
    pub fn from_captures(c: &regex::Captures) -> Option<Self> {
        let data_size = u8::from_str_radix(c.get(1)?.as_str(), 16).ok()?;
        let h = HexRecord {
            data_size,
            address: u16::from_str_radix(c.get(2)?.as_str(), 16).ok()?,
            record_type: u8::from_str_radix(c.get(3)?.as_str(), 16).ok()?,
            data: HexRecord::data_from_str(c.get(4)?.as_str(), data_size),
            checksum: u8::from_str_radix(c.get(5)?.as_str(), 16).ok()?,
        };
        h.calc_checksum().filter(|&c| c == h.checksum).map(|_| h)
    }
    fn data_from_str(s: &str, byte_count: u8) -> Option<Vec<u8>> {
        if byte_count == 0 || s.len() < (2 * byte_count) as usize {
            return None;
        }
        let mut data: Vec<u8> = Vec::with_capacity(byte_count as usize);
        let mut i = 0u8;
        while i < byte_count {
            data.push(u8::from_str_radix(&s[(i * 2) as usize..((i + 1) * 2) as usize], 16).ok()?);
            i += 1;
        }
        Some(data)
    }
    fn calc_checksum(&self) -> Option<u8> {
        let mut sum = 0u16;
        sum += self.data_size as u16;
        sum += self.address >> 8;
        sum += self.address & 0xff;
        sum += self.record_type as u16;
        if let Some(data) = self.data.as_ref() {
            data.iter().for_each(|&b| sum += b as u16);
            if data.len() != self.data_size as usize {
                return None;
            }
        }
        Some((sum as u8).wrapping_neg())
    }
}

pub struct HexRecordCollection {
    records: Vec<HexRecord>,
    eof: bool,
}

impl HexRecordCollection {
    pub fn new() -> Self {
        HexRecordCollection {
            records: Vec::new(),
            eof: false,
        }
    }
    pub fn from_str_iter<I, T>(iter: I) -> Result<Self, Error>
    where
        I: IntoIterator<Item = T>,
        T: Into<String>,
    {
        let mut hf = HexRecordCollection {
            records: Vec::new(),
            eof: false,
        };
        for s in iter {
            let hr = HexRecord::from_str(s.into()).map_err(|_| general_err!("failed to parse hex file"))?;
            if let Some(hr) = hr {
                hf.add_record(hr)?
            }
        }
        if hf.eof {
            Ok(hf)
        } else {
            Err(general_err!("EOF record not found in hex file"))
        }
    }
    pub fn add_record(&mut self, h: HexRecord) -> Result<(), Error> {
        if self.eof {
            return Err(general_err!("records after EOF in hex file"));
        }
        if h.record_type == HexRecordType::EndOfFile {
            self.eof = true
        }
        self.records.push(h);
        Ok(())
    }
    pub fn add_eof(&mut self) {
        self.add_record(HexRecord {
            data_size: 0,
            address: 0,
            record_type: HexRecordType::EndOfFile,
            data: None,
            checksum: 0xff,
        });
    }
    pub fn read_from_file(path: &Path) -> Result<Self, Error> {
        let file = BufReader::new(File::open(path)?)
            .lines()
            .collect::<Result<Vec<String>, io::Error>>()?;
        HexRecordCollection::from_str_iter(file)
    }
    pub fn write_to_file(&self, f: &mut dyn io::Write) -> Result<(), Error> {
        if !self.eof {
            return Err(general_err!("cannot write hex file without EOF record"));
        }
        for r in self.iter() {
            f.write_all(r.to_string().as_bytes())?;
        }
        Ok(())
    }
}

use std::ops::{Deref, DerefMut};
impl Deref for HexRecordCollection {
    type Target = Vec<HexRecord>;
    fn deref(&self) -> &Self::Target { &self.records }
}
