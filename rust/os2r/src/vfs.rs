use alloc::{collections::BTreeMap, string::String, vec::Vec};
use serde::*;
use multiboot2::BootInformation;

#[derive(Debug, Deserialize)]
#[serde(untagged)]
pub enum Entry {
    Directory(String, BTreeMap<String, Entry>),
    File(String, usize, usize)
}

impl Entry {
    fn name(&self) -> &str {
        match self {
            Entry::Directory(name, _) => name.as_str(),
            Entry::File(name, _, _) => name.as_str()
        }
    }
}

static mut ROOT: Entry = Entry::File(String::new(), 0, 0);
static mut DATA: Vec<u8> = Vec::new();

pub fn init(boot_info: &BootInformation) {
    let module = trace!(boot_info.module_tags().find(|m| m.name() == "vfs").unwrap());
    let data = unsafe {
        core::slice::from_raw_parts(
            module.start_address() as usize as *const u8,
            (module.end_address() - module.start_address()) as usize
        )
    };
    
    let header_len = unsafe { *(&data[0] as *const u8 as *const usize) };
    let header = &data[4..][..header_len];

    unsafe {
        DATA.extend_from_slice(&data[(4 + header_len)..]);
        ROOT = serde_json::from_slice(header).unwrap();
    }
}

#[derive(Debug)]
pub enum FindError {
    NoSuchEntry(usize),
    NotADirectory(usize)
}

pub fn find(path: &str) -> Result<&'static Entry, FindError> {
    let mut entry = unsafe { &ROOT };
    let mut segnum = 0;
    for seg in path.trim_start_matches("/").split_terminator("/") {
        match entry {
            Entry::Directory(_, dir) => if let Some(next) = dir.get(seg) {
                entry = next;
                segnum += 1;
            } else {
                return Err(FindError::NoSuchEntry(segnum));
            },
            Entry::File(_, _, _) => return Err(FindError::NotADirectory(segnum))
        }
    }

    log!("found path {:?}", path);

    Ok(entry)
}

#[derive(Debug)]
pub struct Stat {
    pub is_dir: bool,
    pub len: usize,
    pub name: &'static str
}

pub fn stat(entry: &'static Entry) -> Stat {
    log!("stat for {:?}", entry.name());
    match entry {
        Entry::Directory(name, dir) => Stat {
            is_dir: true, len: dir.len(), name: name.as_str()
        },
        Entry::File(name, _, len) => Stat {
            is_dir: false, len: *len, name: name.as_str()
        }
    }
}

pub fn dirent(entry: &'static Entry, buf: &mut [usize], idx: usize) -> usize {
    match entry {
        Entry::File(_, _, _) => 0,
        Entry::Directory(_, dir) => {
            log!("dirent for {:?} at {}", entry.name(), idx);
            let mut out = 0;
            for entry in dir.values().skip(idx) {
                buf[out] = entry as *const Entry as usize;
                out += 1;
            }
            out
        }
    }
}

pub fn read(entry: &'static Entry, buf: &mut [u8], idx: usize) -> usize {
    match entry {
        Entry::Directory(_, _) => 0,
        Entry::File(_, offset, len) => {
            log!("reading from [{}:{}] at {}", *offset, *len, idx);
            let mut out = 0;
            for byte in unsafe { &DATA[*offset..][..*len][idx..] } {
                buf[out] = *byte;
                out += 1;
            }
            out
        }
    }
}