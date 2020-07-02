use goblin::elf32 as elf;

pub struct Elf<'a> {
    pub bytes: &'a [u8]
}

impl<'a> Elf<'a> {
    pub fn header(&self) -> Header<'a> {
        Header {
            bytes: self.bytes,
            header: unsafe {
                &*(self.bytes.as_ptr() as usize as *const elf::header::Header)
            }
        }
    }
}

pub struct Header<'a> {
    bytes: &'a [u8],
    pub header: &'a elf::header::Header
}

impl<'a> Header<'a> {
    pub fn entry_point(&self) -> usize {
        self.header.e_entry as usize
    }

    pub fn program_headers(&self) -> ProgramHeaders<'a> {
        ProgramHeaders {
            bytes: self.bytes,
            headers: unsafe {
                core::slice::from_raw_parts(
                    (&self.bytes[self.header.e_phoff as usize]) as *const u8
                        as usize as *const elf::program_header::ProgramHeader,
                    self.header.e_phnum as usize
                )
            },
            idx: 0
        }
    }
}

pub struct ProgramHeaders<'a> {
    bytes: &'a [u8],
    headers: &'a [elf::program_header::ProgramHeader],
    idx: usize
}

impl<'a> Iterator for ProgramHeaders<'a> {
    type Item = ProgramHeader<'a>;

    fn next(&mut self) -> Option<ProgramHeader<'a>> {
        if self.idx < self.headers.len() {
            self.idx += 1;
            Some(ProgramHeader {
                bytes: self.bytes,
                header: &self.headers[self.idx - 1]
            })
        } else {
            None
        }
    }
}

pub struct ProgramHeader<'a> {
    bytes: &'a [u8],
    header: &'a elf::program_header::ProgramHeader
}

impl<'a> ProgramHeader<'a> {
    pub fn is_load(&self) -> bool {
        self.header.p_type == elf::program_header::PT_LOAD
    }

    pub fn virtual_address(&self) -> usize {
        self.header.p_vaddr as usize
    }

    pub fn memory_size(&self) -> usize {
        self.header.p_memsz as usize
    }

    pub fn data(&self) -> &[u8] {
        &self.bytes[self.header.p_offset as usize..][..self.header.p_filesz as usize]
    }

    pub fn is_write(&self) -> bool {
        (self.header.p_flags & elf::program_header::PF_W) != 0
    }
}