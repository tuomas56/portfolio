use crate::cpu::isr::{Registers, InterruptFrame};
use crate::mem::page::{self, PageDirectory, PageTable};
use crate::mem::phys::{self, Page};
use alloc::{boxed::Box, vec::Vec, collections::BTreeMap, string::String};

pub struct AddressSpace {
    directory: Box<PageDirectory>,
    tables: BTreeMap<usize, Box<PageTable>>,
    pages: Vec<Page>,
    activated: bool
}

impl AddressSpace {
    pub fn new() -> AddressSpace {
        AddressSpace {
            directory: Box::new(page::new_page_directory()),
            tables: BTreeMap::new(),
            pages: Vec::new(),
            activated: false
        }
    }

    pub fn alloc(&mut self, start: usize, len: usize) -> Option<&mut [u8]> {
        let start_page = start >> 12;
        let end_page = (start + len) >> 12;
        
        for page in start_page..=end_page {
            self.alloc_page(page);
        }

        if self.activated {
            Some(unsafe {
                core::slice::from_raw_parts_mut(start as *mut u8, len)
            })
        } else {
            None
        }
    }

    fn alloc_page(&mut self, page: usize) {
        let dir_idx = page >> 10;
        assert!(dir_idx >= 4);
        let tab_idx = page & 1023;
        
        let phys_page = phys::alloc().unwrap();

        let table = self.tables.entry(dir_idx).or_insert_with(|| {
            Box::new(PageTable::new())
        });
        
        table.table[tab_idx].set_address(phys_page.physical_address().get() as u32 >> 12);
        table.table[tab_idx].set_writeable(true);
        table.table[tab_idx].set_present(true);

        self.directory.table[dir_idx].set_address(page::resolve(table.as_ref()).get() as u32 >> 12);
        self.directory.table[dir_idx].set_writeable(true);
        self.directory.table[dir_idx].set_present(true);

        self.pages.push(phys_page);

        if self.activated {
            unsafe {
                llvm_asm!("invlpg [$0]" :: "r"(page << 12) :: "intel");
            }
        }
    }

    pub fn activate(&mut self) {
        self.activated = true;

        unsafe {
            page::set_page_directory(page::resolve(self.directory.as_ref()).get() as u32);
        }
    }
}

pub struct Process {
    pub state: Option<(Registers, InterruptFrame)>,
    pub memory: AddressSpace,
    pub cmdline: String
}