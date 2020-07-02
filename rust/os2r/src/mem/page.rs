use modular_bitfield::prelude::*;
use super::phys;

#[bitfield]
#[derive(Debug, Copy, Clone)]
pub struct PageEntry {
    pub present: bool,
    pub writeable: bool,
    pub user_accessible: bool,
    pub write_through: bool,
    pub cache_disable: bool,
    pub accessed: bool,
    pub dirty: bool,
    pub page_size: bool,
    pub global: bool,
    pub data: B3,
    pub address: B20
}

#[repr(align(4096))]
#[derive(Clone)]
pub struct PageDirectory {
    pub table: [PageEntry; 1024]
}

#[repr(align(4096))]
#[derive(Clone)]
pub struct PageTable {
    pub table: [PageEntry; 1024]
}

impl PageTable {
    pub fn new() -> PageTable {
        PageTable {
            table: [PageEntry::new(); 1024]
        }
    }
}

pub static mut KERNEL_PAGE_DIRECTORY: PageDirectory = PageDirectory {
    table: [PageEntry { data: [0; 4] }; 1024]
};

pub static mut KERNEL_PAGE_TABLES: [PageTable; 4] = [
    PageTable { table: [PageEntry { data: [0; 4] }; 1024] },
    PageTable { table: [PageEntry { data: [0; 4] }; 1024] },
    PageTable { table: [PageEntry { data: [0; 4] }; 1024] },
    PageTable { table: [PageEntry { data: [0; 4] }; 1024] }
];

const KERNEL_CODE_LIMIT: usize = 4 * 1024 * 1024;

extern "cdecl" {
    pub fn set_page_directory(ptr: u32);
    fn enable_paging();
}

pub fn init(end_of_kernel: u64, boot_info: &multiboot2::BootInformation, framebuffer: Option<(usize, usize)>) {
    trace!(assert!(end_of_kernel as usize <= KERNEL_CODE_LIMIT));
    trace!(assert!(boot_info.end_address() as usize <= KERNEL_CODE_LIMIT));
    if let Some((_, framebuffer_end)) = framebuffer {
        trace!(assert!(framebuffer_end <= KERNEL_CODE_LIMIT));
    }

    for module in boot_info.module_tags() {
        trace!(assert!(module.end_address() as usize <= KERNEL_CODE_LIMIT));
    }

    for j in 0..4 {
        let identity_pages = unsafe {
            &mut KERNEL_PAGE_DIRECTORY.table[j]
        };

        trace!(identity_pages.set_address(unsafe { &KERNEL_PAGE_TABLES[j] as *const _ as u32 } >> 12));
        identity_pages.set_writeable(true);
        identity_pages.set_present(true);
    }

    for i in 0..1024 {
        let identity_page = unsafe {
            &mut KERNEL_PAGE_TABLES[0].table[i]
        };

        trace!(identity_page.set_address(i as u32));
        identity_page.set_writeable(true);
        identity_page.set_present(true);
    }

    unsafe {
        set_page_directory(&KERNEL_PAGE_DIRECTORY as *const _ as u32);
        enable_paging();
    }

    log!("first 4MB identity mapped, 16MB prepared");
}

pub fn new_page_directory() -> PageDirectory {
    unsafe {
        KERNEL_PAGE_DIRECTORY.clone()
    }
}

pub fn resolve<T>(r: &T) -> phys::PhysicalAddress {
    let page = (r as *const T as usize) >> 12;
    let dir_idx = page >> 10;
    let tab_idx = page & 1023;

    assert!(dir_idx < 4);

    unsafe {
        phys::PhysicalAddress::new((KERNEL_PAGE_TABLES[dir_idx].table[tab_idx].get_address() as usize) << 12)
    }
}