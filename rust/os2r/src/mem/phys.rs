use core::{mem, num};

static mut BITMAP: PageBitmap = PageBitmap {
    bitmap: [0xffffffff; 32768],
    lowest_free: 32768
};

pub const PAGE_SIZE: usize = 4096;

#[derive(Debug, Copy, Clone)]
pub struct PhysicalAddress(num::NonZeroUsize);

impl PhysicalAddress {
    pub fn new(val: usize) -> PhysicalAddress {
        PhysicalAddress(num::NonZeroUsize::new(val).unwrap())
    }

    pub fn get(&self) -> usize {
        self.0.get()
    }
}

pub struct Page {
    ptr: PhysicalAddress
}

impl Drop for Page {
    fn drop(&mut self) {
        unsafe {
            BITMAP.free(self.ptr);
        }
    }
}

impl Page {
    pub fn physical_address(&self) -> PhysicalAddress {
        self.ptr
    }
}

struct PageBitmap {
    bitmap: [u32; 32768],
    lowest_free: usize
}

impl PageBitmap {
    fn mark_as_free(&mut self, start: usize, end: usize) {
        let page_start = (start + PAGE_SIZE - 1) / PAGE_SIZE;
        let page_end = end / PAGE_SIZE;

        let idx_start = page_start / 32;
        let rem_start = page_start % 32;

        if idx_start < self.lowest_free {
            self.lowest_free = idx_start;
        }

        let idx_end = page_end / 32;
        let rem_end = page_end % 32;

        if idx_start == idx_end {
            self.bitmap[idx_start] &= ((1 << rem_start) - 1) | !((1 << rem_end) - 1);
        } else {
            self.bitmap[idx_start] &= (1 << rem_start) - 1;
            self.bitmap[idx_end] &= !((1 << rem_end) - 1);
            for idx in (idx_start + 1)..idx_end {
                self.bitmap[idx] = 0;
            }
        }
    }

    fn mark_as_used(&mut self, start: usize, end: usize) {
        let page_start = start / PAGE_SIZE;
        let page_end = (end + PAGE_SIZE - 1) / PAGE_SIZE;

        let idx_start = page_start / 32;
        let rem_start = page_start % 32;

        let idx_end = page_end / 32;
        let rem_end = page_end % 32;

        if idx_start == idx_end {
            self.bitmap[idx_start] |= !((1 << rem_start) - 1) & ((1 << rem_end) - 1);
        } else {
            self.bitmap[idx_start] |= !((1 << rem_start) - 1);
            self.bitmap[idx_end] |= (1 << rem_end) - 1;
            for idx in (idx_start + 1)..idx_end {
                self.bitmap[idx] = 0xffffffff;
            }
        }

        if idx_start <= self.lowest_free && idx_end >= self.lowest_free {
            for i in self.lowest_free..32768 {
                if self.bitmap[i] != 0xffffffff {
                    self.lowest_free = i;
                    break;
                }
            }
        }
    }

    fn alloc(&mut self) -> Option<Page> {
        if self.lowest_free < 32768 {
            let idx_page = self.lowest_free;
            let rem_page = self.bitmap[idx_page].trailing_ones() as usize;
            self.bitmap[idx_page] |= 1 << rem_page;

            for i in self.lowest_free..32768 {
                if self.bitmap[i] != 0xffffffff {
                    self.lowest_free = i;
                    break;
                }
            }

            let ptr = PhysicalAddress::new((idx_page * 32 + rem_page) * PAGE_SIZE as usize);
            Some(Page { ptr })
        } else {
            None
        }
    }

    fn free(&mut self, page: PhysicalAddress) {
        let page = page.get() / PAGE_SIZE;

        let idx_page = page / 32;
        let rem_page = page % 32;

        if idx_page < self.lowest_free {
            self.lowest_free = idx_page;
        }

        self.bitmap[idx_page] &= !(1 << rem_page);
    }

    fn count_free(&self) -> usize {
        let mut total = 0;
        for i in self.lowest_free..32768 {
            total += self.bitmap[i].count_zeros() as usize;
        }
        total
    }
}

pub fn init(start_of_kernel: u64, end_of_kernel: u64, boot_info: &multiboot2::BootInformation, framebuffer: Option<(usize, usize)>) {
    let memory_map = trace!(boot_info.memory_map_tag().unwrap());
    for area in memory_map.memory_areas() {
        unsafe {
            BITMAP.mark_as_free(area.start_address() as usize, area.end_address() as usize);
        }
    }

    unsafe {
        BITMAP.mark_as_used(0, 1);
        BITMAP.mark_as_used(start_of_kernel as usize, end_of_kernel as usize);
        BITMAP.mark_as_used(boot_info.start_address(), boot_info.end_address());
        
        for module in boot_info.module_tags() {
            BITMAP.mark_as_used(module.start_address() as usize, module.end_address() as usize);
        }

        if let Some((start, end)) = framebuffer {
            BITMAP.mark_as_used(start, end)
        }
    }

    log!("free pages: {}, starting at 0x{:x}", unsafe { BITMAP.count_free() }, unsafe { BITMAP.lowest_free * 32 * PAGE_SIZE });
}

pub fn count_free() -> usize {
    unsafe {
        BITMAP.count_free()
    }
}

pub fn alloc() -> Option<Page> {
    unsafe {
        BITMAP.alloc()
    }
}

pub fn free(page: Page) {
    mem::drop(page)
}