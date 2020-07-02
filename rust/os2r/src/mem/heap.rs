use super::{phys, page};
use core::{ptr::NonNull, mem, cell::UnsafeCell};
use alloc::alloc::{GlobalAlloc, Layout};

const KERNEL_HEAP_START: usize = 4 * 1024 * 1024;
const KERNEL_HEAP_END: usize = 16 * 1024 * 1024;

const fn bucket_size(n: usize) -> usize {
    (phys::PAGE_SIZE - 2 * mem::size_of::<usize>()) / (n + mem::size_of::<usize>())
}

struct Bucket<const N: usize, const K: usize> {
    head: Option<NonNull<BucketPage<N, K>>>
}

impl<const N: usize, const K: usize> Bucket<N, K> {
    fn alloc(&mut self) -> Option<*mut u8> {
        if let Some(head) = self.head.as_mut() {
            unsafe {
                head.as_mut().alloc()
            }
        } else {
            unsafe {
                self.head.get_or_insert(BucketPage::new()?).as_mut().alloc()
            }
        }
    }

    fn free(&mut self, ptr: *mut u8) {
        unsafe {
            self.head.as_mut()
                .expect("cant free from empty bucket")
                .as_mut().free(ptr)
        }
    }
}

struct BucketPage<const N: usize, const K: usize> {
    next: Option<NonNull<BucketPage<N, K>>>,
    index: usize,
    free: [usize; K],
    data: [[u8; N]; K]
}

impl<const N: usize, const K: usize> BucketPage<N, K> {
    fn new() -> Option<NonNull<BucketPage<N, K>>> {
        let mut new_page = unsafe {
            assert!(mem::size_of::<BucketPage<N, K>>() <= phys::PAGE_SIZE);
            NonNull::new_unchecked(KERNEL_HEAP.0.get().as_mut()?.alloc_region(1)? as usize as *mut BucketPage<N, K>)
        };

        unsafe {
            new_page.as_mut().next = None;
            new_page.as_mut().index = 0;
            for i in 0..K {
                new_page.as_mut().free[i] = i;
            }
        }

        Some(new_page)
    }

    fn alloc(&mut self) -> Option<*mut u8> {
        if self.index < K {
            let res = (&self.data[self.free[self.index]]) as *const _ as usize as *mut _;
            self.index += 1;
            Some(res)
        } else if let Some(next) = self.next.as_mut() {
            unsafe {
                next.as_mut().alloc()
            }
        } else {
            unsafe {
                self.next.get_or_insert(BucketPage::new()?).as_mut().alloc()
            }
        }
    }

    fn free(&mut self, ptr: *mut u8) {
        let data_base = (&self.data) as *const _ as usize;
        let ptr = ptr as usize;

        if ptr >= data_base && (ptr - data_base) / N < K {
            assert!(self.index > 0, "double free in bucket");
            assert!((ptr - data_base) % N == 0, "bad alignment in bucket pointer");
            self.index -= 1;
            self.free[self.index] = (ptr - data_base) / N;
        } else {
            unsafe {
                self.next.as_mut()
                    .expect("pointer doesnt come from this bucket!")
                    .as_mut().free(ptr as *mut u8);
            }
        }
    }
}

struct Region {
    next: Option<NonNull<Region>>,
    prev_next: NonNull<Option<NonNull<Region>>>,
    len: usize
}

impl Region {
    fn alloc(&mut self, pages: usize) -> Option<*mut u8> {
        unsafe {
            if self.len == pages {
                *self.prev_next.as_mut() = self.next;
                if let Some(mut next) = self.next {
                    next.as_mut().prev_next = self.prev_next;
                }
                Some(self as *mut _ as usize as *mut u8)
            } else if self.len > pages {
                self.len -= pages;
                Some((self as *mut _ as usize + self.len * phys::PAGE_SIZE) as *mut u8)
            } else {
                self.next?.as_mut().alloc(pages)
            }
        }
    }
}

struct KernelHeap {
    buckets: (
        Bucket<4, {bucket_size(4)}>,
        Bucket<8, {bucket_size(8)}>,
        Bucket<16, {bucket_size(16)}>,
        Bucket<32, {bucket_size(32)}>,
        Bucket<64, {bucket_size(64)}>,
        Bucket<128, {bucket_size(128)}>,
        Bucket<256, {bucket_size(256)}>,
        Bucket<512, {bucket_size(512)}>,
        Bucket<1024, {bucket_size(1024)}>
    ),
    regions: Option<NonNull<Region>>
}

impl KernelHeap {
    fn alloc_bytes(&mut self, bytes: usize) -> Option<*mut u8> {
        if bytes <= 4 {
            self.buckets.0.alloc()
        } else if bytes <= 8 {
            self.buckets.1.alloc()
        } else if bytes <= 16 {
            self.buckets.2.alloc()
        } else if bytes <= 32 {
            self.buckets.3.alloc()
        } else if bytes <= 64 {
            self.buckets.4.alloc()
        } else if bytes <= 128 {
            self.buckets.5.alloc()
        } else if bytes <= 256 {
            self.buckets.6.alloc()
        } else if bytes <= 512 {
            self.buckets.7.alloc()
        } else if bytes <= 1024 {
            self.buckets.8.alloc()
        } else {
            self.alloc_region((bytes + phys::PAGE_SIZE - 1) / phys::PAGE_SIZE)
        }
    }

    fn free_bytes(&mut self, ptr: *mut u8, bytes: usize) {
        if bytes <= 4 {
            self.buckets.0.free(ptr)
        } else if bytes <= 8 {
            self.buckets.1.free(ptr)
        } else if bytes <= 16 {
            self.buckets.2.free(ptr)
        } else if bytes <= 32 {
            self.buckets.3.free(ptr)
        } else if bytes <= 64 {
            self.buckets.4.free(ptr)
        } else if bytes <= 128 {
            self.buckets.5.free(ptr)
        } else if bytes <= 256 {
            self.buckets.6.free(ptr)
        } else if bytes <= 512 {
            self.buckets.7.free(ptr)
        } else if bytes <= 1024 {
            self.buckets.8.free(ptr)
        } else {
            self.free_region(ptr, (bytes + phys::PAGE_SIZE - 1) / phys::PAGE_SIZE)
        }
    }

    fn alloc_region(&mut self, pages: usize) -> Option<*mut u8> {
        unsafe {
            self.regions?.as_mut().alloc(pages)
        }
    }

    fn free_region(&mut self, ptr: *mut u8, pages: usize) {
        unsafe {
            let mut region = NonNull::new_unchecked(ptr as *mut Region);
            region.as_mut().len = pages;
            region.as_mut().prev_next = NonNull::new_unchecked(&mut self.regions as *mut _);
            region.as_mut().next = None;
            if let Some(mut head) = self.regions.take() {
                head.as_mut().prev_next = NonNull::new_unchecked(&mut region.as_mut().next as *mut _);
                region.as_mut().next = Some(head);
            }
            self.regions = Some(region);
        }
    }
}

struct KernelHeapAllocator(UnsafeCell<KernelHeap>);

unsafe impl GlobalAlloc for KernelHeapAllocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        assert!(layout.align() <= 4096, "cant align to more than 4096");
        self.0.get().as_mut().unwrap().alloc_bytes(layout.pad_to_align().size()).expect("out of memory")
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        self.0.get().as_mut().unwrap().free_bytes(ptr, layout.pad_to_align().size())
    }
}

#[global_allocator]
static mut KERNEL_HEAP: KernelHeapAllocator = KernelHeapAllocator(UnsafeCell::new(KernelHeap {
    buckets: (
        Bucket { head: None },
        Bucket { head: None },
        Bucket { head: None },
        Bucket { head: None },
        Bucket { head: None },
        Bucket { head: None },
        Bucket { head: None },
        Bucket { head: None },
        Bucket { head: None }
    ),
    regions: None
}));

#[alloc_error_handler]
fn alloc_error(layout: Layout) -> ! {
    panic!("allocation error for layout: {:?}", layout);
}

pub fn init() {
    for i in 1..4 {
        for j in 0..1024 {
            let entry = unsafe {
                &mut page::KERNEL_PAGE_TABLES[i].table[j]
            };

            let page = phys::alloc().unwrap();

            entry.set_address((page.physical_address().get() >> 12) as u32);
            entry.set_writeable(true);
            entry.set_present(true);

            core::mem::forget(page);

            unsafe {
                llvm_asm!("invlpg [$0]" :: "r"(phys::PAGE_SIZE * (j + 1024 * i)) :: "intel");
            }
        }
    }

    unsafe {
        let mut heap = KERNEL_HEAP.0.get().as_mut().unwrap();
        let mut region = NonNull::new_unchecked(KERNEL_HEAP_START as *mut Region);
        region.as_mut().len = (KERNEL_HEAP_END - KERNEL_HEAP_START) / phys::PAGE_SIZE;
        region.as_mut().prev_next = NonNull::new_unchecked(&mut heap.regions as *mut _);
        region.as_mut().next = None;
        heap.regions = Some(region);
    }

    log!("12MB kernel heap allocated, {} pages remaining", phys::count_free());
}