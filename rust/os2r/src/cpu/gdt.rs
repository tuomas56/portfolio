use core::mem;

struct GDTEntry {
    base: u32,
    limit: u32,
    entry_type: u8
}

impl GDTEntry {
    const fn encode(&self) -> [u8; 8] {
        if (self.limit > 65536) && (self.limit & 0xFFF) != 0xFFF {
            panic!("can't encode limit in GDT entry");
        }

        let mut target: [u8; 8] = [0; 8];
        target[0] = (self.limit & 0xFF) as u8;
        target[1] = ((self.limit >> 8) & 0xFF) as u8;
        target[6] = ((self.limit >> 16) & 0x0F) as u8;
        target[6] |= 0xC0;
        target[2] = (self.base & 0xFF) as u8;
        target[3] = ((self.base >> 8) & 0xFF) as u8;
        target[4] = ((self.base >> 16) & 0xFF) as u8;
        target[7] = ((self.base >> 24) & 0xFF) as u8;
        target[5] = self.entry_type;
        target
    }
}

const GDT: [[u8; 8]; 3] = [
    GDTEntry { 
        base: 0, 
        limit: 0, 
        entry_type: 0 
    }.encode(),

    GDTEntry {
        base: 0,
        limit: 0xffffffff,
        entry_type: 0x9a
    }.encode(),
    
    GDTEntry {
        base: 0,
        limit: 0xffffffff,
        entry_type: 0x92
    }.encode()
];

extern "cdecl" {
    fn load_gdt(ptr: u32, size: u16);
}

pub fn init() {
    unsafe {
        load_gdt(&GDT as *const _ as u32, mem::size_of_val(&GDT) as u16);
    }

    log!("GDT loaded, segmentation disabled");
}
