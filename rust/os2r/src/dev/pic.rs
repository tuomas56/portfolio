use super::port::Port;
use modular_bitfield::prelude::*;
pub use modular_bitfield::prelude::{B8, B3};

#[bitfield]
#[derive(Debug, Copy, Clone)]
pub struct ICW1 {
    pub icw4_needed: bool,
    pub single_mode: bool,
    pub interval_4: bool,
    pub level_triggered: bool,
    _res: bool,
    pub vector_address_low: B3
}

#[bitfield]
#[derive(Debug, Copy, Clone)]
pub struct ICW2 {
    pub vector_address_high: B8
}

#[bitfield]
#[derive(Debug, Copy, Clone)]
pub struct ICW3Master {
    pub slave_at_zero: bool,
    pub slave_at_one: bool,
    pub slave_at_two: bool,
    pub slave_at_three: bool,
    pub slave_at_four: bool,
    pub slave_at_five: bool,
    pub slave_at_six: bool,
    pub slave_at_seven: bool
}

#[bitfield]
#[derive(Debug, Copy, Clone)]
pub struct ICW3Slave {
    pub slave_id: B3,
    _res: B5
}

#[bitfield]
#[derive(Debug, Copy, Clone)]
pub struct ICW4 {
    pub mode_8086: bool,
    pub auto_eoi: bool,
    pub buffered_master: bool,
    pub buffered_mode: bool,
    pub nested_mode: bool,
    _res: B3
}

#[derive(Copy, Clone)]
pub union ICW3 {
    pub master: ICW3Master,
    pub slave: ICW3Slave
}

#[bitfield]
#[derive(Debug, Copy, Clone)]
pub struct IMR {
    pub mask_0: bool,
    pub mask_1: bool,
    pub mask_2: bool,
    pub mask_3: bool,
    pub mask_4: bool,
    pub mask_5: bool,
    pub mask_6: bool,
    pub mask_7: bool
}

#[derive(BitfieldSpecifier, Debug, Copy, Clone)]
pub enum Command {
    NonSpecificEOI = 1,
    SpecificEOI = 3,
    RotateNonSpecific = 5,
    SetRotateAutomatic = 4,
    ClearRotateAutomatic = 0,
    RotateSpecific = 7,
    SetPriority = 6,
    NoOperation = 2
}

#[bitfield]
#[derive(Debug, Copy, Clone)]
pub struct OCW2 {
    pub level: B3,
    _res: B2,
    #[bits = 3]
    pub command: Command
}

#[derive(BitfieldSpecifier, Copy, Clone, Debug)]
pub enum ReadType {
    ISR = 1,
    IRR = 0
}

#[bitfield]
#[derive(Debug, Copy, Clone)]
pub struct OCW3 {
    pub read_type: ReadType,
    pub read_register: bool,
    pub poll: bool,
    _res3: bool,
    _res2: bool,
    pub special_mask: bool,
    pub enable_special_mask: bool,
    _res1: bool
}

#[derive(Debug, Copy, Clone)]
pub enum OCW {
    OCW2(OCW2),
    OCW3(OCW3)
}

#[bitfield]
#[derive(Debug, Copy, Clone)]
pub struct ISR {
    pub level_0: bool,
    pub level_1: bool,
    pub level_2: bool,
    pub level_3: bool,
    pub level_4: bool,
    pub level_5: bool,
    pub level_6: bool,
    pub level_7: bool
}

#[bitfield]
#[derive(Debug, Copy, Clone)]
pub struct IRR {
    pub request_0: bool,
    pub request_1: bool,
    pub request_2: bool,
    pub request_3: bool,
    pub request_4: bool,
    pub request_5: bool,
    pub request_6: bool,
    pub request_7: bool
}

#[derive(Copy, Clone)]
union OCW23ICW1 {
    isr: ISR,
    irr: IRR,
    ocw2: OCW2,
    ocw3: OCW3,
    icw1: ICW1,
    data: u8
}

impl Into<u8> for OCW23ICW1 {
    fn into(self) -> u8 {
        unsafe { self.data }
    }
}

impl From<u8> for OCW23ICW1 {
    fn from(val: u8) -> Self {
        OCW23ICW1 { data: val }
    }
}

#[derive(Copy, Clone)]
union IMRICW234 {
    imr: IMR,
    icw2: ICW2,
    icw3: ICW3,
    icw4: ICW4,
    data: u8
}

impl Into<u8> for IMRICW234 {
    fn into(self) -> u8 {
        unsafe { self.data }
    }
}

impl From<u8> for IMRICW234 {
    fn from(val: u8) -> Self {
        IMRICW234 { data: val }
    }
}

pub struct PIC {
    command: Port<OCW23ICW1, u8>,
    data: Port<IMRICW234, u8>,
}

impl PIC {
    pub fn init(&self, mut icw1: ICW1, icw2: ICW2, icw3: ICW3, icw4: Option<ICW4>) {
        icw1.set__res(true);
        self.command.write(OCW23ICW1 { icw1 });
        self.command.wait();
        self.data.write(IMRICW234 { icw2 });
        self.data.wait();
        self.data.write(IMRICW234 { icw3 });
        self.data.wait();
        if let Some(icw4) = icw4 {
            self.data.write(IMRICW234 { icw4 });
            self.data.wait();
        }
    }

    pub fn send(&self, ocw: OCW) {
        match ocw {
            OCW::OCW2(ocw2) => {
                self.command.write(OCW23ICW1 { ocw2 });
                self.command.wait();
            },
            OCW::OCW3(mut ocw3) => {
                ocw3.set__res3(true);
                self.command.write(OCW23ICW1 { ocw3 });
                self.command.wait();
            }
        }
    }

    pub fn set_imr(&self, imr: IMR) {
        self.data.write(IMRICW234 { imr });
    }

    pub fn imr(&self) -> IMR {
        unsafe {
            self.data.read().imr
        }
    }

    pub fn isr(&self) -> ISR {
        unsafe {
            self.command.read().isr 
        }
    }

    pub fn irr(&self) -> IRR {
        unsafe {
            self.command.read().irr
        }
    }
}

pub const MASTER: PIC = PIC {
    command: Port::new(0x0020),
    data: Port::new(0x0021)
};

pub const SLAVE: PIC = PIC {
    command: Port::new(0x00a0),
    data: Port::new(0x00a1)
};