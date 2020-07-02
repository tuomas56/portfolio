use crate::dev::pic;
use core::{mem, fmt};

struct IDTEntry {
    base: u32,
    sel: u16,
    flags: u8
}

impl IDTEntry {
    const fn encode(self) -> [u8; 8] {
        let mut target = [0; 8];
        target[0] = (self.base & 0xFF) as u8;
        target[1] = ((self.base >> 8) & 0xFF) as u8;
        target[2] = (self.sel & 0xFF) as u8;
        target[3] = ((self.sel >> 8) & 0xFF) as u8;
        target[5] = self.flags;
        target[6] = ((self.base >> 16) & 0xFF) as u8;
        target[7] = ((self.base >> 24) & 0xFF) as u8;
        target
    }
}

macro_rules! idt {
    ($i:ident, $fi:ident, $($f:ident),+) => {
        static mut $i: [[u8; 8]; 256] = [[0; 8]; 256];
        
        #[allow(unused_assignments)]
        fn $fi() {
            let mut i = 0;
            unsafe {
                $(
                    $i[i] = IDTEntry {
                        base: $f as *const extern "x86-interrupt" fn() as u32,
                        sel: 0x08,
                        flags: 0x8e
                    }.encode();
                    i += 1;
                )+
            }   
        }

        extern "cdecl" {
            $(
                fn $f();
            )+
        }
    }
}

idt!(IDT, init_idt,
      int0,   int1,   int2,   int3,   int4,   int5,   int6,   int7,   int8,   int9,  int10,  int11,  int12,  int13,  int14,  int15,
     int16,  int17,  int18,  int19,  int20,  int21,  int22,  int23,  int24,  int25,  int26,  int27,  int28,  int29,  int30,  int31,
     int32,  int33,  int34,  int35,  int36,  int37,  int38,  int39,  int40,  int41,  int42,  int43,  int44,  int45,  int46,  int47,
     int48,  int49,  int50,  int51,  int52,  int53,  int54,  int55,  int56,  int57,  int58,  int59,  int60,  int61,  int62,  int63,
     int64,  int65,  int66,  int67,  int68,  int69,  int70,  int71,  int72,  int73,  int74,  int75,  int76,  int77,  int78,  int79,
     int80,  int81,  int82,  int83,  int84,  int85,  int86,  int87,  int88,  int89,  int90,  int91,  int92,  int93,  int94,  int95,
     int96,  int97,  int98,  int99, int100, int101, int102, int103, int104, int105, int106, int107, int108, int109, int110, int111,
    int112, int113, int114, int115, int116, int117, int118, int119, int120, int121, int122, int123, int124, int125, int126, int127,
    int128, int129, int130, int131, int132, int133, int134, int135, int136, int137, int138, int139, int140, int141, int142, int143,
    int144, int145, int146, int147, int148, int149, int150, int151, int152, int153, int154, int155, int156, int157, int158, int159,
    int160, int161, int162, int163, int164, int165, int166, int167, int168, int169, int170, int171, int172, int173, int174, int175,
    int176, int177, int178, int179, int180, int181, int182, int183, int184, int185, int186, int187, int188, int189, int190, int191,
    int192, int193, int194, int195, int196, int197, int198, int199, int200, int201, int202, int203, int204, int205, int206, int207,
    int208, int209, int210, int211, int212, int213, int214, int215, int216, int217, int218, int219, int220, int221, int222, int223,
    int224, int225, int226, int227, int228, int229, int230, int231, int232, int233, int234, int235, int236, int237, int238, int239,
    int240, int241, int242, int243, int244, int245, int246, int247, int248, int249, int250, int251, int252, int253, int254, int255
);

extern "cdecl" {
    fn load_idt(ptr: u32, size: u16);
}

#[repr(C)]
#[derive(Clone, Default)]
pub struct InterruptFrame {
    pub eip: u32,
    pub cs: u32,
    pub eflags: u32,
    pub esp: u32
}

impl fmt::Debug for InterruptFrame {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            write!(f, "\x1bfdInterruptFrame \x1bff{{
       \x1bfaeip\x1bf7: \x1bf90x{:08x}\x1bf7,
        \x1bfacs\x1bf7: \x1bf9{}\x1bf7,
    \x1bfaeflags\x1bf7: \x1bf90b{:022b}\x1bff
}}", self.eip, self.cs, self.eflags)
        } else {
            write!(f, "InterruptFrame {{ eip: {}, cs: {}, eflags: {} }}", self.eip, self.cs, self.eflags)
        }
    }
}

#[repr(C)]
#[derive(Clone, Default)]
pub struct Registers {
    pub eax: u32,
    pub ebx: u32,
    pub ecx: u32,
    pub edx: u32,
    pub esi: u32,
    pub edi: u32
}

impl fmt::Debug for Registers {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            write!(f, "\x1bfdRegisters \x1bff{{
    \x1bfaeax\x1bf7: \x1bf90x{:08x} ({})\x1bf7,
    \x1bfaebx\x1bf7: \x1bf90x{:08x} ({})\x1bf7,
    \x1bfaecx\x1bf7: \x1bf90x{:08x} ({})\x1bf7,
    \x1bfaedx\x1bf7: \x1bf90x{:08x} ({})\x1bf7,
    \x1bfaesi\x1bf7: \x1bf90x{:08x} ({})\x1bf7,
    \x1bfaedi\x1bf7: \x1bf90x{:08x} ({})\x1bf7
\x1bff}}", self.eax, self.eax, self.ebx, self.ebx, self.ecx, self.ecx, self.edx, self.edx, self.esi, self.esi, self.edi, self.edi)
        } else {
            write!(f, "InterruptFrame {{ eax: {}, ebx: {}, ecx: {}, edx: {}, esi: {}, edi: {} }}", self.eax, self.ebx, self.ecx, self.edx, self.esi, self.edi)
        }
    }
}

pub type Handler = fn(&mut InterruptFrame, Option<u32>, &mut Registers);

static mut HANDLERS: [Option<Handler>; 256] = [None; 256];

#[no_mangle]
pub extern "C" fn main_handler(num: u32, mut regs: Registers, mut frame: InterruptFrame) {
    handle_isr(num, &mut regs, None, &mut frame);
}

#[no_mangle]
pub extern "C" fn main_handler_err(num: u32, mut regs: Registers, error_code: u32, mut frame: InterruptFrame) {
    handle_isr(num, &mut regs, Some(error_code), &mut frame);
}

fn handle_isr(num: u32, regs: &mut Registers, error_code: Option<u32>, frame: &mut InterruptFrame) {
    let num = num as u8;
    if num == 39 {
        let mut ocw3 = pic::OCW3::new();
        ocw3.set_read_register(true);
        ocw3.set_read_type(pic::ReadType::ISR);
        pic::MASTER.send(pic::OCW::OCW3(ocw3));
        let isr = pic::MASTER.isr();
        if !isr.get_level_7() {
            return
        }
    } else if num == 47 {
        let mut ocw3 = pic::OCW3::new();
        ocw3.set_read_register(true);
        ocw3.set_read_type(pic::ReadType::ISR);
        pic::SLAVE.send(pic::OCW::OCW3(ocw3));
        let isr = pic::SLAVE.isr();
        if !isr.get_level_7() {
            let mut ocw2 = pic::OCW2::new();
            ocw2.set_command(pic::Command::NonSpecificEOI);
            pic::MASTER.send(pic::OCW::OCW2(ocw2));
            return
        }
    }

    unsafe {
        if let Some(handler) = HANDLERS[num as usize] {
            handler(frame, error_code, regs);
        }
    }

    if (num >= 32) && (num < 48) {
        if num >= 40 {
            let mut ocw2 = pic::OCW2::new();
            ocw2.set_command(pic::Command::NonSpecificEOI);
            pic::SLAVE.send(pic::OCW::OCW2(ocw2));
        }
        let mut ocw2 = pic::OCW2::new();
        ocw2.set_command(pic::Command::NonSpecificEOI);
        pic::MASTER.send(pic::OCW::OCW2(ocw2));
    }
}

pub fn push_handler<F: FnOnce(Option<Handler>) -> Handler>(int: u8, chain: F) {
    unsafe {
        HANDLERS[int as usize] = Some(chain(HANDLERS[int as usize]));
    }
}

#[macro_export]
macro_rules! isr_handler {
    {
        $v:vis mod $s:ident {
            $(#[$m:meta])*
            fn $f:ident($a:ident: &mut InterruptFrame, $e:ident: Option<u32>, $r:ident: &mut Registers) $c:block
        }
    } => {
        $v mod $s {
            use crate::cpu::isr::*;
            static mut PREV_HANDLER: Option<Handler> = None;

            $(#[$m])*
            fn actual_handler($a: &mut InterruptFrame, $e: Option<u32>, $r: &mut Registers) {
                $c;

                unsafe {
                    if let Some(prev) = PREV_HANDLER {
                        prev($a, $e, $r);
                    }
                }
            }
            
            pub fn $f(prev: Option<Handler>) -> Handler {
                unsafe {
                    PREV_HANDLER = prev;
                }

                actual_handler
            }
        }
    }
}

pub fn enable_irq(irq: usize) {
    if irq < 8 {
        let mut imr = pic::MASTER.imr();
        match irq {
            0 => imr.set_mask_0(false),
            1 => imr.set_mask_1(false),
            2 => imr.set_mask_2(false),
            3 => imr.set_mask_3(false),
            4 => imr.set_mask_4(false),
            5 => imr.set_mask_5(false),
            6 => imr.set_mask_6(false),
            7 => imr.set_mask_7(false), 
            _ => return
        }
        pic::MASTER.set_imr(imr);
    } else if irq < 16 {
        let mut imr = pic::SLAVE.imr();
        match irq - 8 {
            0 => imr.set_mask_0(false),
            1 => imr.set_mask_1(false),
            2 => imr.set_mask_2(false),
            3 => imr.set_mask_3(false),
            4 => imr.set_mask_4(false),
            5 => imr.set_mask_5(false),
            6 => imr.set_mask_6(false),
            7 => imr.set_mask_7(false), 
            _ => return
        }
        pic::SLAVE.set_imr(imr);
    }
}

pub fn disable_irq(irq: usize) {
    if irq < 8 {
        let mut imr = pic::MASTER.imr();
        match irq {
            0 => imr.set_mask_0(true),
            1 => imr.set_mask_1(true),
            2 => imr.set_mask_2(true),
            3 => imr.set_mask_3(true),
            4 => imr.set_mask_4(true),
            5 => imr.set_mask_5(true),
            6 => imr.set_mask_6(true),
            7 => imr.set_mask_7(true), 
            _ => return
        }
        pic::MASTER.set_imr(imr);
    } else if irq < 16 {
        let mut imr = pic::SLAVE.imr();
        match irq - 8 {
            0 => imr.set_mask_0(true),
            1 => imr.set_mask_1(true),
            2 => imr.set_mask_2(true),
            3 => imr.set_mask_3(true),
            4 => imr.set_mask_4(true),
            5 => imr.set_mask_5(true),
            6 => imr.set_mask_6(true),
            7 => imr.set_mask_7(true), 
            _ => return
        }
        pic::SLAVE.set_imr(imr);
    }
}

pub fn init() {
    unsafe {
        llvm_asm!("cli");
    }

    let mut icw1 = pic::ICW1::new();
    icw1.set_icw4_needed(true);
    let mut icw2 = pic::ICW2::new();
    icw2.set_vector_address_high(32);
    let mut icw3 = pic::ICW3Master::new();
    icw3.set_slave_at_two(true);
    let mut icw4 = pic::ICW4::new();
    icw4.set_mode_8086(true);
    pic::MASTER.init(icw1, icw2, pic::ICW3 { master: icw3 }, Some(icw4));

    let mut icw2 = pic::ICW2::new();
    icw2.set_vector_address_high(40);
    let mut icw3 = pic::ICW3Slave::new();
    icw3.set_slave_id(2);
    pic::SLAVE.init(icw1, icw2, pic::ICW3 { slave: icw3 }, Some(icw4));

    let mut imr = pic::IMR::new();
    imr.set_mask_0(true);
    imr.set_mask_1(true);
    imr.set_mask_2(false);
    imr.set_mask_3(true);
    imr.set_mask_4(true);
    imr.set_mask_5(true);
    imr.set_mask_6(true);
    imr.set_mask_7(true);
    pic::MASTER.set_imr(imr);
    imr.set_mask_2(true);
    pic::SLAVE.set_imr(imr);

    init_idt();
    unsafe {
        load_idt(&IDT as *const [[u8; 8]; 256] as u32, mem::size_of_val(&IDT) as u16);
        llvm_asm!("sti");
    }

    log!("initialized, all masked except slave");
}