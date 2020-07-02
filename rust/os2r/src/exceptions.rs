use core::fmt;

pub fn read_cr2() -> u32 {
    let cr2: u32;
    unsafe {
        llvm_asm!("mov eax, cr2" : "={eax}"(cr2) :: "eax" : "intel");   
    }
    cr2
}

#[derive(Debug)]
struct Exception<A, B, C> {
    error: A,
    frame: B,
    registers: C
}

macro_rules! gen_exception_fn {
    ($sname:literal $name:ident) => {
        isr_handler! {
            mod $name {
                #[allow(unreachable_code)]
                fn handler(frame: &mut InterruptFrame, err_code: Option<u32>, regs: &mut Registers) {
                    log!(@error "{}\x1bff\n{:#?}", $sname, super::Exception { error: err_code, frame, registers: regs });
                    panic!("unrecoverable exception");
                }
            }
        }
    };
    ($sname:literal $name:ident [$err_fn:ident]) => {
        isr_handler! {
            mod $name {
                #[allow(unreachable_code)]
                fn handler(frame: &mut InterruptFrame, err_code: Option<u32>, regs: &mut Registers) {
                    log!(@error "{}\x1bff\n{:#?}", $sname, super::Exception { error: super::$err_fn(err_code), frame, registers: regs });
                    panic!("unrecoverable exception");
                }
            }
        }
    }
}

macro_rules! gen_exception_init {
    ($num:literal, $name:ident) => {
        crate::cpu::isr::push_handler($num, $name::handler);
    }
}

macro_rules! exceptions {
    { $init:ident; $($num:literal $sname:literal => $name:ident $([$err_fn:ident])?),+ } => {
        $(gen_exception_fn!($sname $name $([$err_fn])?);)+
        
        pub fn $init() {
            $(gen_exception_init!($num, $name);)+
        }
    }
}

#[derive(Debug)]
enum SelectorTable {
    GDT,
    IDT,
    LDT
}

struct SelectorError {
    external: bool,
    table: SelectorTable,
    index: u32
}

impl fmt::Debug for SelectorError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            write!(f, "\x1bfdSelectorError \x1bff{{
        \x1bfaexternal\x1bf7: \x1bf9{}\x1bf7,
           \x1bfatable\x1bf7: \x1bf9{:?}\x1bf7,
           \x1bfaindex\x1bf7: \x1bf9{}\x1bf7,
    \x1bff}}", self.external, self.table, self.index)
        } else {
            write!(f, "SelectorError {{ external: {}, table: {:?}, index: {} }}", self.external, self.table, self.index)
        }
    }
}

fn selector_error(err: Option<u32>) -> SelectorError {
    let err = err.unwrap_or(0);
    let external = err & 1 == 1;
    let table = match err & 0b110 {
        0b000 => SelectorTable::GDT,
        0b010 | 0b110 => SelectorTable::IDT,
        0b100 => SelectorTable::LDT,
        _ => unreachable!()
    };
    let index = (err >> 3) & 0b111111111111;

    SelectorError { external, table, index }
}


struct PageError {
    present: bool,
    write: bool,
    user: bool,
    reserved: bool,
    fetch: bool,
    address: *const u8,
    dir_idx: usize,
    table_idx: usize
}

impl fmt::Debug for PageError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            write!(f, "\x1bfdPageError \x1bff{{
      \x1bfapresent\x1bf7: \x1bf9{}\x1bf7,
        \x1bfawrite\x1bf7: \x1bf9{}\x1bf7,
         \x1bfauser\x1bf7: \x1bf9{}\x1bf7,
     \x1bfareserved\x1bf7: \x1bf9{}\x1bf7,
        \x1bfafetch\x1bf7: \x1bf9{}\x1bf7,
      \x1bfaaddress\x1bf7: \x1bf90x{:08x}\x1bf7,
      \x1bfadir_idx\x1bf7: \x1bf9{}\x1bf7,
    \x1bfatable_idx\x1bf7: \x1bf9{}\x1bf7   
\x1bff}}", self.present, self.write, self.user, self.reserved, self.fetch,
                self.address as usize, self.dir_idx, self.table_idx
            )
        } else {
            f.debug_struct("PageError")
                .field("present", &self.present)
                .field("write", &self.write)
                .field("user", &self.user)
                .field("reserved", &self.reserved)
                .field("fetch", &self.fetch)
                .field("address", &self.address)
                .field("dir_idx", &self.dir_idx)
                .field("table_idx", &self.table_idx)
                .finish()
        }
    }
}

fn page_error(err: Option<u32>) -> PageError {
    let err = err.unwrap_or(0);

    let present = err & 1 == 1;
    let write = err & 2 == 2;
    let user = err & 4 == 4;
    let reserved = err & 8 == 8;
    let fetch = err & 16 == 16;

    let address = read_cr2() as usize;
    let dir_idx = address >> 22;
    let table_idx = (address >> 12) & 1023;
    let address = address as *const u8;

    PageError {
        present, write, user, reserved, fetch, address, dir_idx, table_idx
    }
}

exceptions! {
    init;
    0 "DIVIDE BY ZERO" => divide_by_zero,
    1 "DEBUG" => debug,
    2 "NMI" => non_maskable_interrupt,
    3 "BREAK" => breakpoint,
    4 "OVERFLOW" => overflow,
    5 "OUT OF BOUNDS" => bound_range_exceeded,
    6 "INVALID OPCODE" => invalid_opcode,
    7 "FPU UNAVAILABLE" => device_not_available,
    8 "DOUBLE FAULT" => double_fault,
    9 "COPROCESSOR GPF" => coprocessor_segment_overrun,
    10 "INVALID TSS" => invalid_tss [selector_error],
    11 "SEGMENT NOT PRESENT" => segment_not_present [selector_error],
    12 "STACK SEGMENT FAULT" => stack_segment_fault [selector_error],
    13 "GENERAL PROTECTION FAULT" => general_protection_fault [selector_error],
    14 "PAGE FAULT" => page_fault [page_error],
    16 "FPU EXCEPTION" => x87_floating_point_exception,
    17 "ALIGNMENT CHECK" => alignment_check,
    18 "MACHINE CHECK" => machine_check,
    19 "SIMD FP EXCEPTION" => simd_floating_point_exception,
    20 "VIRTUALIZATION EXCEPTION" => virtualization_exception,
    30 "SECURITY EXCEPTION" => security_exception
}
