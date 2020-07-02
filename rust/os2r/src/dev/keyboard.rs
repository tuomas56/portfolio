use super::port::Port;

#[derive(Copy, Debug, Clone)]
pub enum Key {
    Char(u8),
    Special(u8)
}

#[derive(Copy, Debug, Clone)]
pub enum Event {
    KeyUp(Key),
    KeyDown(Key)
}

static SCAN_CODES: &'static [u8] = "\x00\x1B1234567890-=\x08\tqwertyuiop[]\n\x00asdfghjkl;'`\x00\\zxcvbnm,./\x00*\x00 \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00789-456+1230.\x00\x00\x00\x00\x00".as_bytes();
static SCAN_CODES_SHIFTED: &'static [u8] = "\x00\x1B!@#$%^&*()_+\x08\tQWERTYUIOP{}\n\x00ASDFGHJKL:\"~\x00|ZXCVBNM<>?\x00*\x00 \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00789-456+1230.\x00\x00\x00\x00\x00".as_bytes();
static mut SHIFT_DOWN: bool = false;

fn process_scan_code(mut scan_code: usize) -> Event {
    let is_keydown = scan_code & 0x80 == 0;
    if is_keydown {
        if scan_code == 0x2A || scan_code == 0x36 {
            unsafe {
                SHIFT_DOWN = true;
            }
        }
    } else {
        scan_code &= 0b01111111;
        if scan_code == 0x2A || scan_code == 0x36 {
            unsafe {
                SHIFT_DOWN = false;
            }
        }
    }

    let mut key_char: u8 = 0;
    let mut key_is_char = false;
    unsafe {
        if SHIFT_DOWN && scan_code < SCAN_CODES_SHIFTED.len() && SCAN_CODES_SHIFTED[scan_code] != 0 && scan_code > 1 && scan_code < 64 {
            key_char = SCAN_CODES_SHIFTED[scan_code];
            key_is_char = true;
        } else {
            if scan_code < SCAN_CODES.len() && SCAN_CODES[scan_code] != 0 && scan_code > 1 && scan_code < 64 {
                key_char = SCAN_CODES[scan_code];
                key_is_char = true;
            }
        }
    }

    let key = if key_is_char {
        Key::Char(key_char)
    } else {
        Key::Special(scan_code as u8)
    };
    if is_keydown {
        Event::KeyDown(key)
    } else {
        Event::KeyUp(key)
    }
}

const DATA: Port<u8> = Port::new(0x60);
const STATUS: Port<u8> = Port::new(0x64);

isr_handler! {
    mod irq1 {
        fn handler(_frame: &mut InterruptFrame, _error_code: Option<u32>, _regs: &mut Registers) {
            let scan_code = super::DATA.read() as usize;
            let event = super::process_scan_code(scan_code);

        }
    }
}

pub fn getch() -> char {
    loop {
        while STATUS.read() & 1 == 0 {}
        let event = process_scan_code(DATA.read() as usize);

        if let Event::KeyDown(Key::Char(c)) = event {
            return c as char;
        }
    }
}

pub fn init() {
    crate::cpu::isr::push_handler(33, irq1::handler);
    crate::cpu::isr::enable_irq(1);
}