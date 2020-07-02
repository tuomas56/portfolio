#![no_std]
#![feature(llvm_asm)]

#[panic_handler]
fn panic(_: &core::panic::PanicInfo) -> ! {
    loop {}
}

fn write(s: &str) {
    unsafe {
        llvm_asm!("int 0x80" :: "{eax}"(1), "{ebx}"(s.as_ptr()), "{ecx}"(s.len()) :: "intel");
    }
}

fn launch(node: usize, cmdline: &str) {
    unsafe {
        llvm_asm!("int 0x80" :: "{eax}"(2), "{ebx}"(node), "{esi}"(cmdline.as_ptr()), "{edx}"(cmdline.len()), "{edi}"(1) :: "intel");
    }
}

fn exit() {
    unsafe {
        llvm_asm!("int 0x80" :: "{eax}"(0) :: "intel");
    }
}

fn getch() -> char {
    let c: u32;
    unsafe {
        llvm_asm!("int 0x80" : "={eax}"(c) : "{eax}"(7) :: "intel");
    }
    c as u8 as char
}

fn find(path: &str) -> Option<usize> {
    let err: usize;
    let node: usize;

    unsafe {
        llvm_asm!("int 0x80" : "={eax}"(err), "={ebx}"(node) : "{ebx}"(path.as_ptr()), "{ecx}"(path.len()), "{eax}"(3) :: "intel");
    }

    if err == 0 {
        Some(node)
    } else {
        None
    }
}

fn stat(node: usize) -> (bool, usize) {
    let is_dir: bool;
    let len: usize;

    unsafe {
        llvm_asm!("int 0x80" : "={eax}"(is_dir), "={ebx}"(len) : "{ebx}"(node), "{eax}"(4) :: "intel");
    }

    (is_dir, len)
}

struct Stdout;

use core::fmt::Write;

impl core::fmt::Write for Stdout {
    fn write_str(&mut self, s: &str) -> core::fmt::Result {
        write(s);
        Ok(())
    }
}

macro_rules! print {
    ($($t:tt)*) => {
        write!(Stdout, $($t)*).unwrap()
    }
}

macro_rules! println {
    ($($t:tt)*) => {
        writeln!(Stdout, $($t)*).unwrap()
    }
}

fn getstr(buf: &mut [u8]) -> &str {
    let mut i = 0;
    loop {
        let c = getch();
        match c {
            '\x08' => {
                if i > 0 {
                    print!("\x08");
                    i -= 1;
                }
            },
            '\n' => {
                print!("\n");
                break
            },
            c => {
                print!("{}", c);
                buf[i] = c as u8;
                i += 1;
                if i == 80 {
                    break;
                }
            }
        }
    }

    return unsafe { core::str::from_utf8_unchecked(&buf[..i]) }
}

#[no_mangle]
pub extern "cdecl" fn start() {
    let msg = unsafe {
        let ptr: u32;
        let len: u32;
        llvm_asm!("" : "={eax}"(ptr), "={ebx}"(len));
        core::str::from_utf8_unchecked(core::slice::from_raw_parts(ptr as *const u8, len as usize))
    };

    println!("OS2 Shell. Starting with message: {:?}", msg);
    let mut buf = [0u8; 80];
    loop {
        print!("> ");
        let s = getstr(&mut buf);
        if s == "exit" {
            break;
        } else {
            let mut i = s.trim().splitn(2, " ");
            let cmd = i.next().unwrap();
            let line = i.next().unwrap_or("");

            if let Some(node) = find(cmd) {
                let (is_dir, _) = stat(node);
                if is_dir {
                    println!("node {} is a directory ({})", node, line.len());
                } else {
                    launch(node, line);
                }
            } else {
                println!("no such command");
            }
        }
    }
    exit()
}
