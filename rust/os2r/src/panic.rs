use crate::terminal;
use core::mem;

#[derive(Debug)]
pub struct Frame {
    pub context: &'static str,
    pub file: &'static str,
    pub source: &'static str,
    pub line: usize
}

pub struct Backtrace<'s> {
    frames: &'s mut [mem::MaybeUninit<Frame>],
    start: usize,
    end: usize
}

impl<'s> Backtrace<'s> {
    pub fn push(&mut self, frame: Frame) {
        self.frames[self.end].write(frame);
        self.end += 1;
        self.end %= self.frames.len();

        if self.end == self.start {
            self.start += 1;
            self.start %= self.frames.len();
        }
    }

    pub fn pop(&mut self) {
        if self.end != self.start {
            self.end -= 1;
            self.end %= self.frames.len();
        }
    }
}

static mut GLOBAL_BACKTRACE_FRAMES: [mem::MaybeUninit<Frame>; 16] = [mem::MaybeUninit::UNINIT; 16];
pub static mut GLOBAL_BACKTRACE: Backtrace = Backtrace {
    frames: unsafe { &mut GLOBAL_BACKTRACE_FRAMES },
    start: 0,
    end: 0
};

#[macro_export]
macro_rules! trace {
	($expr:expr) => {
		if cfg!(feature = "trace") {
            unsafe {
                crate::panic::GLOBAL_BACKTRACE.push(crate::panic::Frame {
                    context: module_path!(),
                    file: file!(),
                    source: stringify!($expr),
                    line: line!() as usize
                })
            }

            let res = $expr;

            unsafe {
                crate::panic::GLOBAL_BACKTRACE.pop();
            }

            res
		} else {
            $expr
        }
    };
    ($context:literal, $expr:expr) => {
		if cfg!(feature = "trace") {
            unsafe {
                crate::panic::GLOBAL_BACKTRACE.push(crate::panic::Frame {
                    context: $context,
                    file: file!(),
                    source: stringify!($expr),
                    line: line!() as usize
                })
            }

            let res = $expr;

            unsafe {
                crate::panic::GLOBAL_BACKTRACE.pop();
            }

            res
		} else {
            $expr
        }
    };
    ($stmt:stmt) => {
        if cfg!(feature = "trace"){
            unsafe {
                crate::panic::GLOBAL_BACKTRACE.push(crate::panic::Frame {
                    context: module_path!(),
                    file: file!(),
                    source: stringify!($stmt),
                    line: line!() as usize
                })
            }
        }

        $stmt;

        if cfg!(feature = "trace") {
            unsafe {
                crate::panic::GLOBAL_BACKTRACE.pop();
            }
        }
    };
    ($context:literal, $stmt:stmt) => {
        if cfg!(feature = "trace") {
            unsafe {
                crate::panic::GLOBAL_BACKTRACE.push(crate::panic::Frame {
                    context: $context,
                    file: file!(),
                    source: stringify!($stmt),
                    line: line!() as usize
                })
            }
        }

        $stmt;

        if cfg!(feature = "trace") {
            unsafe {
                crate::panic::GLOBAL_BACKTRACE.pop();
            }
        }
    }
}

#[panic_handler]
#[no_mangle] 
#[allow(unused_unsafe)]
pub extern fn panic_impl(info: &core::panic::PanicInfo) -> ! { 
	print!("\x1bfc\x1bb0KERNEL PANIC\x1bff: ");

	if let Some(args) = info.message() {
		unsafe {
            use core::fmt::Write;
			let _ = terminal::GLOBAL_TERMINAL.get_mut().write_fmt(*args);
		}
	}

	if let Some(location) = info.location() {
		println!("\n\x1bfefile\x1bff: {:?}, \x1bfeline\x1bff: {}\n", location.file(), location.line());
	}

    if cfg!(feature = "trace") {
        println!("\x1bf7------------ BACKTRACE -----------");
        unsafe {
            let mut i = GLOBAL_BACKTRACE.start;
            let mut j = 0;
            while i != GLOBAL_BACKTRACE.end {
                let frame = GLOBAL_BACKTRACE.frames[i].get_ref();
                println!("\n\x1bf7{}: \x1bfb{}\n\x1bf7  at {}:{}\n    {} | \x1bff{}",
                    j, frame.context, frame.file, frame.line, frame.line, frame.source);
                i += 1;
                j += 1;
                i %= GLOBAL_BACKTRACE.frames.len();
            }
        }
    }

	loop {}
}