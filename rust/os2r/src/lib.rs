#![feature(maybe_uninit_ref, maybe_uninit_extra, internal_uninit_const)]
#![feature(maybe_uninit_uninit_array, slice_fill)]
#![feature(panic_info_message)]
#![feature(lang_items)]
#![feature(llvm_asm)]
#![allow(incomplete_features)]
#![feature(const_generics)]
#![feature(alloc_error_handler)]
#![feature(const_fn, const_if_match, const_panic, const_raw_ptr_to_usize_cast, const_btree_new)]
#![feature(abi_x86_interrupt)]
#![feature(leading_trailing_ones, slice_ptr_range, or_patterns)]
#![allow(dead_code)]
#![no_std]

#[allow(unused_imports)]
#[macro_use]
extern crate alloc;

#[macro_use]
mod log;
#[macro_use]
mod terminal;
#[macro_use]
mod panic;

#[macro_use]
mod cpu;
mod exceptions;
mod dev;
mod mem;
mod loader;
mod process;
mod vfs;

use alloc::{vec::Vec, string::String};


isr_handler! {
	mod syscall {
		fn handler(frame: &mut InterruptFrame, err_code: Option<u32>, regs: &mut Registers) {
			match regs.eax {
				0 => {
					unsafe {
						let child = super::PROCESS_STACK.pop().unwrap();
						log!("exiting process");
						if let Some(parent) = super::PROCESS_STACK.last_mut() {
							log!("loading parent");

							parent.memory.activate();
							let state = parent.state.take().unwrap();
							*regs = state.0;
							*frame = state.1;
							log!("updated state");

							core::mem::drop(child);
						} else {
							super::PROCESS_STACK.push(child);
							frame.eip = core::mem::transmute::<fn() -> !, u32>(super::process_end);
						}
					}
				},
				1 => {
					let slice = unsafe {
						core::slice::from_raw_parts(regs.ebx as *const u8, regs.ecx as usize)
					};
					let s = unsafe {
						core::str::from_utf8_unchecked(slice)
					};
					print!("{}", s);
				},
				2 => {
					let mut bytes = super::Vec::new();
					let edx = regs.edx.clone();
					if regs.edi == 1 {
						let node = unsafe {
							&*(regs.ebx as *const _)
						};
						//println!("{}", regs.ebx);
						let stat = super::vfs::stat(node);
						bytes.resize(stat.len, 0);
						assert!(super::vfs::read(node, &mut bytes, 0) == stat.len);
					} else if regs.edi == 0 {
						log!("raw elf launch");
						bytes.extend_from_slice(unsafe {
							core::slice::from_raw_parts(regs.ebx as *const u8, regs.ecx as usize)
						});
					} else {
						panic!("unknown launch type {}", regs.edi);
					};

					use alloc::string::ToString;
					let cmdline = unsafe {
						core::str::from_utf8_unchecked(core::slice::from_raw_parts(regs.esi as *const u8, edx as usize))
					}.to_string();

					unsafe {
						if let Some(process) = super::PROCESS_STACK.last_mut() {
							process.state = Some((regs.clone(), frame.clone()));
						}
					}

					let mut space = crate::process::AddressSpace::new();
					space.activate();

					log!("new address space activated");

					let elf = crate::loader::Elf { bytes: &bytes[..] };
					let header = elf.header();

					for phead in header.program_headers() {
						if phead.is_load() {
							let seg = space.alloc(phead.virtual_address(), phead.memory_size()).unwrap();
							seg[..phead.data().len()].copy_from_slice(phead.data());
							log!("segment at 0x{:x} [{} bytes] loaded", phead.virtual_address(), phead.memory_size());
						}
					}

					let stack = space.alloc(0x01000000, 0x10000).unwrap();
					let ptr = cmdline.as_ptr() as u32;
					let len = cmdline.len() as u32;
					log!("64KB stack at 0x1000000, cmdline ptr: {}", ptr);

					regs.eax = ptr;
					regs.ebx = regs.edx;
					regs.ecx = 0;
					regs.edx = 0;
					regs.esi = 0;
					regs.edi = 0;
					frame.eip = header.entry_point() as u32;
					frame.esp = (&stack[stack.len() - 1]) as *const _ as u32;

					unsafe {
						super::PROCESS_STACK.push(crate::process::Process {
							memory: space, state: None, cmdline
						});
					}

					log!("launching process!");
				},
				3 => {
					let slice = unsafe {
						core::slice::from_raw_parts(regs.ebx as *const u8, regs.ecx as usize)
					};
					let s = unsafe {
						core::str::from_utf8_unchecked(slice)
					};

					let res = super::vfs::find(s);
					
					regs.eax = match res {
						Err(super::vfs::FindError::NoSuchEntry(_)) => 1,
						Err(super::vfs::FindError::NotADirectory(_)) => 2,
						Ok(_) => 0
					};

					regs.ebx = match res {
						Err(super::vfs::FindError::NoSuchEntry(segnum)) => segnum as u32,
						Err(super::vfs::FindError::NotADirectory(segnum)) => segnum as u32,
						Ok(node) => node as *const _ as u32
					};
				},
				4 => {
					let stat = super::vfs::stat(unsafe {
						&*(regs.ebx as *const _)
					});
					
					regs.eax = if stat.is_dir {
						1
					} else {
						0
					};
					regs.ebx = stat.len as u32;
					regs.ecx = stat.name.as_ptr() as u32;
					regs.edx = stat.name.len() as u32;
				},
				5 => {
					regs.eax = super::vfs::dirent(unsafe {
						&*(regs.ebx as *const _)
					}, unsafe {
						core::slice::from_raw_parts_mut(regs.edi as *mut usize, regs.ecx as usize)
					}, regs.edx as usize) as u32;
				},
				6 => {
					regs.eax = super::vfs::read(unsafe {
						&*(regs.ebx as *const _)
					}, unsafe {
						core::slice::from_raw_parts_mut(regs.edi as *mut u8, regs.ecx as usize)
					}, regs.edx as usize) as u32;
				},
				7 => {
					regs.eax = super::dev::keyboard::getch() as u32;
				},
				k => panic!("unknown syscall {}", k)
			}
		}
	}
}

static mut PROCESS_STACK: Vec<process::Process> = Vec::new();

#[no_mangle]
pub extern "cdecl" fn kmain(multiboot_struct_ptr: usize) {
	let boot_info = unsafe {
		multiboot2::load(multiboot_struct_ptr)
	};

	let framebuffer = terminal::init(&boot_info, terminal::Options {
		text_mode: terminal::TextModeOptions::TextModeAndSerial {
			ansi: true, port: dev::serial::COM1
		},
		graphics_mode: terminal::GraphicsModeOptions::SerialOnly {
			ansi: true, port: dev::serial::COM1
		},
		backup: terminal::BackupOptions::AttemptTextModeAndSerial {
			ansi: true, port: dev::serial::COM1
		}
	});

	cpu::init();
	exceptions::init();

	trace!(mem::init(&boot_info, framebuffer));

	dev::init();

	vfs::init(&boot_info);

	cpu::isr::push_handler(0x80, syscall::handler);

	log!("launching init process");

	let node = vfs::find("/test_elf.bin").unwrap();
	unsafe {
		llvm_asm!("int 0x80" :: "{eax}"(2), "{ebx}"(node), "{esi}"("test".as_ptr()), "{edx}"("test".len()), "{edi}"(1) :: "intel");
	}
}

fn process_end() -> ! {
	log!("back to the kernel!");

	loop {}
}

#[lang = "eh_personality"]
fn eh_personality() {}