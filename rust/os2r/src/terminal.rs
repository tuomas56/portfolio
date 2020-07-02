use core::fmt::{self, Write};
use core::mem;

use crate::dev::serial;

#[repr(u8)]
#[derive(Copy, Clone, Debug)]
enum Color {
	Black = 0,
	Blue = 1,
	Green = 2,
	Cyan = 3,
	Red = 4,
	Magenta = 5,
	Brown = 6,
	LightGray = 7,
	DarkGray = 8,
	LightBlue = 9,
	LightGreen = 10,
	LightCyan = 11,
	LightRed = 12,
	Pink = 13,
	Yellow = 14,
	White = 15
}

#[repr(C, packed)]
struct Entry {
	text: u8,
	color: u8
}

impl Entry {
	fn new(ch: char, fg: Color, bg: Color) -> Entry {
		let mut entry = Entry { text: 0, color: 0 };
		entry.set_text(ch);
		entry.set_color(fg, bg);
		entry
	}

	fn set_text(&mut self, ch: char) {
		self.text = ch as u8;
	}

	fn set_color(&mut self, fg: Color, bg: Color) {
		self.color = (fg as u8) | ((bg as u8) << 4);
	}
}

pub struct TerminalState {
    fg: Color,
    bg: Color
}

pub enum Terminal {
	TextMode(TextModeTerminal),
	Serial(SerialTerminal),
	Dummy
}

impl Terminal {
	pub fn clear(&mut self) {
		if let Terminal::TextMode(t) = self {
			t.clear();
		}
	}

	pub fn save(&self) -> TerminalState {
		match self {
			Terminal::TextMode(t) => t.save(),
			Terminal::Serial(t) => t.save(),
			Terminal::Dummy => TerminalState { fg: Color::White, bg: Color::Black }
		}
	}

	pub fn restore(&mut self, state: TerminalState) {
		match self {
			Terminal::TextMode(t) => t.restore(state),
			Terminal::Serial(t) => t.restore(state),
			Terminal::Dummy => ()
		}
	}
}

impl Write for Terminal {
	fn write_str(&mut self, s: &str) -> fmt::Result {
		match self {
			Terminal::TextMode(t) => t.write_str(s),
			Terminal::Serial(t) => t.write_str(s),
			Terminal::Dummy => Ok(())
		}
	}
}

pub struct SerialTerminal {
	fg: Color,
	bg: Color,
	ansi: bool,
	port: serial::SerialPort
}

impl SerialTerminal {
	fn ansi_out(&mut self, c: char, ch: bool, cn: char, cl: Color) {
		match c {
			'f' => {
				if self.ansi {
					self.port.set_data(b'\x1b');
					self.port.set_data_unchecked(b'[');
					self.port.set_data_unchecked(if ch {
						b'9'
					} else {
						b'3'
					});
					self.port.set_data_unchecked(cn as u8);
					self.port.set_data_unchecked(b'm');
				}
				self.fg = cl;
			},
			'b' => {
				self.bg = cl;
			}
			_ => unreachable!()
		}
	}

	pub fn escape(&mut self, chars: &mut impl Iterator<Item=char>) {
		match chars.next() {
			Some(c@'f') | Some(c@'b') => {
				let (ch, cn, cl) = match chars.next() {
					Some('0') => (false, '0', Color::Black),
					Some('1') => (false, '4', Color::Blue),
					Some('2') => (false, '2', Color::Green),
					Some('3') => (false, '6', Color::Cyan),
					Some('4') => (false, '1', Color::Red),
					Some('5') => (false, '5', Color::Magenta),
					Some('6') => (false, '3', Color::Brown),
					Some('7') => (false, '7', Color::LightGray),
					Some('8') => (true, '0', Color::DarkGray),
					Some('9') => (true, '4', Color::LightBlue),
					Some('a') => (true, '2', Color::LightGreen),
					Some('b') => (true, '6', Color::LightCyan),
					Some('c') => (true, '1', Color::LightRed),
					Some('d') => (true, '5', Color::Pink),
					Some('e') => (true, '3', Color::Yellow),
					Some('f') => (true, '7', Color::White),
					_ => return
				};

				self.ansi_out(c, ch, cn, cl);
			},
			_ => ()
		}
	}

	pub fn save(&self) -> TerminalState {
		TerminalState {
			fg: self.fg, bg: self.bg
		}
	}

	pub fn restore(&mut self, state: TerminalState) {
		let (a, b) = match state.fg {
			Color::Black => (false, '0'),
			Color::Blue => (false, '4'),
			Color::Green => (false, '2'),
			Color::Cyan => (false, '6'),
			Color::Red => (false, '1'),
			Color::Magenta => (false, '5'),
			Color::Brown => (false, '3'),
			Color::LightGray => (false, '7'),
			Color::DarkGray => (true, '0'),
			Color::LightBlue => (true, '4'),
			Color::LightGreen => (true, '2'),
			Color::LightCyan => (true, '6'),
			Color::LightRed => (true, '1'),
			Color::Pink => (true, '5'),
			Color::Yellow => (true, '3'),
			Color::White => (true, '7'),
		};
		self.ansi_out('f', a, b, state.fg);
		let (a, b) = match state.bg {
			Color::Black => (false, '0'),
			Color::Blue => (false, '4'),
			Color::Green => (false, '2'),
			Color::Cyan => (false, '6'),
			Color::Red => (false, '1'),
			Color::Magenta => (false, '5'),
			Color::Brown => (false, '3'),
			Color::LightGray => (false, '7'),
			Color::DarkGray => (true, '0'),
			Color::LightBlue => (true, '4'),
			Color::LightGreen => (true, '2'),
			Color::LightCyan => (true, '6'),
			Color::LightRed => (true, '1'),
			Color::Pink => (true, '5'),
			Color::Yellow => (true, '3'),
			Color::White => (true, '7'),
		};
		self.ansi_out('b', a, b, state.bg);
	}
}

impl Write for SerialTerminal {
	fn write_str(&mut self, s: &str) -> fmt::Result {
		let mut chars = s.chars();
		let mut first = true;
		while let Some(ch) = chars.next() {
			match ch {
				'\x1b' => self.escape(&mut chars),
				_ => if first {
					self.port.set_data(ch as u8);
					first = false;
				} else {
					self.port.set_data_unchecked(ch as u8)
				}
			}
		}

		Ok(())
	}
}

pub struct TextModeTerminal {
	screen: &'static mut [Entry],
	width: usize,
	height: usize,
	pitch: usize,
	x: usize,
	y: usize,
	fg: Color,
	bg: Color,
	serial: Option<SerialTerminal>
}

impl TextModeTerminal {
	pub fn clear(&mut self) {
		for entry in self.screen.iter_mut() {
			entry.set_color(self.fg, self.bg);
			entry.set_text(' ');
		}
	}

	pub fn advance(&mut self) {
		self.x += 1;
		if self.x >= self.width {
			self.newline()
		}
	}

	pub fn newline(&mut self) {
		self.x = 0;
		self.y += 1;

		if self.y >= self.height {
			self.clear();
			self.y = 0;
		}
	}

	pub fn backspace(&mut self) {
		if self.x == 0 {
			if self.y == 0 {
				return;
			}

			self.x = self.width - 1;
			self.y -= 1;
		} else {
			self.x -= 1;
		}

		let entry = &mut self.screen[self.x + self.y * self.pitch];
		entry.set_text(' ');
		entry.set_color(self.fg, self.bg);
	}

	pub fn escape(&mut self, chars: &mut impl Iterator<Item=char>) {
		match chars.next() {
			Some(c@'f') | Some(c@'b') => {
				let color = match chars.next() {
					Some('0') => Color::Black,
					Some('1') => Color::Blue,
					Some('2') => Color::Green,
					Some('3') => Color::Cyan,
					Some('4') => Color::Red,
					Some('5') => Color::Magenta,
					Some('6') => Color::Brown,
					Some('7') => Color::LightGray,
					Some('8') => Color::DarkGray,
					Some('9') => Color::LightBlue,
					Some('a') => Color::LightGreen,
					Some('b') => Color::LightCyan,
					Some('c') => Color::LightRed,
					Some('d') => Color::Pink,
					Some('e') => Color::Yellow,
					Some('f') => Color::White,
					_ => return
				};

				match c {
					'f' => self.fg = color,
					'b' => self.bg = color,
					_ => unreachable!()
				}
			},
			_ => ()
		}
	}
	
	pub fn save(&self) -> TerminalState {
       	TerminalState { fg: self.fg, bg: self.bg }
    }

    pub fn restore(&mut self, state: TerminalState) {
		self.fg = state.fg;
		self.bg = state.bg;

		if let Some(serial) = self.serial.as_mut() {
			serial.restore(state);
		}
    }
}

impl Write for TextModeTerminal {
	fn write_str(&mut self, s: &str) -> fmt::Result {
		if let Some(serial) = self.serial.as_mut() {
			serial.write_str(s)?;
		}

		let mut chars = s.chars();
		while let Some(ch) = chars.next() {
			match ch {
				'\n' => self.newline(),
				'\x08' => self.backspace(),
				'\x1b' => self.escape(&mut chars),
				ch => {
					self.screen[self.x + self.y * self.pitch] = Entry::new(ch, self.fg, self.bg);
					self.advance();
				}
			}
		}

		Ok(())
	}
}

pub static mut GLOBAL_TERMINAL: mem::MaybeUninit<Terminal> = mem::MaybeUninit::uninit();

#[macro_export]
macro_rules! println {
	($($t:tt)*) => {{
        use core::fmt::Write;
		let _ = writeln!(crate::terminal::get(), $($t)*);
    }};
}

#[macro_export]
macro_rules! print {
	($($t:tt)*) => {{
        use core::fmt::Write;
		let _ = write!(crate::terminal::get(), $($t)*);
    }};
}

pub struct Options {
	pub text_mode: TextModeOptions,
	pub graphics_mode: GraphicsModeOptions,
	pub backup: BackupOptions
}

pub enum TextModeOptions {
	TextModeOnly,
	SerialOnly {
		ansi: bool,
		port: serial::SerialPort
	},
	TextModeAndSerial {
		ansi: bool,
		port: serial::SerialPort 
	},
	Nothing
}

pub enum GraphicsModeOptions {
	SerialOnly {
		ansi: bool,
		port: serial::SerialPort
	},
	Nothing
}

pub enum BackupOptions {
	AttemptTextModeAndSerial {
		ansi: bool,
		port: serial::SerialPort 
	},
	AttemptTextModeOnly,
	SerialOnly {
		ansi: bool,
		port: serial::SerialPort 
	},
	Nothing
}

fn init_serial(com1: serial::SerialPort) {
	let mut interrupt_enable = com1.interrupt_enable();
	interrupt_enable.set_modem_status(false);
	interrupt_enable.set_reciever_line_status(false);
	interrupt_enable.set_transmit_holding_empty(false);
	interrupt_enable.set_recieved_data_available(false);
	com1.set_interrupt_enable(interrupt_enable);

	com1.set_baud_divisor(3);

	let mut line_control = com1.line_control();
	line_control.set_parity_enable(false);
	line_control.set_two_stop_bits(false);
	line_control.set_word_length(serial::WordLength::Eight);
	com1.set_line_control(line_control);

	let mut fifo_control = serial::FIFOControl::new();
	fifo_control.set_fifo_enable(true);
	fifo_control.set_clear_tx_fifo(true);
	fifo_control.set_clear_rx_fifo(true);
	fifo_control.set_interrupt_trigger_level(serial::InterruptTriggerLevel::FourteenBytes);
	com1.set_fifo_control(fifo_control);

	let mut modem_control = com1.modem_control();
	modem_control.set_force_dtr(true);
	modem_control.set_force_rts(true);
	com1.set_modem_control(modem_control);
}

pub fn init(boot_info: &multiboot2::BootInformation, options: Options) -> Option<(usize, usize)> {
	unsafe {
		match boot_info.framebuffer_tag() {
			Some(tag) => match tag.buffer_type {
				multiboot2::FramebufferType::Text => match options.text_mode {
					TextModeOptions::TextModeOnly | TextModeOptions::TextModeAndSerial { .. } => {
						GLOBAL_TERMINAL.write(Terminal::TextMode(TextModeTerminal {
							screen: core::slice::from_raw_parts_mut(
								tag.address as usize as *mut Entry,
								(tag.pitch * tag.height) as usize
							),
							width: tag.width as usize,
							height: tag.height as usize,
							pitch: tag.pitch as usize >> 1,
							x: 0, y: 0,
							fg: Color::White,
							bg: Color::Black,
							serial: if let TextModeOptions::TextModeAndSerial { ansi, port } = options.text_mode {
								init_serial(port);
								Some(SerialTerminal {
									bg: Color::Black, fg: Color::White,
									ansi, port
								})
							} else {
								None
							}
						})).clear();

						if let TextModeOptions::TextModeAndSerial { .. } = options.text_mode {
							log!("using {}x{} text mode and serial", tag.width, tag.height);
						} else {
							log!("using {}x{} text mode", tag.width, tag.height);
						}

						Some((tag.address as usize, tag.address as usize + (tag.pitch * tag.height) as usize))
					},
					TextModeOptions::SerialOnly { ansi, port } => {
						init_serial(port);
						GLOBAL_TERMINAL.write(Terminal::Serial(SerialTerminal {
							bg: Color::Black, fg: Color::White,
							ansi, port
						}));
						log!("text mode set, using serial");
						None
					},
					TextModeOptions::Nothing => {
						GLOBAL_TERMINAL.write(Terminal::Dummy);
						None
					}
				},
				_ => match options.graphics_mode {
					GraphicsModeOptions::SerialOnly { ansi, port } => {
						init_serial(port);
						GLOBAL_TERMINAL.write(Terminal::Serial(SerialTerminal {
							bg: Color::Black, fg: Color::White,
							ansi, port
						}));
						log!("graphics mode set, using serial");
						None
					},
					GraphicsModeOptions::Nothing => {
						GLOBAL_TERMINAL.write(Terminal::Dummy);
						None
					}
				}
			},
			None => match options.backup {
				BackupOptions::AttemptTextModeAndSerial { .. } | BackupOptions::AttemptTextModeOnly => {
					GLOBAL_TERMINAL.write(Terminal::TextMode(TextModeTerminal {
						screen: core::slice::from_raw_parts_mut(
							0xb8000 as *mut Entry,
							2000
						),
						width: 80,
						height: 25,
						pitch: 80,
						x: 0, y: 0,
						fg: Color::White,
						bg: Color::Black,
						serial: if let BackupOptions::AttemptTextModeAndSerial { ansi, port} = options.backup {
							init_serial(port);
							Some(SerialTerminal {
								bg: Color::Black, fg: Color::White,
								ansi, port
							})
						} else {
							None
						}
					})).clear();

					if let BackupOptions::AttemptTextModeAndSerial { .. } = options.backup {
						log!("no framebuffer tag available, trying 80x25 text mode and serial");
					} else {
						log!("no framebuffer tag available, trying 80x25 text mode");
					}

					Some((0xb8000, 0xb8000 + 80*25))
				},
				BackupOptions::SerialOnly { ansi, port } => {
					init_serial(port);
					GLOBAL_TERMINAL.write(Terminal::Serial(SerialTerminal {
						fg: Color::White, bg: Color::Black,
						ansi, port
					}));
					log!("no framebuffer tag available, using serial");
					None
				},
				BackupOptions::Nothing => {
					GLOBAL_TERMINAL.write(Terminal::Dummy);
					None
				}
			}
		}
	}
}

pub fn get() -> &'static mut Terminal {
    unsafe {
        GLOBAL_TERMINAL.get_mut()
    }
}