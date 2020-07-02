use super::port::Port;
use super::pci;
use modular_bitfield::prelude::*;
use alloc::{rc::Rc, vec::Vec};

macro_rules! impl_from {
    ($t:ty) => {
        impl From<u8> for $t {
            fn from(val: u8) -> Self {
                unsafe {
                    core::mem::transmute(val)
                }
            }
        }
    };
}

macro_rules! impl_to {
    ($t:ty) => {
        impl Into<u8> for $t {
            fn into(self) -> u8 {
                unsafe {
                    core::mem::transmute(self)
                }
            }
        }
    }
}

#[bitfield]
#[derive(Copy, Clone)]
struct Errors {
    address_mark_not_found: bool,
    track_zero_not_found: bool,
    aborted_command: bool,
    media_change_request: bool,
    id_not_found: bool,
    media_changed: bool,
    uncorrectable_data_error: bool,
    bad_block_detected: bool,
}

impl_from!(Errors);

#[bitfield]
#[derive(Copy, Clone)]
struct DriveSel {
    lba_top: B4,
    slave_selected: bool,
    _res1: bool,
    lba_enabled: bool,
    _res2: bool
}

impl_from!(DriveSel);
impl_to!(DriveSel);

#[bitfield]
#[derive(Copy, Clone, PartialEq)]
struct Status {
    error: bool,
    index: bool,
    corrected_data: bool,
    data_request: bool,
    service_request: bool,
    drive_fault: bool,
    ready: bool,
    busy: bool
}

impl_from!(Status);

#[bitfield]
#[derive(Copy, Clone)]
struct Control {
    _res1: bool,
    interrupt_disable: bool,
    software_reset: bool,
    _res2: B4,
    readback_high_order: bool
}

impl_to!(Control);

#[bitfield]
#[derive(Copy, Clone)]
struct DriveAddr {
    master_selected: bool,
    slave_selected: bool,
    selected_head: B4,
    write_gate: bool,
    _res: bool
}

impl_from!(DriveAddr);

#[derive(Copy, Clone)]
struct Features(u8);

impl_to!(Features);

#[repr(u8)]
#[derive(Copy, Clone)]
enum Command {
    Identify = 0xec
}

impl_to!(Command);

struct Channel {
    device: pci::Device,
    primary: bool,
    data: Port<u16>,
    error: Port<Errors, u8>,
    features: Port<Features, u8>,
    sec_count: Port<u8>,
    lba_low: Port<u8>,
    lba_mid: Port<u8>,
    lba_high: Port<u8>,
    drive_sel: Port<DriveSel, u8>,
    status: Port<Status, u8>,
    command: Port<Command, u8>,
    alternate_status: Port<Status, u8>,
    control: Port<Control, u8>,
    drive_addr: Port<DriveAddr, u8>
}

struct Device {
    channel: Rc<Channel>,
    master: bool,
    capabilites: [u8; 512]
}

impl Device {
    fn new(channel: Rc<Channel>, master: bool) -> Option<Device> {
        let mut drive_sel = channel.drive_sel.read();
        drive_sel.set_slave_selected(!master);
        drive_sel.set_lba_top(0);
        drive_sel.set_lba_enabled(false);
        channel.drive_sel.write(drive_sel);
        channel.lba_low.write(0);
        channel.lba_mid.write(0);
        channel.lba_high.write(0);
        channel.sec_count.write(0);
        channel.command.write(Command::Identify);

        if channel.status.read() == Status::new() {
            return None
        }
        
        while channel.status.read().get_busy() {}

        if channel.lba_mid.read() != 0 || channel.lba_high.read() != 0 {
            return None
        }

        while !(channel.status.read().get_error() || channel.status.read().get_data_request()) {}

        if channel.status.read().get_error() {
            return None
        }

        let mut buf = [0; 512];
        for i in 0..256 {
            let word = channel.data.read();
            buf[2 * i + 1] = word as u8;
            buf[2 * i] = (word >> 8) as u8;
        }

        Some(Device {
            channel, master, capabilites: buf
        })
    }

    fn model_number(&self) -> &str {
        core::str::from_utf8(&self.capabilites[54..94]).unwrap().trim()
    }
}

static mut DEVICES: Vec<Device> = Vec::new();

pub fn init() {
    for device in pci::devices() {
        if let pci::DeviceClass::MassStorageController(pci::MassStorageController::IDE(_)) = device.class() {
            log!("checking controller at {}/{}/{}", device.bus, device.device, device.function);

            let bar0 = match device.bar0.read() {
                pci::BAR::IO(0 | 1) | pci::BAR::Mem(0 | 1) => 0x1f0,
                pci::BAR::IO(addr) => addr,
                pci::BAR::Mem(_) => continue
            } as u16;

            let bar1 = match device.bar1.read() {
                pci::BAR::IO(0 | 1) | pci::BAR::Mem(0 | 1) => 0x3f6,
                pci::BAR::IO(addr) => addr,
                pci::BAR::Mem(_) => continue
            } as u16;

            let bar2 = match device.bar2.read() {
                pci::BAR::IO(0 | 1) | pci::BAR::Mem(0 | 1) => 0x170,
                pci::BAR::IO(addr) => addr,
                pci::BAR::Mem(_) => continue
            } as u16;

            let bar3 = match device.bar3.read() {
                pci::BAR::IO(0 | 1) | pci::BAR::Mem(0 | 1) => 0x376,
                pci::BAR::IO(addr) => addr,
                pci::BAR::Mem(_) => continue
            } as u16;

            let primary = Rc::new(Channel {
                device: device.clone(),
                primary: true,
                data: Port::new(bar0 + 0),
                error: Port::new(bar0 + 1),
                features: Port::new(bar0 + 1),
                sec_count: Port::new(bar0 + 2),
                lba_low: Port::new(bar0 + 3),
                lba_mid: Port::new(bar0 + 4),
                lba_high: Port::new(bar0 + 5),
                drive_sel: Port::new(bar0 + 6),
                command: Port::new(bar0 + 7),
                status: Port::new(bar0 + 7),
                alternate_status: Port::new(bar1 + 0),
                control: Port::new(bar1 + 0),
                drive_addr: Port::new(bar1 + 1)
            });

            let secondary = Rc::new(Channel {
                device: device.clone(),
                primary: false,
                data: Port::new(bar2 + 0),
                error: Port::new(bar2 + 1),
                features: Port::new(bar2 + 1),
                sec_count: Port::new(bar2 + 2),
                lba_low: Port::new(bar2 + 3),
                lba_mid: Port::new(bar2 + 4),
                lba_high: Port::new(bar2 + 5),
                drive_sel: Port::new(bar2 + 6),
                command: Port::new(bar2 + 7),
                status: Port::new(bar2 + 7),
                alternate_status: Port::new(bar3 + 0),
                control: Port::new(bar3 + 0),
                drive_addr: Port::new(bar3 + 1)
            });

            unsafe {
                if let Some(device) = Device::new(primary.clone(), true) {
                    log!("found {:?} at primary master", device.model_number());
                    DEVICES.push(device);
                }

                if let Some(device) = Device::new(primary, false) {
                    log!("found {:?} at primary slave", device.model_number());
                    DEVICES.push(device);
                }

                if let Some(device) = Device::new(secondary.clone(), true) {
                    log!("found {:?} at secondary master", device.model_number());
                    DEVICES.push(device);
                }

                if let Some(device) = Device::new(secondary, false) {
                    log!("found {:?} at secondary slave", device.model_number());
                    DEVICES.push(device);
                }
            }
        }
    }
}