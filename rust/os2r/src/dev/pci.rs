use alloc::vec::Vec;
use super::port::{Port, PortRegister, PortRegisters};
use modular_bitfield::prelude::*;

macro_rules! device_class {
    { $(#[$attr:meta] enum $tyname:ident: $typ:ty { $($val:literal => $name:ident$(($name2:ident))?),* })+ } => {
        $(
            #[$attr]
            pub enum $tyname {
                $($name$(($name2))?),*
            }

            impl $tyname {
                pub fn new(val: &[$typ]) -> $tyname {
                    match val[0] {
                        $($val => $tyname::$name$(($name2::new(&val[1..])))?),*,
                        _ => unreachable!()
                    }
                }

                /*#[allow(non_snake_case)]
                pub fn code(&self, c: &mut Vec<$typ>) {
                    match self {
                        $($tyname::$name$(($name2))? => {
                            c.push($val);
                            $($name2.code(c);)?
                        }),*
                    }
                }*/
            }
        )+
    }
}

device_class! {
    #[derive(Debug, Copy, Clone, PartialEq)]
    enum DeviceClass: u8 {
        0x00 => Unclassified(Unclassified),
        0x01 => MassStorageController(MassStorageController),
        0x02 => NetworkController(NetworkController),
        0x03 => DisplayController(DisplayController),
        0x04 => MultimediaController(MultimediaController),
        0x05 => MemoryController(MemoryController),
        0x06 => Bridge(Bridge),
        0x07 => SimpleCommunicationController(SimpleCommunicationController),
        0x08 => BaseSystemdev(BaseSystemdev),
        0x09 => InputDeviceController(InputDeviceController),
        0x0a => DockingStation(DockingStation),
        0x0b => Processor(Processor),
        0x0c => SerialBusController(SerialBusController),
        0x0d => WirelessController(WirelessController),
        0x0e => IntelligentController(IntelligentController),
        0x0f => SatelliteCommunicationController(SatelliteCommunicationController),
        0x10 => EncryptionController(EncryptionController),
        0x11 => SignalProcessingController(SignalProcessingController),
        0x12 => ProcessingAccelerator,
        0x13 => NonEssentialInstrumentation,
        0x40 => CoProcessor,
        0xff => Unassigned
    }

    #[derive(Debug, Copy, Clone, PartialEq)]
    enum Unclassified: u8 {
        0x00 => Unclassified,
        0x01 => VGACompatible
    }

    #[derive(Debug, Copy, Clone, PartialEq)]
    enum MassStorageController: u8 {
        0x00 => SCSIBus,
        0x01 => IDE(IDEController),
        0x02 => FloppyDisk,
        0x03 => IPIBus,
        0x04 => RAID,
        0x05 => ATA(ATAController),
        0x06 => SerialATA(SerialATA),
        0x07 => SerialAttachedSCSI(SerialAttachedSCSI),
        0x08 => NonVolatileMemory(NonVolatileMemoryController),
        0x80 => Other
    }

    #[derive(Debug, Copy, Clone, PartialEq)]
    enum NetworkController: u8 {
        0x00 => Ethernet,
        0x01 => TokenRing,
        0x02 => FDDI,
        0x03 => ATM,
        0x04 => ISDN,
        0x05 => WorldFip,
        0x06 => PICMG,
        0x07 => Infiniband,
        0x08 => Fabric,
        0x80 => Other
    }

    #[derive(Debug, Copy, Clone, PartialEq)]
    enum DisplayController: u8 {
        0x00 => VGACompatible(VGACompatibleController),
        0x01 => XGA,
        0x02 => Other3D,
        0x80 => Other
    }

    #[derive(Debug, Copy, Clone, PartialEq)]
    enum MultimediaController: u8 {
        0x00 => Video,
        0x01 => Audio,
        0x02 => Telephony,
        0x03 => AudioDevice,
        0x80 => Other
    }

    #[derive(Debug, Copy, Clone, PartialEq)]
    enum MemoryController: u8 {
        0x00 => RAM,
        0x01 => Flash,
        0x80 => Other
    }

    #[derive(Debug, Copy, Clone, PartialEq)]
    enum Bridge: u8 {
        0x00 => Host,
        0x01 => ISA,
        0x02 => EISA,
        0x03 => MCA,
        0x04 => PCI(PCIBridge),
        0x05 => PCMCIA,
        0x06 => NuBus,
        0x07 => CardBus,
        0x08 => RACEway(RACEwayBridge),
        0x80 => Other
    }

    #[derive(Debug, Copy, Clone, PartialEq)]
    enum SimpleCommunicationController: u8 {
        0x00 => Serial(SerialController),
        0x01 => Parallel(ParallelController),
        0x02 => MultiportSerial,
        0x03 => Modem(ModemController),
        0x04 => GPIB,
        0x05 => SmartCard,
        0x80 => Other
    }

    #[derive(Debug, Copy, Clone, PartialEq)]
    enum BaseSystemdev: u8 {
        0x00 => PIC(PIC),
        0x01 => DMAController(DMAController),
        0x02 => Timer(Timer),
        0x03 => RTCController(RTCController),
        0x04 => PCIHotplugController,
        0x05 => SDHostController,
        0x06 => IOMMU,
        0x80 => Other
    }

    #[derive(Debug, Copy, Clone, PartialEq)]
    enum InputDeviceController: u8 {
        0x00 => Keyboard,
        0x01 => DigitizerPen,
        0x02 => Mouse,
        0x03 => Scanner,
        0x04 => Gameport(GameportController),
        0x80 => Other
    }

    #[derive(Debug, Copy, Clone, PartialEq)]
    enum DockingStation: u8 {
        0x00 => Generic,
        0x80 => Other
    }

    #[derive(Debug, Copy, Clone, PartialEq)]
    enum Processor: u8 {
        0x00 => I386,
        0x01 => I486,
        0x02 => Pentium,
        0x10 => Alpha,
        0x20 => PowerPC,
        0x30 => MIPS,
        0x40 => CoProcessor
    }

    #[derive(Debug, Copy, Clone, PartialEq)]
    enum SerialBusController: u8 {
        0x00 => FireWire(FireWireController),
        0x01 => Access,
        0x02 => SSA,
        0x03 => USB(USBController),
        0x04 => FibreChannel,
        0x05 => SMBus,
        0x06 => InfiniBand,
        0x07 => IPMI(IPMIInterface),
        0x08 => SERCOS,
        0x09 => CAN
    }

    #[derive(Debug, Copy, Clone, PartialEq)]
    enum WirelessController: u8 {
        0x00 => IRDACompatible,
        0x01 => ConsumerIR,
        0x10 => RF,
        0x11 => Bluetooth,
        0x12 => Broadband,
        0x20 => EthernetA,
        0x21 => EthernetB,
        0x80 => Other
    }

    #[derive(Debug, Copy, Clone, PartialEq)]
    enum IntelligentController: u8 {
        0x00 => I20
    }

    #[derive(Debug, Copy, Clone, PartialEq)]
    enum SatelliteCommunicationController: u8 {
        0x01 => TV,
        0x02 => Audio,
        0x03 => Voice,
        0x04 => Data
    }

    #[derive(Debug, Copy, Clone, PartialEq)]
    enum EncryptionController: u8 {
        0x00 => Network,
        0x10 => Multimedia,
        0x80 => Other
    }

    #[derive(Debug, Copy, Clone, PartialEq)]
    enum SignalProcessingController: u8 {
        0x00 => DPIOModules,
        0x01 => PerformanceCounters,
        0x10 => CommunicationSynchronizer,
        0x20 => SignalProcessingManagement,
        0x80 => Other
    }

    #[derive(Debug, Copy, Clone, PartialEq)]
    enum IDEController: u8 {
        0x00 => ISA,
        0x05 => PCI,
        0x0a => ISASupportsPCI,
        0x0f => PCISupportsISA,
        0x80 => ISASupportsBM,
        0x85 => PCISupportsBM,
        0x8a => ISASupportsBMPCI,
        0x8f => PCISupportsBMISA
    }

    #[derive(Debug, Copy, Clone, PartialEq)]
    enum ATAController: u8 {
        0x20 => SingleDMA,
        0x30 => ChainedDMA
    }

    #[derive(Debug, Copy, Clone, PartialEq)]
    enum SerialATA: u8 {
        0x00 => VendorSpecific,
        0x01 => AHCI1,
        0x02 => SerialStorageBus
    }

    #[derive(Debug, Copy, Clone, PartialEq)]
    enum SerialAttachedSCSI: u8 {
        0x00 => SAS,
        0x01 => SerialStorageBus
    }

    #[derive(Debug, Copy, Clone, PartialEq)]
    enum NonVolatileMemoryController: u8 {
        0x01 => NVMHCI,
        0x02 => NVMExpress
    }

    #[derive(Debug, Copy, Clone, PartialEq)]
    enum VGACompatibleController: u8 {
        0x00 => VGA,
        0x01 => IBM8514Compatible
    }

    #[derive(Debug, Copy, Clone, PartialEq)]
    enum PCIBridge: u8 {
        0x00 => Normal,
        0x01 => Subtractive
    }

    #[derive(Debug, Copy, Clone, PartialEq)]
    enum RACEwayBridge: u8 {
        0x00 => Transparent,
        0x01 => Endpoint
    }

    #[derive(Debug, Copy, Clone, PartialEq)]
    enum SerialController: u8 {
        0x00 => U8250Compatible,
        0x01 => U16450Compatible,
        0x02 => U16550Compatible,
        0x03 => U16650Compatible,
        0x04 => U16750Compatible,
        0x05 => U16850Compatible,
        0x06 => U16950Compatible
    }

    #[derive(Debug, Copy, Clone, PartialEq)]
    enum ParallelController: u8 {
        0x00 => Standard,
        0x01 => Bidirectional,
        0x02 => ECP1Compliant,
        0x03 => IEEE1284Controller,
        0x04 => IEEE1284Device
    }

    #[derive(Debug, Copy, Clone, PartialEq)]
    enum ModemController: u8 {
        0x00 => Generic,
        0x01 => Hayes16450Compatible,
        0x02 => Hayes16550Compatible,
        0x03 => Hayes16650Compatible,
        0x04 => Hayes16750Compatible
    }

    #[derive(Debug, Copy, Clone, PartialEq)]
    enum PIC: u8 {
        0x00 => Generic8259Compatible,
        0x01 => ISACompatible,
        0x02 => EISACompatible,
        0x10 => IOAPIC,
        0x20 => IOxAPIC
    }

    #[derive(Debug, Copy, Clone, PartialEq)]
    enum DMAController: u8 {
        0x00 => Generic8237Compatible,
        0x01 => ISACompatible,
        0x02 => EISACompatible
    }

    #[derive(Debug, Copy, Clone, PartialEq)]
    enum Timer: u8 {
        0x00 => Generic8254Compatible,
        0x01 => ISACompatible,
        0x02 => EISACompatible,
        0x03 => HPET
    }

    #[derive(Debug, Copy, Clone, PartialEq)]
    enum RTCController: u8 {
        0x00 => Generic,
        0x01 => ISACompatible
    }

    #[derive(Debug, Copy, Clone, PartialEq)]
    enum GameportController: u8 {
        0x00 => Generic,
        0x10 => Extended
    }

    #[derive(Debug, Copy, Clone, PartialEq)]
    enum FireWireController: u8 {
        0x00 => Generic,
        0x10 => OHCI
    }

    #[derive(Debug, Copy, Clone, PartialEq)]
    enum USBController: u8 {
        0x00 => UHCIController,
        0x10 => OHCIController,
        0x20 => EHCIController,
        0x30 => XHCIController,
        0x80 => Unspecified,
        0xFE => Device
    }

    #[derive(Debug, Copy, Clone, PartialEq)]
    enum IPMIInterface: u8 {
        0x00 => SMIC,
        0x01 => KeyboardControllerStyle,
        0x02 => BlockTransfer
    }
}

macro_rules! impl_iso_transmute {
    { $b:ty: $a:ty } => {
        impl From<$a> for $b {
            fn from(val: $a) -> $b {
                unsafe {
                    core::mem::transmute(val)
                }
            }
        }

        impl Into<$a> for $b {
            fn into(self) -> $a {
                unsafe {
                    core::mem::transmute(self)
                }
            }
        }
    }
}

#[bitfield]
#[derive(Debug, Clone, Copy)]
pub struct ConfigAddress {
    _res1: B2,
    pub register: B6,
    pub function: B3,
    pub device: B5,
    pub bus: B8,
    _res2: B7,
    pub enable: bool
}

impl_iso_transmute!(ConfigAddress: u32);

const CONFIG: PortRegisters<ConfigAddress, u32, u32> = PortRegisters::new(Port::new(0xcf8), Port::new(0xcfc));

#[derive(Debug, Copy, Clone)]
pub struct DeviceVendor {
    pub vendor_id: u16,
    pub device_id: u16
}

impl_iso_transmute!(DeviceVendor: u32);

#[derive(Debug, Copy, Clone)]
pub struct StatusCommand {
    pub command: u16,
    pub status: u16
}

impl_iso_transmute!(StatusCommand: u32);

#[derive(Debug, Copy, Clone)]
pub struct ClassCode {
    pub revision: u8,
    pub prog_if: u8,
    pub subclass: u8,
    pub class: u8
}

impl_iso_transmute!(ClassCode: u32);


#[derive(Debug, Copy, Clone)]
pub struct BISTHeaderLatency {
    pub cache_line_size: u8,
    pub latency_timer: u8,
    pub header_type: u8,
    pub bist: u8
}

impl_iso_transmute!(BISTHeaderLatency: u32);

#[derive(Debug, Copy, Clone)]
pub struct LatencyGrantInterrupt {
    pub interrupt_line: u8,
    pub interrupt_pin: u8,
    pub min_grant: u8,
    pub max_latency: u8
}

impl_iso_transmute!(LatencyGrantInterrupt: u32);

#[derive(Debug, Clone, Copy)]
pub enum BAR {
    IO(usize),
    Mem(usize)
}

impl From<u32> for BAR {
    fn from(val: u32) -> Self {
        if val & 1 == 0 {
            BAR::Mem(val as usize & 0xFFFFFFF0)
        } else {
            BAR::IO(val as usize & 0xFFFFFFFC)
        }
    }
}

impl Into<u32> for BAR {
    fn into(self) -> u32 {
        match self {
            BAR::IO(i) => i as u32 | 1,
            BAR::Mem(i) => i as u32
        }
    }
}

type ConfigRegister<U> = PortRegister<ConfigAddress, U, u32, u32>;

#[derive(Clone)]
pub struct Device {
    pub bus: u8,
    pub device: u8,
    pub function: u8,
    pub device_vendor: ConfigRegister<DeviceVendor>,
    pub status_command: ConfigRegister<StatusCommand>,
    pub class_code: ConfigRegister<ClassCode>,
    pub bist_header: ConfigRegister<BISTHeaderLatency>,
    pub bar0: ConfigRegister<BAR>,
    pub bar1: ConfigRegister<BAR>,
    pub bar2: ConfigRegister<BAR>,
    pub bar3: ConfigRegister<BAR>,
    pub bar4: ConfigRegister<BAR>,
    pub bar5: ConfigRegister<BAR>,
    pub latency_grant: ConfigRegister<LatencyGrantInterrupt>
}

impl Device {
    fn new(bus: u8, device: u8, function: u8) -> Option<Device> {
        let mut address = ConfigAddress::new();
        address.set_register(0);
        address.set_function(function);
        address.set_bus(bus);
        address.set_device(device);
        address.set_enable(true);
        if (CONFIG.get::<u32>(address).read() & 0xffff) != 0xffff {
            let device_vendor = CONFIG.get(address);
            address.set_register(1);
            let status_command = CONFIG.get(address);
            address.set_register(2);
            let class_code = CONFIG.get(address);
            address.set_register(3);
            let bist_header = CONFIG.get(address);
            address.set_register(4);
            let bar0 = CONFIG.get(address);
            address.set_register(5);
            let bar1 = CONFIG.get(address);
            address.set_register(6);
            let bar2 = CONFIG.get(address);
            address.set_register(7);
            let bar3 = CONFIG.get(address);
            address.set_register(8);
            let bar4 = CONFIG.get(address);
            address.set_register(9);
            let bar5 = CONFIG.get(address);
            address.set_register(15);
            let latency_grant = CONFIG.get(address);

            Some(Device {
                bus, device, function,
                device_vendor, status_command,
                class_code, bist_header,
                bar0, bar1, bar2,
                bar3, bar4, bar5,
                latency_grant
            })
        } else {
            None
        }
    }

    pub fn class(&self) -> DeviceClass {
        let class_code = self.class_code.read();
        DeviceClass::new(&[class_code.class, class_code.subclass, class_code.prog_if])
    }

    pub fn multifunction(&self) -> bool {
        self.bist_header.read().header_type & 0x80 != 0
    }
}

fn check_function(device: &Device, devices: &mut Vec<Device>) {
    if let DeviceClass::Bridge(Bridge::PCI(_)) = device.class() {
        let bar: u32 = device.bar2.read().into();
        let bus = (bar >> 8) as u8;
        log!("checking pci bridge to bus {}", bus);
        check_bus(bus, devices);
    }
}

fn check_device(bus: u8, device: u8, devices: &mut Vec<Device>) {
    if let Some(rdevice) = Device::new(bus, device, 0) {
        if rdevice.multifunction() {
            for function in 0..8 {
                if let Some(mdevice) = Device::new(bus, device, function) {
                    log!("{}/{}/{} is a {:?}", bus, device, function, mdevice.class());
                    check_function(&mdevice, devices);
                    devices.push(mdevice);
                }
            }
        } else {
            check_function(&rdevice, devices);
            log!("{}/{}/{} is a {:?}", bus, device, 0, rdevice.class());
            devices.push(rdevice);
        }
    }
}

fn check_bus(bus: u8, devices: &mut Vec<Device>) {
    for device in 0..32 {
        check_device(bus, device, devices);
    }
}

static mut DEVICES: Vec<Device> = Vec::new();

pub fn init() {
    if let Some(host_controller) = Device::new(0, 0, 0) {
        if host_controller.multifunction() {
            for function in 0..8 {
                if Device::new(0, 0, function).is_some() {
                    log!("checking host controller {}", function);
                    check_bus(function, unsafe { &mut DEVICES });
                }
            }
        } else {
            log!("checking host controller 0");
            check_bus(0, unsafe { &mut DEVICES });
        }
    }
}

pub fn devices() -> &'static [Device] {
    unsafe {
        &DEVICES
    }
}