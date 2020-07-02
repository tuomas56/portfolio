use super::port::Port;
use modular_bitfield::prelude::*;

#[derive(BitfieldSpecifier, Debug, Copy, Clone)]
pub enum Parity {
    Odd = 0,
    Even = 1,
    High = 2,
    Low = 3
}

#[derive(BitfieldSpecifier, Debug, Copy, Clone)]
pub enum WordLength {
    Five = 0,
    Six = 1,
    Seven = 2,
    Eight = 3
}

#[bitfield]
#[derive(Debug, Copy, Clone)]
pub struct LineControl {
    pub dlab: bool,
    pub break_enable: bool,
    pub parity_enable: bool,
    #[bits = 2]
    pub parity: Parity,
    pub two_stop_bits: bool,
    #[bits = 2]
    pub word_length: WordLength
}

#[derive(Copy, Clone)]
union LCTL {
    data: u8,
    lctl: LineControl
}

impl Into<u8> for LCTL {
    fn into(self) -> u8 {
        unsafe {
            self.data
        }
    }
}

impl From<u8> for LCTL {
    fn from(val: u8) -> Self {
        LCTL { data: val }
    }
}

#[derive(Copy, Clone)]
union DataDivLSB {
    data: u8,
    div_lsb: u8
}

impl Into<u8> for DataDivLSB {
    fn into(self) -> u8 {
        unsafe {
            self.data
        }
    }
}

impl From<u8> for DataDivLSB {
    fn from(val: u8) -> DataDivLSB {
        DataDivLSB { data: val }
    }
}

#[bitfield]
#[derive(Debug, Copy, Clone)]
pub struct InterruptEnable {
    _res1: bool,
    _res2: bool,
    pub low_power_mode: bool,
    pub sleep_mode: bool,
    pub modem_status: bool,
    pub reciever_line_status: bool,
    pub transmit_holding_empty: bool,
    pub recieved_data_available: bool
}

#[derive(Copy, Clone)]
union IERDivMSB {
    ier: InterruptEnable,
    div_msb: u8
}

impl Into<u8> for IERDivMSB {
    fn into(self) -> u8 {
        unsafe {
            self.div_msb
        }
    }
}

impl From<u8> for IERDivMSB {
    fn from(val: u8) -> IERDivMSB {
        IERDivMSB { div_msb: val }
    }
}

#[derive(BitfieldSpecifier, Debug, Copy, Clone)]
pub enum Interrupt {
    ModemStatus = 0,
    TransmitterHoldingEmpty = 1,
    RecievedDataAvailable = 2,
    RecieverLineStatus = 3
}

#[bitfield]
#[derive(Debug, Copy, Clone)]
pub struct InterruptIdentification {
    _res1: bool,
    pub fifo_enabled: bool,
    pub extended_fifo: bool,
    _res2: bool,
    pub timeout_pending: bool,
    #[bits = 2]
    pub interrupt: Interrupt,
    pub pending: bool
}

#[derive(BitfieldSpecifier, Debug, Copy, Clone)]
pub enum InterruptTriggerLevel {
    OneByte = 0,
    TwoBytes = 1,
    FourBytes = 2,
    FourteenBytes = 3
}

#[bitfield]
#[derive(Debug, Copy, Clone)]
pub struct FIFOControl {
    #[bits = 2]
    pub interrupt_trigger_level: InterruptTriggerLevel,
    pub extended_fifo: bool,
    _res: bool,
    pub switch_dma_mode: bool,
    pub clear_tx_fifo: bool,
    pub clear_rx_fifo: bool,
    pub fifo_enable: bool
}

#[derive(Copy, Clone)]
union IIRFCTL {
    data: u8,
    iir: InterruptIdentification,
    fctl: FIFOControl
}

impl From<u8> for IIRFCTL {
    fn from(val: u8) -> Self {
        IIRFCTL { data: val }
    }
}

impl Into<u8> for IIRFCTL {
    fn into(self) -> u8 {
        unsafe {
            self.data
        }
    }
}

#[bitfield]
#[derive(Debug, Copy, Clone)]
pub struct ModemControl {
    _res1: bool,
    _res2: bool,
    pub auto_flow_control: bool,
    pub loopback_mode: bool,
    pub aux_output_2: bool,
    pub aux_output_1: bool,
    pub force_rts: bool,
    pub force_dtr: bool
}

#[derive(Copy, Clone)]
union MCTL {
    data: u8,
    mctl: ModemControl
}

impl From<u8> for MCTL {
    fn from(val: u8) -> Self {
        MCTL { data: val }
    }
}

impl Into<u8> for MCTL {
    fn into(self) -> u8 {
        unsafe {
            self.data
        }
    }
}

#[bitfield]
#[derive(Debug, Copy, Clone)]
pub struct LineStatus {
    pub recieve_error: bool,
    pub empty_rx_holding: bool,
    pub empty_tx_holding: bool,
    pub break_interrupt: bool,
    pub framing_error: bool,
    pub parity_error: bool,
    pub overrun_error: bool,
    pub data_ready: bool
}

#[derive(Copy, Clone)]
union LSTA {
    data: u8,
    lsta: LineStatus
}

impl From<u8> for LSTA{
    fn from(val: u8) -> Self {
        LSTA { data: val }
    }
}

#[derive(Copy, Clone)]
pub struct SerialPort {
    data_divlsb: Port<DataDivLSB, u8>,
    ier_divmsb: Port<IERDivMSB, u8>,
    fctl: Port<IIRFCTL, u8>,
    lctl: Port<LCTL, u8>,
    mctl: Port<MCTL, u8>,
    lsta: Port<LSTA, u8>
}

impl SerialPort {
    pub const fn new(base: u16) -> SerialPort {
        SerialPort {
            data_divlsb: Port::new(base),
            ier_divmsb: Port::new(base + 1),
            fctl: Port::new(base + 2),
            lctl: Port::new(base + 3),
            mctl: Port::new(base + 4),
            lsta: Port::new(base + 5)
        }
    }

    pub fn set_data(&self, data: u8) {
        let mut lctl = unsafe {
            self.lctl.read().lctl
        };
        lctl.set_dlab(false);
        self.lctl.write(LCTL { lctl });
        self.data_divlsb.write(DataDivLSB { data });
    }

    pub fn set_data_unchecked(&self, data: u8) {
        self.data_divlsb.write(DataDivLSB { data });
    }

    pub fn data(&self) -> u8 {
        let mut lctl = unsafe {
            self.lctl.read().lctl
        };
        lctl.set_dlab(false);
        self.lctl.write(LCTL { lctl });
        unsafe {
            self.data_divlsb.read().data
        }
    }

    pub fn data_unchecked(&self) -> u8 {
        unsafe {
            self.data_divlsb.read().data
        }
    }

    pub fn set_baud_divisor(&self, divisor: u16) {
        let mut lctl = unsafe {
            self.lctl.read().lctl
        };
        lctl.set_dlab(true);
        self.lctl.write(LCTL { lctl });
        self.data_divlsb.write(DataDivLSB { div_lsb: (divisor & 0xff) as u8 });
        self.ier_divmsb.write(IERDivMSB { div_msb: (divisor >> 8) as u8 });
    }

    pub fn baud_divisor(&self) -> u16 {
        let mut lctl = unsafe {
            self.lctl.read().lctl
        };
        lctl.set_dlab(true);
        self.lctl.write(LCTL { lctl });
        let lsb = unsafe {
            self.data_divlsb.read().div_lsb
        };
        let msb = unsafe {
            self.ier_divmsb.read().div_msb
        };

        ((msb as u16) << 8) | (lsb as u16)
    }

    pub fn interrupt_enable(&self) -> InterruptEnable {
        let mut lctl = unsafe {
            self.lctl.read().lctl
        };
        lctl.set_dlab(false);
        self.lctl.write(LCTL { lctl });
        unsafe {
            self.ier_divmsb.read().ier
        }
    }

    pub fn set_interrupt_enable(&self, ier: InterruptEnable) {
        let mut lctl = unsafe {
            self.lctl.read().lctl
        };
        lctl.set_dlab(false);
        self.lctl.write(LCTL { lctl });
        self.ier_divmsb.write(IERDivMSB { ier });
    }

    pub fn interrupt_identification(&self) -> InterruptIdentification {
        unsafe {
            self.fctl.read().iir
        }
    }

    pub fn set_fifo_control(&self, fctl: FIFOControl) {
        self.fctl.write(IIRFCTL { fctl });
    }

    pub fn line_control(&self) -> LineControl {
        unsafe {
            self.lctl.read().lctl
        }
    }

    pub fn set_line_control(&self, lctl: LineControl) {
        self.lctl.write(LCTL { lctl });
    }

    pub fn modem_control(&self) -> ModemControl {
        unsafe {
            self.mctl.read().mctl
        }
    }

    pub fn set_modem_control(&self, mctl: ModemControl) {
        self.mctl.write(MCTL { mctl });
    }

    pub fn line_status(&self) -> LineStatus {
        unsafe {
            self.lsta.read().lsta
        }
    }
}

pub const COM1: SerialPort = SerialPort::new(0x3f8);
pub const COM2: SerialPort = SerialPort::new(0x2f8);
