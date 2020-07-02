use core::marker::PhantomData;

pub trait PortWrite<K>: Sized + Into<K> {
    unsafe fn write(self, port: &Port<Self, K>);
}

pub trait PortRead<K>: Sized + From<K> {
    unsafe fn read(port: &Port<Self, K>) -> Self;
}

impl<T: Into<u8>> PortWrite<u8> for T {
    #[inline(always)]
    unsafe fn write(self, port: &Port<Self, u8>) {
        llvm_asm!("out dx, al" : : "{dx}"(port.addr), "{al}"(self.into()) : : "intel");
    }
}

impl<T: From<u8>> PortRead<u8> for T {
    #[inline(always)]
    unsafe fn read(port: &Port<Self, u8>) -> Self {
        let r: u8;
        llvm_asm!("in al, dx" : "={al}"(r) : "{dx}"(port.addr) : : "intel");
        r.into()
    }
}


impl<T: Into<u16>> PortWrite<u16> for T {
    #[inline(always)]
    unsafe fn write(self, port: &Port<Self, u16>) {
        llvm_asm!("out dx, ax" : : "{dx}"(port.addr), "{ax}"(self.into()) : : "intel");
    }
}

impl<T: From<u16>> PortRead<u16> for T {
    #[inline(always)]
    unsafe fn read(port: &Port<Self, u16>) -> Self {
        let r: u16;
        llvm_asm!("in ax, dx" : "={ax}"(r) : "{dx}"(port.addr) : : "intel");
        r.into()
    }
}

impl<T: Into<u32>> PortWrite<u32> for T {
    #[inline(always)]
    unsafe fn write(self, port: &Port<Self, u32>) {
        llvm_asm!("out dx, eax" : : "{dx}"(port.addr), "{eax}"(self.into()) : : "intel");
    }
}

impl<T: From<u32>> PortRead<u32> for T {
    #[inline(always)]
    unsafe fn read(port: &Port<Self, u32>) -> Self {
        let r: u32;
        llvm_asm!("in eax, dx" : "={eax}"(r) : "{dx}"(port.addr) : : "intel");
        r.into()
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Port<T, K=T> {
    addr: u16,
    phantom: PhantomData<(T, K)>
}

impl<T, K> Port<T, K> {
    pub const fn new(addr: u16) -> Port<T, K> {
        Port {
            addr, phantom: PhantomData
        }
    }

    pub fn wait(&self) {
        unsafe {
            llvm_asm!("out 0x80, al" :: "{al}"(0) :: "intel");
        }
    }
}

impl<K> Port<K, K> {
    pub fn specialize<T>(self) -> Port<T, K> {
        Port { 
            addr: self.addr, phantom: PhantomData
        }
    }
}

impl<K, T: PortWrite<K>> Port<T, K> {
    pub fn write(&self, value: T) {
        unsafe {
            value.write(self);
        }
    }
}

impl<T: Into<u8> + Copy> Port<T, u8> {
    pub fn write_log(&self, value: T) {
        let u: u8 = value.into();
        log!("0x{:02x} => 0x{:04x}", u, self.addr);
        unsafe {
            value.write(self);
        }
    }
}

impl<K, T: PortRead<K>> Port<T, K> {
    pub fn read(&self) -> T {
        unsafe {
            T::read(self)
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct PortRegisters<U, A, B> {
    index: Port<U, A>,
    data: Port<B, B>
}

impl<U, A, B> PortRegisters<U, A, B> {
    pub const fn new(index: Port<U, A>, data: Port<B, B>) -> PortRegisters<U, A, B> {
        PortRegisters { index, data }
    }
}

impl<A: Copy, B: Copy, U: Copy> PortRegisters<U, A, B> {
    pub fn get<V>(&self, idx: U) -> PortRegister<U, V, A, B> {
        PortRegister { 
            idx, index: self.index,
            data: self.data.specialize()
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct PortRegister<U, V, A, B> {
    idx: U,
    index: Port<U, A>,
    data: Port<V, B>
}

impl<A, B, U: Copy + PortWrite<A>, V: PortWrite<B>> PortRegister<U, V, A, B> {
    pub fn write(&self, val: V) {
        self.index.write(self.idx);
        self.data.write(val);
    }
}

impl<A, B, U: Copy + PortWrite<A>, V: PortRead<B>> PortRegister<U, V, A, B> {
    pub fn read(&self) -> V {
        self.index.write(self.idx);
        self.data.read()
    }
}