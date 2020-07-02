pub mod gdt;
#[macro_use]
pub mod isr;

pub fn init() {
    gdt::init();
    isr::init();
}