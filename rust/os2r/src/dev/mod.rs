pub mod port;
//pub mod pci;

//pub mod ata;
pub mod pic;
pub mod serial;
pub mod keyboard;

pub fn init() {
    //pci::init();
    //ata::init();
    keyboard::init();
}