pub mod phys;
pub mod page;
pub mod heap;
pub mod virt;

pub fn init(boot_info: &multiboot2::BootInformation, framebuffer: Option<(usize, usize)>) {
    let elf_sections = trace!(boot_info.elf_sections_tag().unwrap());
    let end_of_kernel = trace!(elf_sections.sections().map(|s| s.end_address()).max().unwrap());
    let start_of_kernel = trace!(elf_sections.sections().map(|s| s.start_address()).min().unwrap());

    trace!(phys::init(start_of_kernel, end_of_kernel, boot_info, framebuffer));
    trace!(page::init(end_of_kernel, boot_info, framebuffer));
    trace!(heap::init());
}