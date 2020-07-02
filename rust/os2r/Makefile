.PHONY: build clean iso run

build:
	nasm -felf32 -o build/multiboot.asm.o src/asm/multiboot.asm
	nasm -felf32 -o build/boot.asm.o src/asm/boot.asm
	nasm -felf32 -o build/utils.asm.o src/asm/utils.asm
	cargo xbuild --release --features=trace --target=i686-elf.json
	i686-elf-ld -n -T linker.ld -o iso/boot/kernel.bin build/*.o target/i686-elf/release/libos.a /Users/tuomas/opt/cross/lib/gcc/i686-elf/5.3.0/libgcc.a
	i686-elf-objcopy --only-keep-debug iso/boot/kernel.bin output/kernel.sym
	i686-elf-objcopy --strip-debug iso/boot/kernel.bin

iso: build
	python3 package.py
	grub-mkrescue -o output/os.iso iso

clean:
	rm build/*
	rm iso/boot/kernel.bin
	cargo clean

run: iso
	qemu-system-i386 -cdrom output/os.iso -serial stdio
	@printf '\033[0m'

flash: build
	sudo etcher output/os.iso

test-elf: FORCE
	cd test-elf/test-elf && cargo xbuild --release --target=i686-elf.json && i686-elf-ld -Tlinker.ld --gc-sections -o ../../files/test_elf.bin target/i686-elf/release/libtest_elf.a

FORCE: