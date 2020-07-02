ENTRY(start)

SECTIONS {
	. = 1M;
	.boot :
	{
		KEEP(*(.multiboot))
	}

	.text :
	{
		KEEP(*(.text))
	}

	.bss : 
	{
		KEEP(*(.bss))
	}

	.rodata :
	{
		KEEP(*(.rodata))
	}
}
