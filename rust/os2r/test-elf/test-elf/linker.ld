ENTRY(start)

SECTIONS {
	. = 1024M;

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
