[section .multiboot]
header_start:
	dd 0xe85250d6 													; Magic Number
	dd 0															; Architecture = x86
	dd header_end - header_start									; Header Length
	dd 0x100000000 - (0xe85250d6 + 0 + (header_end - header_start)) ; Checksum
	dw 1    														; Information Request
	dw 0															; Non-optional
	dd 16															; Size = 16
	dd 6															; Request memory-map (Tag = 6)
	dd 8															; Request framebuffer
	;dw 5															; Set Video Mode
	;dw 0															; Non-optional
	;dd 24															; Size = 24
	;dd 1024															; Width = 1024
	;dd 768  														; Height = 768
	;dd 32															; BPP = 32
	;dd 0
	dw 0															; Terminating Tag
	dw 0
	dd 8
header_end:
