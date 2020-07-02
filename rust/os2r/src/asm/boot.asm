[section .bss]

stack_bottom:
resb 262144
stack_top:

[section .text]
[bits 32]
[extern kmain]
[global start]
start:
	mov esp, stack_top 	; Initialize the stack to the kernel stack.
	push ebx			; Push the multiboot header pointer that GRUB passed us onto the stack.
	call kmain			; Call into the kernel.
halt:
	hlt					; If the kernel exits, just busy loop.
	jmp halt
