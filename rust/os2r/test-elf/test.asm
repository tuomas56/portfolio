[bits 32]
[global start]
start:
    mov eax, 1
    mov ebx, msg
    mov ecx, 21
    int 0x80
    mov eax, 0
    int 0x80

msg db "Hello, from assembly!"
