[global load_gdt]
[bits 32]
load_gdt:
    mov eax, [esp + 4]
    mov [gdtr + 2], eax
    mov ax, [esp + 8]
    mov [gdtr], ax
    lgdt [gdtr]
    mov ax, 0x10
    mov ds, ax
    mov es, ax
    mov fs, ax
    mov gs, ax
    mov ss, ax
    pop eax
    push 0x08
    push eax
    retf

gdtr dw 0
     dd 0

[global load_idt]
[bits 32]
load_idt:
    mov eax, [esp + 4]
    mov [idtr + 2], eax
    mov ax, [esp + 8]
    mov [idtr], ax
    lidt [idtr]
    ret

idtr dw 0
     dd 0

[extern main_handler]
[extern main_handler_err]

%macro handler_ex 1
    [global int%1]
    int%1:
        push edi
        push esi
        push edx
        push ecx
        push ebx
        push eax
        push dword %1
        cld
        call main_handler
        add esp, 4
        pop eax
        pop ebx
        pop ecx
        pop edx
        pop esi
        pop edi
        iret
%endmacro

%macro handler_ex_err 1
    [global int%1]
    int%1:
        push edi
        push esi
        push edx
        push ecx
        push ebx
        push eax
        push dword %1
        cld
        call main_handler_err
        add esp, 4
        pop eax
        pop ebx
        pop ecx
        pop edx
        pop esi
        pop edi
        add esp, 4
        iret
%endmacro



handler_ex 0
handler_ex 1
handler_ex 2
handler_ex 3
handler_ex 4
handler_ex 5
handler_ex 6
handler_ex 7
handler_ex_err 8
handler_ex 9
handler_ex_err 10
handler_ex_err 11
handler_ex_err 12
handler_ex_err 13
handler_ex_err 14
handler_ex 15
handler_ex 16
handler_ex_err 17
handler_ex 18
handler_ex 19
handler_ex 20
handler_ex 21
handler_ex 22
handler_ex 23
handler_ex 24
handler_ex 25
handler_ex 26
handler_ex 27
handler_ex 28
handler_ex 29
handler_ex_err 30
handler_ex 31

%assign i 32
%rep 224
    handler_ex i
%assign i i+1
%endrep

[global set_page_directory]
[bits 32]
set_page_directory:
    mov eax, [esp + 4]
    mov cr3, eax
    ret

[global enable_paging]
[bits 32]
enable_paging:
    mov eax, cr0
    or eax, 1 << 31
    mov cr0, eax
    ret