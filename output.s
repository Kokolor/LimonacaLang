section .text
global _start
_start:
    push rbp
    mov rbp, rsp
    mov rax, 3
    push rax
    mov rax, 42
    pop rbx
    add rax, rbx
    mov [rbp-4], eax
    mov rax, 0      ; Return value
    mov rsp, rbp    ; Restore stack
    pop rbp         ; Restore base pointer
    mov rdi, rax    ; Exit code
    mov rax, 60     ; sys_exit
    syscall
