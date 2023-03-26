	
	SECTION .data

a:	 dq 5
fmt:	 db "a=%ld, rax=%ld", 10, 0

	SECTION .text
	global main
	extern printf

main:
	push rbp
	mov rax, [a]
	add rax, 2
	mov rdi, fmt
	mov rsi, [a]
	mov rdx, rax
	mov rax, 0
	call printf

	pop rbp
	mov rax, 0
	ret
