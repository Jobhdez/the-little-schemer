	.section .data
matrix:
	.quad 2, 3
	.quad 5, 6

	.section .text
	.globl main

main:
	leaq matrix(%rip), %r15
	movq $0, %r13  /* i */
	movq $0, %r14  /* j */
	jmp test

test:
	cmpq $2, %r13
	jne test_2
	jmp exit

test_2:
	cmpq $2, %r14
	jne body
	jmp body_2

body:
	leaq (%r14, %r13, 2), %rax  
	leaq (%r15, %rax, 8), %rdx
	movq (%rdx), %rdi
	callq print_int
	incq %r14
	jmp test_2

body_2:
	incq %r13
	movq $0, %r14
	jmp test

exit:
	retq
