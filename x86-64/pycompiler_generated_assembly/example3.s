	/*
	this assembly corresponds to the following python program:

	x = 10 + -3
	y = 2
	z = x + y
	print(z)


	*/

	.globl main

main:
	pushq %rbp
	movq %rsp, %rbp
	subq $32, %rsp
	movq $3, -8(%rbp)
	negq -8(%rbp)
	movq -8(%rbp), %rax
	movq $10, -16(%rbp)
	addq -16(%rbp), %rax
	movq $2, -24(%rbp)
	movq -24(%rbp), %rsi
	addq %rsi, %rax
	movq %rax, %rdi
	callq print_int
	addq $32, %rsp
	popq %rbp
	retq
