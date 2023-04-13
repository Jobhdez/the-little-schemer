	/*
	the following program is correct code that a Python compiler should
	generate for the program:

	x = 50 + -10
	print(x + 10)


	*/

	.globl main
main:
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
	movq $10, -8(%rbp)
	negq -8(%rbp)
	movq -8(%rbp), %rax
	movq $50, -16(%rbp)
	addq -16(%rbp), %rax
	movq $10, -24(%rbp)
	addq -24(%rbp), %rax
	movq %rax, %rdi
	callq print_int
	addq $16, %rsp
	popq %rbp
	retq
