	/*
	the following program is correct code that a Python compiler should
	generate for the program:

	x = 50 + -10
	print(x + 10)
	->
	temp1 = -10
	x = 50 + temp1
	temp2 = x + 10
	->
	movq $10, temp1
	negq temp1
	movq $50, x
	addq temp1, x
	movq x, temp2
	addq $10, temp2


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
