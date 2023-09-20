	/*
this is an example of what a "Python" compiler should generate for
program:
	x = 50 + -10
	print(x)
	|
	v
	temp_0 = -10
	x = 50 + temp_0
	print(x)
	|
	V
        movq $10, temp_0
	negq temp_0
	movq temp_0, x
	addq $50, x
	movq x, %rdi
	callq print_int
	|
	v
	***assign home pass: replace variables for stack locations***
	movq $10, -8(%rbp)
	negq -8(%rbp)
	movq -8(%rbp), -16(%rbp)
	addq $50, -16(%rbp)
	movq -16(%rbp), %rdi
	callq print_int
	|
	V
	**patch instructions: makes sure each instruction adheres to the rule that at most one argument could be a memory reference***
	movq $10, -8(%rbp)
	negq -8(%rbp)
	movq -8(%rbp), %rax
	movq %50, -16(%rbp)
	addq -16(%rbp), %rax
	movq %rax, %rdi
	callq print_int

--------------------------------------------------
Note:	theres a prelude and a conclusion which allocate stack space i belive.

	to run this program:
	gcc -g runtime2.o example1.s
	./a.out
	

	*/

	.globl main
	
main:
	pushq %rbp
	movq %rsp, %rbp
        subq $16, %rsp
        movq $5, -8(%rbp)
        negq -8(%rbp)
        movq -8(%rbp), %rax 
        movq $50, -16(%rbp)
	addq -16(%rbp), %rax
        movq %rax, %rdi
        callq print_int
	addq $16, %rsp
	popq %rbp
	retq
        
        
