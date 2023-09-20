	/*
example:
	
if 3=3:
  x = 30 + -5
  print(x)
else:
  y = 25 + -10


	*/

	.globl main

main:
	pushq %rbp
	movq %rsp, %rbp
        subq $16, %rsp
	movq $5, -8(%rbp)
	cmp  $3, -8(%rbp)
	je block_1
	jmp block_2

block_1:
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

block_2:
	movq $10, -8(%rbp)
        negq -8(%rbp)
        movq -8(%rbp), %rax 
        movq $30, -16(%rbp)
	addq -16(%rbp), %rax
        movq %rax, %rdi
        callq print_int
	addq $16, %rsp
	popq %rbp
	retq
