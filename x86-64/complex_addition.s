start:
	movq $10, -8(%rbp)
	negq -8(%rbp)
	movq -8(%rbp), %rax
	addq $30, %rax
	jmp conclusion

	.globl main

main:
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
	jmp start

conclusion:
	addq $16, %rsp
	popq %rbp
	retq
