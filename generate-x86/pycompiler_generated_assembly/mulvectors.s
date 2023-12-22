	/*

	i=0
	vec = [1,2,3,4,5]
	vec2 = [2,3,4,5,6]
	while i< 5:
	print(vec[i] * vec2[i]
	i = i + 1
	*/

	.section .data
vec:
	.quad 1,2,3,4,5
vec_2:
	.quad 2,3,4,5,6
	
	.section .text
	
	.globl main
main:
	leaq vec(%rip), %rbx
	leaq vec_2(%rip), %r15
	movq $0, %r14
	jmp test

body:
	movq (%rbx, %r14, 8), %r13
	imulq (%r15, %r14, 8), %r13
	movq %r13, %rdi
	callq print_int
	incq %r14

test:
	cmpq $5, %r14
	jne body
	jmp exit

exit:
	retq
