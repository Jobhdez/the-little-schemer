	/*

sum = 0
i = 5
while i > 0:
    sum = sum + i
    i = i - 1
print(sum)
	*/

	.globl main

main:
	movq $0, %rbx
	movq $5, %r15
	jmp test

body:
	addq %r15, %rbx
	subq $1, %r15
	jmp test

test:
	cmpq $0, %r15
	jne body
	jmp exit

exit:
	movq %rbx, %rdi
	callq print_int
	retq
