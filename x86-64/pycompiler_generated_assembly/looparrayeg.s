	/*
sum = 0
i = 0
array = [1,2,3,4,5]	
while i < 5:
    sum = sum + array[i]
    i = i + 1
print(sum)
	*/

	.section .data

array:
	.quad 1,2,3,4,5

	.section .text

	.globl main

main:
	leaq array(%rip), %rbx 	/* get the address of the array*/
	movq $0, %r14 		/*sum = 0 */
	movq $0, %r15		/*i=0 */
	jmp test

body:
	addq (%rbx, %r15, 8), %r14
	incq %r15
	jmp test

test:
	cmpq $5, %r15
	jne body
	jmp exit

exit:
	movq %r14, %rdi
	callq print_int
	retq
	
