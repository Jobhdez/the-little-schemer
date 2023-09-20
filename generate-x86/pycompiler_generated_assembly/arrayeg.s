	/*
	this is x86-64 that represents the following code:

	for i in array:
	    print(array[i] + 2)

	*/

.section .data

array:
    .quad 1,2,3,4,5
    .section .text
    
    .globl main
    
main:
    leaq array(%rip), %rbx
    movq $0, %r15
    jmp test

body:
    
    movq (%rbx, %r15, 8), %rdi
    addq $2, %rdi	
    callq print_int
    incq %r15
    jmp test

test:
    cmpq $5, %r15
    jne body
    jmp exit

exit:
    retq 
    
