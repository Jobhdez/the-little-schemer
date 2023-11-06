	/*
	this is x86-64 that represents the following code:

	for i in array:
	   print(array[i] + 2)

Notes:
	x86 assembly exposes the registers of the x86 cpu and the program pointer. the program pointer is the address of the next instruction. When using memory addresses instead of registers these memory addresses are actually part of the virtual memory address space. The program counter is called %rip in x86-64. The register file of x86-64 consists of 16 named locations storing 64bit values i.e., registers. The register %rsp is the stack pointer, ie indicates the end positoon of the run time stack.

	stack, last in, first out

	the operands of the instructions defined by the isa consist of
	immediates, stack locations, and registers.

	the stack grows to lower addresses. in textbooks a stack is
	depicted upside down

	                             stack bottom
                                  ^
 (stack grows to lower addresses) |
                                  |   stack top
	pushing data to stack involves decrementing the stack pointer.

	*/

.section .data

array:
    .quad 1,2,3,4,5
    .section .text
    
    .globl main
    
main:
    leaq array(%rip), %rbx // gets the address of array using the program counter; the program counter points to the address of the next instruction.
    movq $0, %r15	   // initializes i to 0
    jmp test

body:
    
    movq (%rbx, %r15, 8), %rdi // using the adress of the array and current value of i and the size of the array numbers (number of bits) this gets array[i]. this essentially is equivalent to xA + L * i where xA is a pointer to the starting location of the array, L is the size of data type L.
	
    addq $2, %rdi // this adds two to a[i]	
    callq print_int // it prints a[i] + 2
    incq %r15 // increments i by 1
    jmp test

test:
    cmpq $5, %r15 // if i > 5 (size of array) go to body; otherwise exit
    jne body
    jmp exit

exit:
    retq 
    
