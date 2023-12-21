	/*

(list
 (atomic-assignment
  (atomic (py-id "temp_0"))
  (anf-if-exp (atomic (py-bool 'True)) (atomic (py-bool 'True)) (atomic (py-bool 'False))))
 (atomic-assignment
  (atomic (py-id "temp-1"))
  (anf-if-exp (atomic (py-id "temp_0")) (atomic (py-bool 'False)) (atomic (py-bool 'True))))
 (anf-if-exp (atomic (py-id "temp-1")) (atomic (py-num 1)) (atomic (py-num 3))))
	to-anf.rkt>
	if (if (if true then true else true) then false else true) then 1 else 3
	*/
	.section .data
true_value:
    .quad 1

false_value:
   .quad 0
	.section .text
	.globl main
main:
	pushq %rbp
	movq %rsp, %rbp
	subq $16, %rsp
        movq true_value(%rip), %rax   
        cmpq $0, %rax            
        je false_branch          

true_branch:

    movq true_value(%rip), %r15
  
    jmp block_1

false_branch:
	movq false_value(%rip), %r15
	jmp block_1

block_1:
	cmpq $1, %r15
	je block_2
	jmp block_3

block_2:
	movq false_value(%rip), %r13
	jmp block_4

block_3:
	movq true_value(%rip), %r13
	jmp block_4

block_4:
	cmpq $1, %r13
	je block_5
	jmp block_6

block_5:
	movq $1, %r14
	jmp end_program

block_6:
	movq $3, %r14
	jmp end_program
	

end_program:
	movq %r14, %rdi
	callq print_int
	addq $16, %rsp
	popq %rbp
	retq
	

