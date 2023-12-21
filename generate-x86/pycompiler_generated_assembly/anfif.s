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
    movq true_value(%rip), %rax   
    cmpq $1, %rax            
    je false_branch          

true_branch:

    movq $3, %r14
  
    jmp end_program

false_branch:
	movq $5, %r14
	jmp end_program

end_program:
	movq %r14, %rdi
	callq print_int
	retq
	

