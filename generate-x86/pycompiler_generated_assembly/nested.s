	/*
	original code:
	
def test(x y):
   n = 10
   while x < 20:
      if x < n:
         j = 10 + -3
	 print(j+10)
      else:
         j = 10 + -6
	print(j + 3);;
      x = x + 1
(#S(INSTRUCTION :NAME "movq" :ARG1 "rdi" :ARG2 "-8(%rbp)")
 #S(INSTRUCTION :NAME "movq" :ARG1 "rsi" :ARG2 "-16(%rbp)")
 #S(INSTRUCTION :NAME "movq" :ARG1 #S(IMMEDIATE :INT 10) :ARG2 "-24(%rbp)")
 #S(INSTRUCTION :NAME "jmp" :ARG1 "test" :ARG2 NO-ARG)
 #S(INSTRUCTION :NAME "jg" :ARG1 "loop" :ARG2 NO-ARG)
 #S(BLOCK-PY :NAME "loop:")
 #S(INSTRUCTION :NAME "cmpq" :ARG1 "-24(%rbp)" :ARG2 "-8(%rbp)")
 #S(INSTRUCTION :NAME "je" :ARG1 "block_1" :ARG2 NO-ARG)
 #S(INSTRUCTION :NAME "jmp" :ARG1 "block_2" :ARG2 NO-ARG)
 #S(BLOCK-PY :NAME "block_1")
 #S(INSTRUCTION :NAME "movq" :ARG1 3 :ARG2 "-32(%rbp)")
 #S(INSTRUCTION :NAME "subq" :ARG1 "-32(%rbp)" :ARG2 NO-ARG)
 #S(INSTRUCTION :NAME "movq" :ARG1 #S(IMMEDIATE :INT 10) :ARG2 "-40(%rbp)")
 #S(INSTRUCTION :NAME "addq" :ARG1 "-32(%rbp)" :ARG2 "-40(%rbp)")
 #S(INSTRUCTION :NAME "movq" :ARG1 10 :ARG2 "-48(%rbp)")
 #S(INSTRUCTION :NAME "addq" :ARG1 "-40(%rbp)" :ARG2 "-48(%rbp)")
 #S(CALLQ :LABEL "print_int") #S(BLOCK-PY :NAME "block_2")
 #S(INSTRUCTION :NAME "movq" :ARG1 6 :ARG2 "-48(%rbp)")
 #S(INSTRUCTION :NAME "subq" :ARG1 "-48(%rbp)" :ARG2 NO-ARG)
 #S(INSTRUCTION :NAME "movq" :ARG1 #S(IMMEDIATE :INT 10) :ARG2 "-40(%rbp)")
 #S(INSTRUCTION :NAME "addq" :ARG1 "-48(%rbp)" :ARG2 "-40(%rbp)")
 #S(INSTRUCTION :NAME "movq" :ARG1 3 :ARG2 "-56(%rbp)")
 #S(INSTRUCTION :NAME "addq" :ARG1 "-40(%rbp)" :ARG2 "-56(%rbp)")
 #S(CALLQ :LABEL "print_int") #S(BLOCK-PY :NAME "test:")
 #S(INSTRUCTION :NAME "cmpq" :ARG1 #S(IMMEDIATE :INT 20) :ARG2 "-8(%rbp)"))

update:
def test(x y):
	n = 10
	x=0
   while x < 20:
      if x < n:
         j = 10 + -3
	print(j+10)
	x = x + 1
      else:
         j = 10 + -6
	print(j + 3)
	x = x + 1;;
      
	*/


	.globl main
main:
	pushq %rbp
	movq %rsp, %rbp
	subq $48, %rsp
	movq $10, %rbx
	movq $0, %r15
	jmp test

test:
	cmpq $20, %r15
	jge exit

	cmp %rbx, %r15
	jl block_1
	jmp block_2
block_1:	
	movq $10, -8(%rbp)
	movq $3, -16(%rbp)
	negq -16(%rbp)
	movq -16(%rbp), %rax
	addq -8(%rbp), %rax
	movq $10, -32(%rbp)
	addq -32(%rbp), %rax
	movq %rax, %rdi
	callq print_int
	incq %r15
	jmp test

block_2:
	movq $10, -8(%rbp)
	movq $6, -16(%rbp)
	negq -16(%rbp)
	movq -16(%rbp), %rax
	addq -8(%rbp), %rax
	movq %rax, %rdi
	callq print_int
	incq %r15
	jmp test
	

exit:
	addq $48, %rsp
	popq %rbp
	retq
	
