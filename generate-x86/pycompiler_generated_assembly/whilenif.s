	/*
	[ ("movq", ImmInt 0, ImmStr "x")
, ("loop", ImmStr "dummy", ImmStr "dummy")
, ("cmpq", ImmInt 3, ImmStr "x")
, ("jmp", ImmStr "block_0", ImmStr "dummy")
, ("je", ImmStr "block_1", ImmStr "dummy")
, ("block_0", ImmStr "blkdummy", ImmStr "dummy")
, ("movq", ImmStr "x", ImmReg "%rdi")
, ("print", ImmStr "dummy", ImmStr "dummy")
, ("movq", ImmInt 1, ImmStr "x")
, ("addq", ImmStr "x", ImmStr "x")
, ("block_1", ImmStr "dummy", ImmStr "dummy")
, ("movq", ImmInt 3, ImmReg "%rdi")
, ("print", ImmStr "dummy", ImmStr "dummy")
, ("test", ImmStr "tst", ImmStr "tstdummy")
, ("cmpq", ImmInt 4, ImmStr "x")
, ("jg", ImmStr "loop", ImmStr "dummy")
	]
	*/
	.globl main
main:	
	movq $0, %rbx
	jmp whiletest

whiletest:
	cmpq $4, %rbx
	jge exit
	jmp iftest

iftest:
	cmpq $3, %rbx
	je ifelse
	jmp ifthn

ifthn:
	movq %rbx, %rdi
	callq print_int
	incq %rbx
	jmp whiletest

ifelse:
	movq $3, %rdi
	callq print_int
	incq %rbx
	jmp whiletest

exit:
	retq

