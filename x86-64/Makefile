output: hello.o printf.o runtime2.o simple_addition.s complex_addition.s
	ld hello.o -o hello
	gcc -static -o printf printf.o
	gcc -g runtime2.o simple_addition.s
	gcc -g -o complex runtime2.o complex_addition.s

hello.o: hello.asm
	nasm -felf64 hello.asm

printf.o: printf.asm
	nasm -felf64 printf.asm

runtime2.o: runtime2.c
	gcc -c -g -std=c99 runtime2.c

clean:
	rm *.o hello printf *.out
