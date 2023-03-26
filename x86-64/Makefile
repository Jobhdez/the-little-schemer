output: hello.o printf.o runtime2.o simple_addition.s print_exit_code.sh
	ld hello.o -o hello
	gcc -static -o printf printf.o
	gcc -g runtime2.o simple_addition.s && ./a.out
	bash print_exit_code.sh

hello.o: hello.asm
	nasm -felf64 hello.asm

printf.o: printf.asm
	nasm -felf64 printf.asm

runtime2.o: runtime2.c
	gcc -c -g -std=c99 runtime2.c

clean:
	rm *.o hello printf *.out
