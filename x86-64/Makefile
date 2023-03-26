output: hello.o printf.o
	ld hello.o -o hello
	gcc -static -o printf printf.o

hello.o: hello.asm
	nasm -felf64 hello.asm

printf.o: printf.asm
	nasm -felf64 printf.asm

clean:
	rm *.o hello printf
