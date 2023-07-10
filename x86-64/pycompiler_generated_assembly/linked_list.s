	/*
struct linked_list {
	long id; // the data in this link: one student ID
	struct linked_list *next; // the next node, or NULL if none
};
struct linked_list tail={7,NULL};
struct linked_list mid={4,&tail};
struct linked_list start={2,&mid};

struct linked_list *cur;
for (cur=&start; cur!=NULL; cur=cur->next) {
	printf("Node %p has id %ld\n",
	             cur,       cur->id);
}




	*/

push rbx
mov rbx, listStart

loopAgain:
	mov rdi,QWORD[rbx] ; load student ID
	extern print_int
	call print_int
	mov rbx,QWORD[rbx+8] ; move to next student
	cmp rbx,0 ; check for NULL
	jne loopAgain

	pop rbx
	ret

listStart:
	dq 2       ; offset 0: student ID
	dq listMid ; offset 8: next link

listMid:
	dq 4       ; offset 0: student ID
	dq listEnd ; offset 8: next link

listEnd:
	dq 7       ; offset 0: student ID
	dq 0       ; offset 8: next link (0 indicates the end of the list)
 
