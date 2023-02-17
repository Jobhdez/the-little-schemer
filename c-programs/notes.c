

// array of pointers to type

char *array[] = {"unix",
		 "linux",
		 "macOS"};
/*
in the above example each element of the array
is a pointer to each of those strings; so each
element is a pointer to a type 'char'; e.g.,:
 */

array[0]; // points to "unix"
array[1]; // points to "linux"
array[2]; // points to "macOs"

// what is a pointer, to start with

int x = 1, y = 2, z[10];
int *ip;    // ip is a pointer to int
ip = &x;    // ip now points to the address of x
y = *ip;    // y is now 1 because when the deferencing operator is applied to a pointer ie `ip` above it accesses the object the pointer points to
*ip = 0;    // x is now 0
ip = &z[0]; // ip now points to z[0]

/*
if a double pointer is just a pointer that points to a pointer then what this mean in the context
of a 2d array?

lets start with the relationship between pointers
and arrays: "any operation that can be achieved by array subscripting can also be done with pointers.

e.g.,
 */

int a[10]; //defines an array of size 10, that is a block of 10 consecutive objects
           // a[i] refers to the i-th element of the array; so if `pa` is a pointer to an integer declared as:
int *pa; // then the assignment
pa = &a[0]; // sets `pa` to point to element zero of `a`; that is. `pa` contains the address of a[0]

// so
x = *pa; // will copy the contents of a[0] to x

// deferencing operator applied to a function
 char *alloc(int n) // return a pointer to n characters
 {//...
 }

// given the above example:
int **add_matrices(int m[2][4], int m2[2][4], int size) {
  //...
}
/* here in the above `**add_matrices` it returns
 a pointer that points to a pointer to m[2][4] ints; in this case it points to four ints */
  
