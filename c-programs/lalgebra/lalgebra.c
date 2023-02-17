// author: Job Hernandez <hj93@protonmail.com>

#include <stdio.h>
#include <stdlib.h>

// vectors


/*

  @param vec: a vector
  @param vec2: a vector
  @returns: the sum of VEC and VEC2


*/
int *add_vectors(int vec[], int vec2[], int size) {

  int *sum = malloc(size);

  if (!sum) {

    return NULL;

  }

  for (int i = 0; i < size; i++) {

    sum[i] = vec[i] + vec2[i];

    

  }

  return sum;

}

/*

  @param vec: a vector
  @param vec2: a vector
  @returns: the subtraction of VEC and VEC2


*/
int *sub_vectors(int vec[], int vec2[], int size) {

  int *sub = malloc(size);

  if (!sub) {

    return NULL;

  }

  for (int i = 0; i < size; i++) {

    sub[i] = vec[i] - vec2[i];

  }

  return sub;


}
/*

  @param vec: a vector
  @param vec2: a vector
  @returns: the multiplication of VEC and VEC2


*/
int *mul_vectors(int vec[], int vec2[], int size) {

  int *mul = malloc(size);

  if (!mul) {

    return NULL;

  }

  for (int i = 0; i < size; i++) {

    mul[i] = vec[i] * vec2[i];

  }

  return mul;

}

/*

  @param vec: a vector
  @param scalar: a scalar
  @returns: the product of VEC and the SCALAR


*/

int *mul_scalar(int vec[], int scalar, int size) {

  int *mul = malloc(size);

  if (!mul) {

    return NULL;

  }

  for (int i = 0; i < size; i++) {

    mul[i] = vec[i] * scalar;

  }

  return mul;

}

// matrices

/*

  @param m: a matrix
  @param m2: a matrix
  @returns: the sum of M and M2


*/

int **add_matrices(int m[2][4], int m2[2][4], int size) {

  int **add;

  add = malloc(sizeof(int*) * size);

  for (int i = 0; i < size; i++) {

    add[i] = malloc(sizeof(int*) * size);

  }

  for (int i = 0; i < 2; i++) {

    for (int j = 0; j < size; j++) {

      add[i][j] = m[i][j] + m2[i][j];

    }

  }
  return add;



}

/*

  @param m: a matrix
  @param m2: a matrix
  @returns: the subtraction of M and M2


*/
int **sub_matrices(int m[2][4], int m2[2][4], int size) {

  int **sub;

  sub = malloc(sizeof(int*) * size);
  for (int i = 0; i < size; i++) {

    sub[i] = malloc(sizeof(int*) * size);

  }

  for (int i = 0; i < 2; i++) {

    for (int j = 0; j < size; j++) {

      sub[i][j] = m[i][j] + m2[i][j];

    }

  }
  return sub;

}

/*

  @param m: a matrix
  @param scalar: a scalar
  @returns: the multiplication of the scalar and the matrix

  the Scalar is multiplied by each member of the matrix.


*/

int **mul_scalar(int m[2][4], int scalar, int size) {

  int **mul;

  mul = malloc(sizeof(int*) * size);

  for (int i = 0; i < size; i++) {

    mul[i] = malloc(sizeof(int*) * size);

  }

  for (int i = 0; i < 2; i++) {

    for (int j = 0; j < size; j++) {

      mul[i][j] = m[i][j] * scalar;

    }

  }
  return mul;

}

// some examples :-)

int main () {

  int vec[] = {3,4,5,6};
  int size = 4;

  int *p = add_vectors(vec, vec, size);

  if (p) {

    for (int i = 0; i < size; i++) {
      printf("%d\n", p[i]);

    }
    free(p);

  }

  printf("\n");

  int m[2][4] = {{3,4,5,6}, {4,5,6,7}};
  int size2 = 4;

  int **p2 = add_matrices(m, m, size2);

  for (int i = 0; i < 2; i++) {
    for (int j = 0; j < size2; j++) {
      printf("%d ", p2[i][j]);
      printf("\t");

      }
    printf("\n");

    }

  // free memory
  for (int i = 0; i < size2; i++) {

    free(p2[i]);

  }
  free(p2);
  return 0;
  


}

