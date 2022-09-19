#include <stdio.h>
#include <stdlib.h>

// vectors

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

int **add_matrices(int m[2][4], int m2[2][4], int size) {

  int **add;

  add = malloc(sizeof(int*) * size);

  for (int i = 0; i < size; i++) {

    add[i] = malloc(sizeof(int*) * size);

  }

  for (int i = 0; i < size; i++) {

    for (int j = 0; j < size; j++) {

      add[i][j] = m[i][j] + m2[i][j];

    }

  }
  return add;



}

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

