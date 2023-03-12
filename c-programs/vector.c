#include <stdio.h>
#include <stdlib.h>

typedef struct vector {
  unsigned int length;
  int *data;
} vector;

vector *add_vectors(vector *v1, vector *v2) {
  vector *result;
  result->data = malloc(v1->length);
  result->length = v1->length;

  if (!result->data) {
    return NULL;
  }

  for (int i = 0; i < v1->length; i++) {
    result->data[i] = v1->data[i] + v2->data[i];
  }

  return result;
}

vector *sub_matrices(vector *v1, vector *v2) {
  vector *result;
  result->data = malloc(v1->length);
  result->length = v1->length;

  if (!result->data) {
    return NULL;
  }

  for (int i = 0; v1->length; i++) {
    result->data[i] = v1->data[i] - v2->data[i];
  }
  return result;
}

vector *vec_mul_by_scalar(vector *v1, int scalar) {
  vector *result;
  result->data = malloc(v1->length);
  result->length = v1->length;

  for (int i = 0; i < v1->length; i++) {
    
    result->data[i] = v1->data[i] * scalar;
  }
  return result;
}

int print_vector(vector *v1) {

  if (v1->data) {
    for (int i = 0; i < v1->length; i++) {
      printf("%d", v1->data[i]);
    }
  }
  free(v1->data);
}

int main(void) {
  vector *v1 = calloc(1, sizeof(*v1));
  v1->length = 4;
  v1->data = calloc(v1->length, sizeof(*v1->data));
  for (int i = 0; i < v1->length; i++) {
    v1->data[i] = i;
    }
  vector *result = add_vectors(v1, v1);
    //print_vector(result);
  for (int j = 0; j < v1->length; j++) {
    printf("%d", result->data[j]);
    }
}

    
    
