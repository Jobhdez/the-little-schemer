#include <stdio.h>
#include <stdlib.h>

typedef struct vector {
  unsigned int length;
  int *data;
} vector;

vector *add_vectors(vector v1, vector v2) {
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

int print_vector(vector v1) {
  int *vec_data = v1->data;

  if (vec_data) {
    for (int i = 0; i < v1->length; i++) {
      printf("%d", vec_data[i]);
    }
  }
  free(vec_data);

  int main() {
    vector v1;
    v1->length = 4;
    v1->data = [1,2,3,4];

    
    
