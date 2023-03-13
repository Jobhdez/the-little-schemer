#include <stdio.h>
#include <stdlib.h>

typedef struct vector {
  int length;
  int *data;
} vector;

typedef struct poly {
  int degree;
  int *coefficients;
} poly;

poly *add_polys(poly *p1, poly *p2) {
  poly *p3;
  int size = p1->degree + 1;
  
  p3->coefficients = malloc(size);
  p3->degree = p1->degree;
  

  for (int i = 0; i < size; i++) {
    p3->coefficients[i] = p1->coefficients[i] + p2->coefficients[i];
    }

  return p3;
}
      
int eval_poly(poly *p1, int x) {
  int sum = 0;
  int inter;
  for (int i = 0; i < p1->degree+1; i++) {
    inter = p1->coefficients[i] * x;
    sum = sum + inter;
  }
  return sum;
}

  
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

vector *sub_vectors(vector *v1, vector *v2) {
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
vector *element_wise_product(vector *v1, vector *v2) {
  vector *element_wise;
  element_wise->data = malloc(v1->length);
  element_wise->length = v1->length;
  
  for (int i = 0; i < v1->length; i++) {
    element_wise->data[i] = v1->data[i] * v2->data[i];
  }

  return element_wise;
}
int dot_product(vector *v1) {


  int sum = 0;
  int inter;
  for (int j = 0; j < v1->length; j++) {
    inter = v1->data[j];
    sum = sum + inter;
  }

  return sum;
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

  printf("\n");
  printf("---------polynomials-------\n");

  poly *p1 = calloc(1, sizeof(*p1));
  p1->degree = 3;
  int size = p1->degree + 1;
  p1->coefficients = calloc(size, sizeof(*p1->coefficients));

  for (int k = 0; k < size; k++) {
    p1->coefficients[k] = k;
  }

  poly *result_poly = add_polys(p1, p1);
  int deg = result_poly->degree;
  
  for (int i = 0; i < result_poly->degree + 1; i++) {
    printf("%dx^%d+", result_poly->coefficients[i], deg);
    deg = deg - 1;

}
  printf("\n");
  printf("---eval---\n");
  printf("eval: %d\n", eval_poly(p1, 2));
  
  //printf("\n");
  printf("----dot product----");
  vector *element_wise_prod = element_wise_product(v1, v1);
  for (int i = 0; i<element_wise_prod->length; i++) {
    printf("%d", element_wise_prod->data[i]);
  }
    //printf("dotp: %d\n", dot_product(element_wise_prod));
  
}

    
    
