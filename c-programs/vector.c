#include <stdio.h>
#include <stdlib.h>

/*
this is a just an experiment to try c out so I didnt write any comments :)

happy programming!

*/

/*
math structures

*/

typedef struct vector {
  int length;
  int *data;
} vector;

typedef struct poly {
  int degree;
  int *coefficients;
} poly;

typedef struct matrix {
  int rows;
  int columns;
  int **data;
} matrix;

/*
basic Polynomials

*/

poly *add_polys(poly *p1, poly *p2) {
  // @param p1: poly
  // @param p2: poly
  // returns: p3, the polynomial sum of poly p1 and poly p2
	
  // adds two polynomials
   poly *p3;
  int size = p1->degree + 1;
  
  p3->coefficients = malloc(size);
  p3->degree = p1->degree;
  

  for (int i = 0; i < size; i++) {
    p3->coefficients[i] = p1->coefficients[i] + p2->coefficients[i];
    }

  return p3;
}
poly *sub_polys(poly *p1, poly *p2) {
  poly *p3;
  int size = p1->degree + 1;

  p3->coefficients = malloc(size);
  p3->degree = p1->degree;

  for (int i = 0; i < size; i++) {
    p3->coefficients[i] = p1->coefficients[i] + p2->coefficients[i];
  }

  return p3;
}

poly *multiply_polys(poly *p1, poly *p2) {
  poly *p3;
  int p1_size = p1->degree + 1;
  int p2_size = p2->degree + 1;

  p3->coefficients = malloc(p1_size+p2_size-1);
  p3->degree = p1->degree + p2->degree;

  for (int i = 0; i < p1_size; i++) {
    for (int j=0; j<p2_size; j++) {
      p3->coefficients[i+j] += p1->coefficients[i] *  p2->coefficients[j];
    }
  }

  return p3;
}

poly *derivative(poly *p1) {
  poly *p2;
  p2->degree = p1->degree - 1;
  p2->coefficients = malloc(p1->degree);

  int deg = p1->degree;
  for (int i = 0; i < p1->degree + 1; i++) {
    p2->coefficients[i] = p1->coefficients[i] * deg;
    deg = deg - 1;
  }
  return p2;
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

void free_poly(poly *p) {
  free(p->coefficients);
  free(p);
}

/*
linear algebra (vectors)

 */
  
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
int dot_product(vector *inter_element_wise) {
  
  int sum = 0;
  int inter;
  for (int j = 0; j < inter_element_wise->length; j++) {
    inter = inter_element_wise->data[j];
    sum = sum + inter;
  }

  return sum;
}

/* basic matrix algebra. */
matrix *make_matrix(int rows, int columns) {
  matrix *m = calloc(1, sizeof(*m));
  m->rows = rows;
  m->columns = columns;
  m->data = calloc(m->rows, sizeof(*m->data)); // allocate memory for column array
  for (int i = 0; i < m->rows; i++) {
    m->data[i] = calloc(m->columns, sizeof(**m->data));
  }

  return m;
}

matrix *add_mat(matrix *m1, matrix *m2) {
  int rows = m1->rows;
  int columns = m1->columns;

  matrix *m3 = make_matrix(rows, columns);

  for (int i = 0; i < m3->rows; i++) {
    for (int j = 0; j < m3->columns; j++) {
      m3->data[i][j] = m1->data[i][j] + m2->data[i][j];
    }
  }
  return m3;
}

matrix *sub_mat(matrix *m1, matrix *m2) {
  int rows = m1->rows;
  int columns = m1->columns;

  matrix *m3 = make_matrix(rows, columns);

  for (int i = 0; i < m3->rows; i++) {
    for (int j = 0; j < m3->columns; j++) {
      m3->data[i][j] = m1->data[i][j] + m2->data[i][j];
    }
  }
  return m3;
}

matrix *mul_scalar(matrix *m1, int scalar) {
  int rows = m1->rows;
  int columns = m1->columns;

  matrix *m2 = make_matrix(rows, columns);

  int i, j;
  for (i = 0; i < rows; i++) {
    for (j = 0; j < columns; j++) {
      m2->data[i][j] = m1->data[i][j] * scalar;
    }
  }
  return m2;
}

// note: expects two square matrices
matrix *mul_sq_matrix(matrix *m1, matrix *m2) {
  int n = m1->rows;
  matrix *m3 = make_matrix(m1->rows, m1->columns);

  for (int i = 0; i < n; i++) {
    for (int j = 0; j < n; j++) {
      m3->data[i][j] = 0;
      for (int k = 0; k < n; k++) {
	m3->data[i][j] = m3->data[i][j] + m1->data[i][k] * m2->data[k][j];
	}
      }
    }
  return m3;
}

    
	
void print_matrix(matrix *m1) {
  for (int i = 0; i < m1->rows; i++) {
    for (int j = 0; j < m1->columns; j++) {
      printf("%d ", m1->data[i][j]);
      printf("\t");
    }
    printf("\n");
  }
}

void initialize_matrix(matrix *m1, int data) {
  for (int i = 0; i < m1->rows; i++) {
    for (int j = 0; j < m1->columns; j++) {
      m1->data[i][j] = data;
      data += 2;
    }
  }
}
  
		    
void free_vector(vector *v) {
  free(v->data);
  free(v);
}
int print_vector(vector *v1) {

  if (v1->data) {
    for (int i = 0; i < v1->length; i++) {
      printf("%d", v1->data[i]);
    }
  }
  free(v1->data);
}

// tests :)
//------------------------
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
  free(v1->data);
  free(v1);

  free(result->data);
  //free(result);
  
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
  free(result_poly->coefficients);
  //free(result_poly);
  printf("\n");
  printf("---eval---\n");
  printf("eval: %d\n", eval_poly(p1, 2));
  free(p1->coefficients);
  free(p1);
  //printf("\n");
  printf("----dot product----");
  vector *v2 = calloc(1, sizeof(*v2));
  v2->length = 4;
  v2->data = calloc(v2->length, sizeof(*v2->data));

  for (int i = 0; i < v2->length; i++) {
    v2->data[i] = i;
  }
  vector *element_wise_prod = element_wise_product(v2, v2);
  /*for (int i = 0; i<element_wise_prod->length; i++) {
    printf("%d", element_wise_prod->data[i]);
    }*/
  free(v2->data);
  free(v2);
  printf("\n");
  printf("dotp: %d\n", dot_product(element_wise_prod));
  printf("\n");

  free(element_wise_prod->data);

  int rows = 2;
  int columns = 3;
  matrix *m1 = make_matrix(rows, columns);
  int data = 1;
  for (int i = 0; i < m1->rows; i++) {
    for (int j = 0; j < m1->columns; j++) {
      m1->data[i][j] = data;
      data += 2;
    }
  }

  matrix *m2 = add_mat(m1, m1);
  for (int i = 0; i < m2->rows; i++) {
    for (int j = 0; j < m2->columns; j++) {
      printf("%d ", m2->data[i][j]);
      printf("\t");
    }
    printf("\n");
  }

  free(m1->data);
  free(m1);
  free(m2->data);
  int rows2 = 2;
  int columns2 = 3;
  matrix *m3 = make_matrix(rows2, columns2);
  int data2 = 1;
  for (int i = 0; i < m3->rows; i++) {
    for (int j = 0; j < m3->columns; j++) {
      m3->data[i][j] = data2;
      data2 += 2;
    }
  }
  int scalar = 3;
  matrix *m4 = mul_scalar(m3, scalar);
  printf("\n");
  printf("---print matrix * scalar-----");
  printf("\n");
  print_matrix(m4);

  free(m3->data);
  free(m3);
  free(m4->data);

  matrix *m5 = make_matrix(4,4);

  initialize_matrix(m5, 2);
  matrix *m6 = mul_sq_matrix(m5, m5);
  printf("\n");
  printf("----squared matrix multiplication.----\n");
  print_matrix(m6);

  free(m5->data);
  free(m5);
  free(m6->data);
  
  
}
