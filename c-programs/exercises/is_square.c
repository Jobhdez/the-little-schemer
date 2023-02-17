#include <stdio.h>
#include <math.h>

int is_square(int n) {
  double sq = sqrt(n);
  int sq_int = (int)sq;
  if (sq_int * sq_int == n) {
    return 1;
  }
  else {
    return 0;
  }
}

void check(int n) {
  printf("%d -> %d\n", n, is_square(n));
}

int main(void) {
  int i;
  for(i = 1; i < 11; i = i + 1) {
    check(i);
  }
}
    
