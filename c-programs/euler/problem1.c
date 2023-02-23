#include <stdio.h>

int get_sum(int n) {
  int i;
  int sum = 0;
  for (i = 1; i < n; i++) {
    if (i % 3 == 0 || i % 5 == 0) {
      sum = sum + i;
    }
  }
  return sum;
}

int main() {
  int sum;
  sum = get_sum(10);
  printf("the sum is %d\n", sum);
}
