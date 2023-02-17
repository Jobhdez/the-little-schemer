#include <stdio.h>

int is_prime(int n) {
  if (n <= 1) return 0;
  int i;
  for (int i = 2; i < n; i = i + 1) {
    if (n % i == 0) {
      return 0;
    }
  }

  return 1;
}

void check(int n) {
  printf("%d -> %d\n", n, is_prime(n));
}
