#include <stdio.h>

typedef double (*CallbackFn)(int x, double y);

void call_callback_fn(CallbackFn cbk) {
  int a = 102;
  double b = 3.14;

  double result = cbk(a, b);

  printf("result = %f\n", result);
}