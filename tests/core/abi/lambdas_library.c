#include <stdio.h>

typedef double (*CallbackFn)(double x, double y);

void call_callback_fn(CallbackFn cbk) {
  double a = 102.2;
  double b = 3.14;

  double result = cbk(a, b);

  printf("result = %f\n", result);
}