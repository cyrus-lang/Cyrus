#include <stdarg.h>
#include <stdio.h>

// Basic Types

int add_ints(int a, int b) { return a + b; }

long add_longs(long a, long b) { return a + b; }

double add_doubles(double a, double b) { return a + b; }

float add_floats(float a, float b) { return a + b; }

// Many Arguments

long many_ints(long a, long b, long c, long d, long e, long f, long g, long h,
               long i, long j) {
  return a + b + c + d + e + f + g + h + i + j;
}

double many_floats(double a, double b, double c, double d, double e, double f,
                   double g, double h, double i, double j) {
  return a + b + c + d + e + f + g + h + i + j;
}

// Pointers

void write_int(int *ptr, int value) { *ptr = value; }

int read_int(int *ptr) { return *ptr; }

// Simple Struct

struct Pair {
  int a;
  int b;
};

struct Pair make_pair(int a, int b) {
  struct Pair p = {a, b};
  return p;
}

int sum_pair(struct Pair p) {
  printf("sum_pair %d %d\n", p.a, p.b);
  return p.a + p.b;
}

// Float Struct (SSE classification)

struct FloatPair {
  float a;
  float b;
};

float sum_float_pair(struct FloatPair p) { return p.a + p.b; }

// Mixed Struct (INTEGER + SSE)

struct MixedIntegerSSE {
  int a;
  double b;
};

double sum_mixed_integer_sse(struct MixedIntegerSSE m) { return m.a + m.b; }

// Large Struct (>16 bytes) (passed by memory)

struct LargeStruct {
  int a;
  int b;
  float c;
  double d;
  int *ptr;
};

double print_large_struct(struct LargeStruct v) {
  return v.a + v.b + v.c + v.d + *v.ptr;
}

// Struct Return

struct Triple {
  long a;
  long b;
  long c;
};

struct Triple make_triple(long a, long b, long c) {
  struct Triple t = {a, b, c};
  return t;
}

// Union

union UnionPayload {
  int int_value;
  float float_value;
  char *text_value;
};

void display_union_payload(union UnionPayload payload, int kind) {
  switch (kind) {
  case 1:
    printf("union_payload(int)=%d\n", payload.int_value);
    break;
  case 2:
    printf("union_payload(text)=%s\n", payload.text_value);
    break;
  case 3:
    printf("union_payload(float)=%f\n", payload.float_value);
    break;
  default:
    printf("union_payload(invalid)\n");
    break;
  }
}

// Nested Struct

struct NestedStructInner {
  int x;
  int y;
};

struct NestedStructOuter {
  struct NestedStructInner inner;
  double z;
};

double nested_sum(struct NestedStructOuter o) {
  printf("nested_sum %d %d %f\n", o.inner.x, o.inner.y, o.z);

  return o.inner.x + o.inner.y + o.z;
}

// Arrays

int sum_array(int *arr, int n) {
  int s = 0;
  for (int i = 0; i < n; i++)
    s += arr[i];

  printf("sum_array=%d\n", s);
  return s;
}

// Function Pointer

typedef int (*callback_t)(int, int);

int apply_callback(callback_t fn, int a, int b) {
  printf("apply_callback\n");
  return fn(a, b);
}

// Enum

enum Color { RED = 1, GREEN = 2, BLUE = 3 };

int enum_test(enum Color c) {
  printf("enum_test=%d\n", c);
  return c * 10;
}

// Variadic

long sum_variadic(int count, ...) {
  va_list args;
  va_start(args, count);

  long total = 0;

  for (int i = 0; i < count; i++)
    total += va_arg(args, long);

  va_end(args);

  printf("sum_variadic=%ld\n", total);
  return total;
}

// Const String

void print_text(const char *text) { printf("text: %s\n", text); }

// Globals

int global_counter = 42;

int get_global() { return global_counter; }

void set_global(int v) { global_counter = v; }

// Combined (INTEGER + SSE + STRUCT)

struct CombinedStruct {
  double x;
  int y;
};

double evil_abi_test(int a, double b, int c, double d,
                     struct CombinedStruct combined, float f, int g) {
  printf("evil_abi_test called\n");

  printf("a=%d\n", a);
  printf("b=%f\n", b);
  printf("c=%d\n", c);
  printf("d=%f\n", d);
  printf("w.x=%f\n", combined.x);
  printf("w.y=%d\n", combined.y);
  printf("f=%f\n", f);
  printf("g=%d\n", g);

  return a + b + c + d + combined.x + combined.y + f + g;
}