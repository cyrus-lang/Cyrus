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
  return p.a + p.b;
}

// Float Struct (SSE)

struct FloatPair {
  float a;
  float b;
};

float sum_float_pair(struct FloatPair p) { return p.a + p.b; }

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