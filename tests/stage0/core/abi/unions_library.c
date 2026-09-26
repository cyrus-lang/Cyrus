#include <stdio.h>

union UnionPayload {
  int int_value;
  float float_value;
  char *text_value;
};

void display_union_payload(union UnionPayload payload, int kind) {
  switch (kind) {
  case 1:
    printf("union_payload(int)=%d ", payload.int_value);
    break;
  case 2:
    printf("union_payload(text)=%s ", payload.text_value);
    break;
  case 3:
    printf("union_payload(float)=%f ", payload.float_value);
    break;
  default:
    printf("union_payload(invalid) ");
    break;
  }
}

typedef struct WithNestedUnion {
  int tag;
  union {
    int i;
    float f;
  } data;
} WithNestedUnion;

double nested_union(WithNestedUnion v) {
  if (v.tag == 0)
    return (double)v.data.i;

  return (double)v.data.f;
}

typedef union WithNestedStruct {
  int id;
  struct {
    double a;
    double b;
  } coords;
} WithNestedStruct;

double nested_struct_inside_union(WithNestedStruct u, int use_struct) {
  if (use_struct)
    return u.coords.a + u.coords.b;
    
  return (double)u.id;
}