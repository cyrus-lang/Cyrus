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
