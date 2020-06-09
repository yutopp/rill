#include <stdio.h>
#include <inttypes.h>

void c_println(char const* s) {
  printf("%s\n", s);
}

void c_print_int_ln(int32_t v) {
  printf("%d\n", v);
}

// TODO: fix
extern void test();

int main() {
  test();
}
