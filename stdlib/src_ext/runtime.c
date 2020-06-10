#if 0
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>

void rill_print_int32(int32_t v)
{
    printf("%d", v);
    fflush(stdout);
}

void rill_print_uint32(uint32_t v)
{
    printf("%u", v);
    fflush(stdout);
}

void rill_print_bool(bool v)
{
    printf("%b", v);
    fflush(stdout);
}

void rill_print_char(char v)
{
    printf("%c", v);
    fflush(stdout);
}

void rill_print_string(char const* v)
{
    printf("%s", v);
    fflush(stdout);
}

void rill_println_string(char const* v)
{
    printf("%s\n", v);
    fflush(stdout);
}

void rill_print_addr(void const* v)
{
    printf("%p\n", v);
    fflush(stdout);
}

void rill_assert(bool b)
{
    assert( b );
}

int32_t rill_read_int32()
{
    int32_t i;
    scanf("%d", &i);
    return i;
}

char rill_read_char()
{
    char c;
    scanf("%c", &c);
    return c;
}
#endif
