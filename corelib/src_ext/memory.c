#include <string.h>
#include <stdlib.h>
#include <stdint.h>

void* rill_core_malloc(int32_t const size)
{
    return malloc(size);
}

void rill_core_free(void* p)
{
    free(p);
}

void rill_core_memcpy(void* trg, void const* src, uint32_t n)
{
    memcpy(trg, src, n);
}

void rill_core_memset(void* trg, char ch, uint32_t n)
{
    memset(trg, ch, n);
}
