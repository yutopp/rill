#include <iostream>
#include <cassert>
#include <cstring>

extern "C"
void* rill_core_malloc(std::int32_t const size)
{
    return ::malloc(size);
}

extern "C"
void rill_core_free(void* p)
{
    ::free(p);
}

// TODO: fix
extern "C"
void rill_core_memcpy(void* trg, void const* src, std::int32_t n)
{
    memcpy(trg, src, n);
}

extern "C"
void rill_core_memset(void* trg, char ch, std::int32_t n)
{
    memset(trg, ch, n);
}

// TODO: move to elsewhere
extern "C"
void _Rill_main();

extern "C"
int main() {
    _Rill_main();
    return 0;
}
