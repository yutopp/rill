#include <iostream>
#include <cassert>

extern "C"
void rill_print_int(int const v)
{
    std::cout << v << std::endl;
}

extern "C"
void rill_print_bool(bool const v)
{
    std::cout << v << std::endl;
}

extern "C"
void rill_print_string(char const* const v)
{
    std::cout << v << std::flush;
}

extern "C"
void rill_println_string(char const* const v)
{
    std::cout << v << std::endl;
}

extern "C"
void rill_assert(bool const b)
{
    assert( b );
}

extern "C"
int rill_read_int32()
{
    int i;
    std::cin >> i;
    return i;
}
