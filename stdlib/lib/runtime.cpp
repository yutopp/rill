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
    std::cout << v << std::endl;
}

extern "C"
void rill_assert(bool const b)
{
    assert( b );
}
