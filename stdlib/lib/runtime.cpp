#include <iostream>
#include <cassert>

extern "C"
void rill_print_int(int const v)
{
    std::cout << "rill/std/print_int = " << v << std::endl;
}

extern "C"
void rill_print_bool(bool const v)
{
    std::cout << "rill/std/print_bool = " << v << std::endl;
}

extern "C"
void rill_print_string(char const* const v)
{
    std::cout << "rill/std/print_string = " << v << std::endl;
}

extern "C"
void rill_assert(bool const b)
{
    assert( b );
}

extern "C"
void rill_assert_not(bool const b)
{
    assert( !b );
}
