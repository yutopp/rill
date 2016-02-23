#include <iostream>
#include <cassert>

extern "C"
void rill_print(int const v)
{
    std::cout << "rill/std/print_int = " << v << std::endl;
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
