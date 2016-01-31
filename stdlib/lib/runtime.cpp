#include <iostream>

extern "C"
void rill_print(int v)
{
    std::cout << "rill/std/print_int = " << v << std::endl;
}
