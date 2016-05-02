#include <iostream>
#include <cassert>
#include <cstring>

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
void rill_print_char(char const c)
{
    std::cout << c << std::endl;
}

extern "C"
int rill_strlen(char const* const s)
{
    return std::strlen(s);
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
void rill_print_addr(void const* const v)
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

extern "C"
char rill_read_char()
{
    char c;
    std::cin >> c;

    return c;
}


extern "C"
char* rill_read_string()
{
    std::string s;
    std::cin >> s;
    auto p = new char[s.size() + 1];
    memcpy(p, s.c_str(), s.size());
    p[s.size()] = '\0';

    return p;
}
