#include <iostream>

#include <stark/parser.hpp>

int main()
{
    auto const v = parse( "1*2 ;");
    for( auto const& s : v.product ) {
        s->eval();
    }

    {char c; std::cin >> c;}
}