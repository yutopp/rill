#include <cstdio>
#include <cstdlib>


extern "C"
{
    extern int main();    // defined at rill

    void put_string( char const* const str )
    {
        std::printf( "%s", str );
    }

    void put_string2( int const num )
    {
        std::printf( "num => %d\n", num );
        return;
    }

    int rill_main()
    {
        int const value = main();
        //put_string( "Hello Bunchou!\n" );

        std::exit( 0 );
    }

} // extern
