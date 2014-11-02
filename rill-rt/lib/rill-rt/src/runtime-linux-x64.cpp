#include <cstdio>
#include <cstdlib>


extern "C"
{
    extern int main();    // defined at rill

    void put_string( char const* const str )
    {
        std::printf( "%s", str );
    }

    void print_int32( int const num )
    {
        std::printf( "num => %d\n", num );
        return;
    }

    void print_bool( bool const b )
    {
        std::printf( "bool => %s\n", b ? "true" : "false" );
        return;
    }

    int rill_main()
    {
        int const value = main();
        //put_string( "Hello Bunchou!\n" );

        std::exit( 0 );
    }

} // extern
