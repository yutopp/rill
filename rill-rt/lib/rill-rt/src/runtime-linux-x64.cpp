#include <cstdio>
#include <cstdlib>


extern "C"
{
    extern int _R4main();    // defined at rill

    void put_string( char const* const str )
    {
        std::printf( "%s", str );
    }

    void print_int32( int const num )
    {
        std::printf( "num => %d\n", num );
    }

    void print_bool( bool const b )
    {
        std::printf( "bool => %s\n", b ? "true" : "false" );
    }

    void print_bytes( char const* const bytes )
    {
        std::printf( "bytes => %s\n", bytes );
    }

    void print_float( float const f )
    {
        std::printf( "float => %f\n", f );
    }


    // currently, rill-runtime uses libc. so entry point is set to "main".
    // rill-runtime will support nostdlib in future.
    // int rill_main()

    int main()
    {
        int const value = _R4main();
        //put_string( "Hello Bunchou!\n" );

        std::exit( 0 );
    }

} // extern
