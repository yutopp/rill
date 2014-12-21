#include <cstdio>
#include <cstdlib>


extern "C"
{
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

} // extern
