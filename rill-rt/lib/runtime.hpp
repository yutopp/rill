#ifndef RILL_RT_RUNTIME_HPP
#define RILL_RT_RUNTIME_HPP

extern "C"
{
    extern void put_string( char const* const str );
    extern void print_int32( int const num );
    extern void print_bool( bool const b );
    extern void print_bytes( char const* const bytes );
    extern void print_float( float const f );
}

#endif /*RILL_RT_RUNTIME_HPP*/
