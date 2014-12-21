#include <cstdlib>

#include "runtime.hpp"


extern "C"
{
    extern int _R4main();    // defined at rill

    // currently, rill-runtime uses libc. so entry point is set to "main".
    // rill-runtime will support nostdlib in future.
    // int rill_main()

    int main()
    {
        int const value = _R4main();
        //put_string( "Hello Bunchou!\n" );

        std::exit( 0 );
    }
}
