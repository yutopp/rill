#include <Windows.h>


extern "C"
{
    extern int main0_();    // defined at rill
}

namespace detail
{
    class system
    {
    public:
        system()
        {
            hStdOutput = CreateFile(TEXT("CONOUT$"), GENERIC_WRITE, FILE_SHARE_WRITE, NULL, OPEN_EXISTING, 0, 0);
            hStdInput = CreateFile(TEXT("CONIN$"), GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, 0, 0);
        }

        void test( char const* const str )
        {
            WriteFile( hStdOutput, str, lstrlenA( str ), &dwWriteByte, NULL );
        }

    private:
        HANDLE hStdOutput, hStdInput;
        DWORD  dwWriteByte, dwReadByte;
    };
}



extern "C"
{
    void put_string( char const* const str )
    {
        detail::system().test( str );
    }

    int pow( int a, int b )
    {
        int v = 1;
        for( int i=0; i<b; ++i )
            v *= a;
        return v;
    }

    void put_string2( int const num )
    {
        char buffer[1024];

        for( int i=0; i<1024; ++i )
            buffer[i] = '\0';

        int deg = num, degg = 0;
        while( deg / 10 > 0 ) {
            ++degg;
            deg /= 10;
        }

        int p = num;
        for( int i=degg; i>=0; --i ) {
            char c = '0' + ( p / pow( 10, i ) );
            buffer[degg-i] = c;

            p = ( p % pow( 10, i ) );
        }

        detail::system().test( buffer );
    }

    int rill_main()
    {
        AllocConsole();

        int const value = main0_();
        //put_string( "Hello Bunchou!\n" );

        FreeConsole();
        return 9;
    }

} // extern
