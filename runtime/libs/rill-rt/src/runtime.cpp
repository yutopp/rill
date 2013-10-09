#if defined(RILL_RT_WINDOWS)

//
# include "runtime_windows-x64.cpp"

#elif defined(RILL_RT_LINUX)

//
# include "runtime-linux-x64.cpp"

#else

// TODO: add more runtime supports
# error "Your target environment is not supported..."

#endif
