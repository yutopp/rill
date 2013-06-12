#ifndef RILL_CONFIG_MACROS_HPP
#define RILL_CONFIG_MACROS_HPP

#ifdef _MSC_VER

# define RILL_CXX11_FINAL
# define RILL_CXX11_OVERRIDE

#else

# define RILL_CXX11_FINAL final
# define RILL_CXX11_OVERRIDE override

#endif

#endif /*RILL_CONFIG_MACROS_HPP*/