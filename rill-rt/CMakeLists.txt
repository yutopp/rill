# source of librill-rt
set( src ${CMAKE_CURRENT_SOURCE_DIR}/lib )

#
add_library(
  rill-rt
  ${src}/runtime.cpp
  )

# TODO: FIXME
if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "Clang")
  # using Clang
  message( STATUS "Using CLang" )

elseif ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "GNU")
  # using GCC
  message( STATUS "Using GNU" )

  # TODO: FIXME
  add_definitions(
#    -nodefaultlibs
    -fno-exceptions
#    -fno-stack-protector
    )

elseif ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "MSVC")
  # using Visual Studio C++
  message( STATUS "Using MSVC" )

else()
  message( ERROR "..." )
endif()



# TODO: ...
add_definitions(
  # -DRILL_RT_WINDOWS
  -DRILL_RT_LINUX
  )

# librill-rt
set_target_properties( rill-rt PROPERTIES LINKER_LANGUAGE CXX )
install( TARGETS rill-rt DESTINATION lib )

# core library for rilllang
install( DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/src/ DESTINATION lib/rill-rt )
