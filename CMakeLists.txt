cmake_minimum_required(VERSION 3.2)
enable_testing()

project(rill)

set(RILLC_VERSION latest)

set(HOST_TRIPLE x86_64_unknown-linux-gnu)
set(TARGET_TRIPLES x86_64_unknown-linux-gnu wasm32-wasi)

include(./Rillc.cmake)

#
# Core Libraries
#
# These libraries will be installed in the following directory structure.
#
# - ${PREFIX}/lib/rill-lib/${TARGET_TRIPLE}/
#
set(TARGET_TRIPLE x86_64_unknown-linux-gnu)
set(INSTALL_RILL_LIB_LINK_PATH lib/rill-lib/${TARGET_TRIPLE}/lib)
set(INSTALL_RILL_LIB_SRC_PATH lib/rill-lib/src)

add_subdirectory(corelib)
add_subdirectory(stdlib)

# Integration test
find_program(BASH_PATH bash)
if(NOT BASH_PATH)
  message(FATAL_ERROR "Error: bash is not found")
endif()

add_test(
  NAME
    rillc_integration_test
  COMMAND
    ${BASH_PATH} ${CMAKE_CURRENT_SOURCE_DIR}/test/run.sh
)
foreach(EV
  "RILLC_COMPILER=../rillc/_build/default/bin/tool/compile/main.exe"
  "RILLC_CORELIB_SRCDIR=../corelib/src"
  "RILLC_CORELIB_LIBDIR=./corelib"
  "RILLC_STDLIB_SRCDIR=../stdlib/src"
  "RILLC_STDLIB_LIBDIR=./stdlib"
)
  set_property(TEST rillc_integration_test
    APPEND PROPERTY
    ENVIRONMENT ${EV}
  )
endforeach()
