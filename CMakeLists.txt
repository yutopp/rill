cmake_minimum_required(VERSION 3.5.1)
enable_testing()

project(rill)

set(RILLC_VERSION latest)

set(HOST_TRIPLE x86_64-unknown-linux-gnu)
set(TARGET_TRIPLES x86_64-unknown-linux-gnu wasm32-wasi)

#
# Compiler
#
include(./Rillc.cmake)

#
# Core Libraries
#
# These libraries will be installed in the following directory structure.
#
# - ${PREFIX}/lib/rill-lib/src
# - ${PREFIX}/lib/rill-lib/${TARGET_TRIPLE}/lib
# - ${PREFIX}/lib/rill-lib/${TARGET_TRIPLE}/bin
#
set(TARGET_TRIPLE x86_64-unknown-linux-gnu)

set(RILL_LIB_DIR lib/rill-lib)
set(RILL_LIB_SRC_DIR ${RILL_LIB_DIR}/src)
set(RILL_LIB_TARGET_BIN_DIR ${RILL_LIB_DIR}/${TARGET_TRIPLE}/bin)
set(RILL_LIB_TARGET_LIB_DIR ${RILL_LIB_DIR}/${TARGET_TRIPLE}/lib)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/${RILL_LIB_TARGET_BIN_DIR}) # exe
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/${RILL_LIB_TARGET_LIB_DIR}) # lib (shared)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/${RILL_LIB_TARGET_LIB_DIR}) # lib (static)

add_subdirectory(corelib)
add_subdirectory(stdlib)

#
# Integration tests
#
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
  "RILL_TEST_COMPILER=${CMAKE_BINARY_DIR}/bin/rillc_compile"
  "RILL_TEST_CORELIB_SRCDIR=${CMAKE_BINARY_DIR}/${RILL_LIB_SRC_DIR}/core/src"
  "RILL_TEST_STDLIB_SRCDIR=${CMAKE_BINARY_DIR}/${RILL_LIB_SRC_DIR}/std/src"
  "RILL_TEST_TARGET_LIBDIR=${CMAKE_BINARY_DIR}/${RILL_LIB_TARGET_LIB_DIR}"
)
  set_property(TEST rillc_integration_test
    APPEND PROPERTY
    ENVIRONMENT ${EV}
  )
endforeach()
