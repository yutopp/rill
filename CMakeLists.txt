cmake_minimum_required(VERSION 3.5.1)
enable_testing()

project(rill)

set(RILLC_VERSION latest)

set(SUPPORTED_TRIPLES
  x86_64-unknown-linux-gnu
  wasm32-wasi
  )

set(RILL_TARGET_TRIPLES ${SUPPORTED_TRIPLES} CACHE STRING "Target triples")
set(RILL_HOST_TRIPLE x86_64-unknown-linux-gnu CACHE STRING "Host triple")

# Rillc spec files/libraries will be installed in the following directory structure.
#
# - ${PREFIX}/lib/rill-lib/src
# - ${PREFIX}/lib/rill-lib/${target_triple}/target-spec.json
# - ${PREFIX}/lib/rill-lib/${target_triple}/toolchains
# - ${PREFIX}/lib/rill-lib/${target_triple}/lib
# - ${PREFIX}/lib/rill-lib/${target_triple}/bin
#
set(RILL_LIB_DIR lib/rill-lib)
set(RILL_LIB_SRC_DIR ${RILL_LIB_DIR}/src)

macro(rill_set_build_vars_for_target target_triple)
  set(RILL_LIB_TARGET_SYSROOT ${RILL_LIB_DIR}/${target_triple})
  set(RILL_LIB_TARGET_TOOLCHAINS ${RILL_LIB_TARGET_SYSROOT}/toolchains)
  set(RILL_LIB_TARGET_BIN_DIR ${RILL_LIB_TARGET_SYSROOT}/bin)
  set(RILL_LIB_TARGET_LIB_DIR ${RILL_LIB_TARGET_SYSROOT}/lib)

  set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/${RILL_LIB_TARGET_BIN_DIR}) # exe
  set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/${RILL_LIB_TARGET_LIB_DIR}) # shared lib
  set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/${RILL_LIB_TARGET_LIB_DIR}) # static lib
endmacro()

function(rill_create_lib_target_sysroot target_triple)
  rill_set_build_vars_for_target(${target_triple})

  add_custom_command(OUTPUT ${RILL_LIB_TARGET_SYSROOT}
    COMMAND ${CMAKE_COMMAND} -E make_directory
      ${RILL_LIB_TARGET_SYSROOT}
    )
endfunction()

add_subdirectory(cmake)

#
# Compiler
#
include(cmake/Rillc.cmake)

#
# Core Libraries
#
add_subdirectory(corelib)
add_subdirectory(stdlib)

#
# Integration tests
# TODO: Run tests per targets (now only for host).
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
    "RILL_TEST_COMPILER=${RILLC_COMMAND}"
    "RILL_TEST_TARGET=${RILL_HOST_TRIPLE}"
    )
  set_property(TEST rillc_integration_test
    APPEND PROPERTY
    ENVIRONMENT ${EV}
  )
endforeach()
