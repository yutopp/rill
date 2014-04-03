PROJECT( rill )
CMAKE_MINIMUM_REQUIRED( VERSION 2.8 )

set( RILL_VERSION_MAJOR 0 )
set( RILL_VERSION_MINOR 1 )

set( PACKAGE_VERSION "${RILL_VERSION_MAJOR}.${RILL_VERSION_MINOR}" )


#
include_directories( "${PROJECT_SOURCE_DIR}" )

#
add_definitions(
    -std=c++11
#    -Wall
)


#
# Boost Libraries
#
set( Boost_USE_STATIC_LIBS ON )
#add_definitions( -DBOOST_ERROR_CODE_HEADER_ONLY )
#add_definitions( -DBOOST_SYSTEM_NO_LIB )
find_package(
  Boost
  1.55.0
  COMPONENTS
    system
  REQUIRED
  )

include_directories( ${Boost_INCLUDE_DIRS} )

# the Boost's "target_link_libraries" is specified at tools/compiler/CMakeLists.txt


#
# LLVM
# aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa!!!!!!!!

if( NOT DEFINED LLVM_CONFIG_PATH )
  message( FATAL_ERROR "Please set LLVM_CONFIG_PATH to your 'llvm-config' file" )
endif()

if( NOT EXISTS ${LLVM_CONFIG_PATH} )
  message( FATAL_ERROR "The 'llvm-config' file(${LLVM_CONFIG_PATH}) was not found." )
endif()

execute_process(
  COMMAND
    ${LLVM_CONFIG_PATH} --version
  OUTPUT_VARIABLE
    LLVM_VERSION
  OUTPUT_STRIP_TRAILING_WHITESPACE
  )
execute_process(
  COMMAND
    ${LLVM_CONFIG_PATH} --includedir
  OUTPUT_VARIABLE
    LLVM_INCLUDE_DIRS
  OUTPUT_STRIP_TRAILING_WHITESPACE
  )
execute_process(
  COMMAND
    ${LLVM_CONFIG_PATH} --libdir
  OUTPUT_VARIABLE
    LLVM_LIBRARY_DIRS
  OUTPUT_STRIP_TRAILING_WHITESPACE
  )
set( # FIXME
  LLVM_DEFINITIONS
    -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MAC -D__STDC_LIMIT_MACROS
  )

message( STATUS "LLVM version: ${LLVM_VERSION}" )
message( STATUS "LLVM Include Dir: ${LLVM_INCLUDE_DIRS}" )
message( STATUS "LLVM Library Dir: ${LLVM_LIBRARY_DIRS}" )

include_directories( ${LLVM_INCLUDE_DIRS} )
link_directories( ${LLVM_LIBRARY_DIRS} )
add_definitions( ${LLVM_DEFINITIONS} )

#llvm_map_components_to_libraries(
#  REQ_LLVM_LIBRARIES
#  native
#  support
#  bitreader
#  instrumentation
#  scalaropts
#  ipo
#  vectorize
#  core
#  object
#  jit
#  interpreter
#  nativecodegen
#  selectiondag
#  asmparser
#  asmprinter
#  )

# the LLMV's "target_link_libraries" is specified at lib/rill/CMakeLists.txt


#
# Rill projects
#

# librill(lib)
add_subdirectory( ${PROJECT_SOURCE_DIR}/lib/rill )
install( DIRECTORY ${PROJECT_SOURCE_DIR}/rill DESTINATION include )

# tools(compiler)
add_subdirectory( ${PROJECT_SOURCE_DIR}/tools )

# librill-rt(rill runtime)
add_subdirectory( ${PROJECT_SOURCE_DIR}/rill-rt )
