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
    -Wall
)


#
# Boost Libraries
#
set( BOOST_ROOT "~\\boost_1_55_0" )
ADD_DEFINITIONS( -DBoost_USE_STATIC_LIBS=ON )
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
#
# ref. http://llvm.org/docs/CMake.html#embedding-llvm-in-your-project
find_package( LLVM )

if( NOT LLVM_FOUND )
  message( FATAL_ERROR "LLVM package can't be found. Set CMAKE_PREFIX_PATH variable to LLVM's installation prefix." )
else()
  message( STATUS "LLVM lib: ${LLVM_LIBRARY_DIRS}" )
endif()

include_directories( ${LLVM_INCLUDE_DIRS} )
link_directories( ${LLVM_LIBRARY_DIRS} )
add_definitions( ${LLVM_DEFINITIONS} )

llvm_map_components_to_libraries(
  REQ_LLVM_LIBRARIES
  native
  support
  bitreader
  instrumentation
  scalaropts
  ipo
  vectorize
  core
  object
  jit
  interpreter
  nativecodegen
  selectiondag
  asmparser
  asmprinter
  )

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


