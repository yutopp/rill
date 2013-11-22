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
)


#
# Boost Libraries
#
set( BOOST_ROOT "~\\boost_1_55_0" )
ADD_DEFINITIONS( -DBoost_USE_STATIC_LIBS=ON )
find_package(
  Boost
  1.55.0
  # COMPONENTS
  #                 spirit
  # REQUIRED
  )

include_directories( ${Boost_INCLUDE_DIRS} )

# target_link_libraries(<target_name> ${Boost_LIBRARIES})

#
# LLVM
#
# A convenience variable:
set(LLVM_ROOT "" CACHE PATH "Root of LLVM install.")

# A bit of a sanity check:
if( NOT EXISTS ${LLVM_ROOT}/include/llvm )
message(FATAL_ERROR "LLVM_ROOT (${LLVM_ROOT}) is not a valid LLVM install")
endif()

# We incorporate the CMake features provided by LLVM:
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${LLVM_ROOT}/share/llvm/cmake")
include(LLVMConfig)

# Now set the header and library paths:
include_directories( ${LLVM_INCLUDE_DIRS} )
link_directories( ${LLVM_LIBRARY_DIRS} )
add_definitions( ${LLVM_DEFINITIONS} )

# Let's suppose we want to build a JIT compiler with support for
# binary code (no interpreter):
# llvm_map_components_to_libraries(REQ_LLVM_LIBRARIES jit native)

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
# llvm_map_components_to_libraries( REQ_LLVM_LIBRARIES all )

add_definitions(
  -D__STDC_LIMIT_MACROS
  -D__STDC_CONSTANT_MACROS
)


# target_link_libraries(mycompiler ${REQ_LLVM_LIBRARIES})




# librill
add_subdirectory( ${PROJECT_SOURCE_DIR}/libs/rill )

add_subdirectory( ${PROJECT_SOURCE_DIR}/tools )

add_subdirectory( ${PROJECT_SOURCE_DIR}/runtime )


