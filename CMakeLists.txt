PROJECT( rill )
CMAKE_MINIMUM_REQUIRED( VERSION 2.8 )

set( RILL_VERSION_MAJOR 0 )
set( RILL_VERSION_MINOR 1 )

set( PACKAGE_VERSION "${RILL_VERSION_MAJOR}.${RILL_VERSION_MINOR}" )


#
include_directories( "${PROJECT_SOURCE_DIR}" )

#
message( STATUS "CMAKE_CXX_COMPILER_ID => ${CMAKE_CXX_COMPILER_ID}" )

#
if( CMAKE_CXX_COMPILER_ID STREQUAL "Clang" )
  add_definitions(
    -std=c++11
    -DBOOST_PP_VARIADICS=1
#    -Wall
    )

elseif( CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
  add_definitions(
    -std=c++11
#    -Wall
    )

else()
  message( WARNING "This compile is not supported..." )

endif()


############################################################
############################################################
#
# Boost Libraries
#
set( Boost_USE_STATIC_LIBS ON )
find_package(
  Boost
  1.55.0
  COMPONENTS
    system
  REQUIRED
  )

include_directories( ${Boost_INCLUDE_DIRS} )

# the Boost's "target_link_libraries" is specified at tools/compiler/CMakeLists.txt


############################################################
############################################################
#
# LLVM
#  workaround code for LLVM that has no valid cmake files...
#  You must set 'LLVM_CONFIG_PATH' variable.
#    Ex. cmake ~~ -DLLVM_CONFIG_PATH=/usr/bin/llvm-config-3.4
if( NOT DEFINED LLVM_CONFIG_PATH )
  message( FATAL_ERROR "Please set LLVM_CONFIG_PATH to your 'llvm-config' file" )
endif()

if( NOT EXISTS ${LLVM_CONFIG_PATH} )
  message( FATAL_ERROR "The 'llvm-config' file(${LLVM_CONFIG_PATH}) was not found." )
endif()

# collect llvm envs
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

#
include_directories( ${LLVM_INCLUDE_DIRS} )
link_directories( ${LLVM_LIBRARY_DIRS} )
add_definitions( ${LLVM_DEFINITIONS} )

#
message( STATUS "LLVM version: ${LLVM_VERSION}" )
message( STATUS "LLVM Include Dir: ${LLVM_INCLUDE_DIRS}" )
message( STATUS "LLVM Library Dir: ${LLVM_LIBRARY_DIRS}" )

# collect llvm library object names
execute_process(
  COMMAND
    ${LLVM_CONFIG_PATH} --libnames
  OUTPUT_VARIABLE
    LLVM_LIBRARY_NAMES_STRING
  OUTPUT_STRIP_TRAILING_WHITESPACE
  )
# process => Ex.
#   from string $LLVM_LIBRARY_NAMES_STRING "libLLVMInstrumentation.a libLLVMIRReader.a libLLVMAsmParser.a"
#   to list $LLVM_LIBRARY_NAMES ["LLVMInstrumentation", "LLVMIRReader", "LLVMAsmParser"]
string( REGEX REPLACE " +" ";" LLVM_LIBRARY_NAMES ${LLVM_LIBRARY_NAMES_STRING} )
string( REGEX REPLACE "lib([^.]*)\\.a" "\\1" LLVM_LIBRARY_NAMES ${LLVM_LIBRARY_NAMES} )


# collect llvm components
execute_process(
  COMMAND
    ${LLVM_CONFIG_PATH} --components
  OUTPUT_VARIABLE
    LLVM_COMPONENTS_STRING
  OUTPUT_STRIP_TRAILING_WHITESPACE
  )
# process => Ex.
#   from string $LLVM_COMPONENTS_STRING " aarch64 aarch64asmparser"
#   to list $LLVM_COMPONENTS ["aarch64", "aarch64asmparser"]
string( REGEX MATCHALL "([^ ]+)" LLVM_COMPONENTS ${LLVM_COMPONENTS_STRING} )


# Rill requires these LLVM components
set( LLVM_COMPONENTS_RILL_REQUIRES
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


# check that specified packages are valid
foreach( c ${LLVM_COMPONENTS_RILL_REQUIRES} )
  list( FIND LLVM_COMPONENTS ${c} is_supported )
  if ( is_supported EQUAL -1 )
    message( FATAL_ERROR "A component '${c}' is not supported by LLVM." )
  endif()
endforeach(c)


# collect library names of components
execute_process(
  COMMAND
    ${LLVM_CONFIG_PATH} --libs ${LLVM_COMPONENTS_RILL_REQUIRES}
  OUTPUT_VARIABLE
    LLVM_LIBS_STRING
  OUTPUT_STRIP_TRAILING_WHITESPACE
  )
# process => Ex.
#   from string $LLVM_LIBS_STRING "-lLLVMAsmParser -lLLVMInterpreter -lLLVMJIT"
#   to list $LLVM_LIBS ["-lLLVMAsmParser", "-lLLVMInterpreter", "-lLLVMJIT"]
string( REGEX MATCHALL "-l([^ ]+)" LLVM_LIBS ${LLVM_LIBS_STRING} )


# collect system library names
execute_process(
  COMMAND
    ${LLVM_CONFIG_PATH} --ldflags
  OUTPUT_VARIABLE
    LLVM_SYS_LIBS_STRING
  OUTPUT_STRIP_TRAILING_WHITESPACE
  )
# process => Ex.
#   from string $LLVM_SYS_LIBS_STRING "-L/usr/lib/llvm-3.4/lib  -lz -lpthread -lffi -ltinfo -lrt -ldl -lm"
#   to list $LLVM_SYS_LIBS ["-lz", "-lpthread", "-lffi", "-ltinfo", "-lrt", "-ldl", "-lm"]
string( REGEX MATCHALL "-l([^ ]+)" LLVM_SYS_LIBS ${LLVM_SYS_LIBS_STRING} )


# insert all names of required LLVM(and system) libraries into $REQ_LLVM_LIBRARIES
set( REQ_LLVM_LIBRARIES )
foreach( l ${LLVM_LIBS} ${LLVM_SYS_LIBS} )
  # make "Hoge" from "-lHoge"
  string( REGEX REPLACE "-l([^ ]+)" "\\1" lib_name ${l} )
  list( APPEND REQ_LLVM_LIBRARIES ${lib_name} )
endforeach(l)

# the LLMV's "target_link_libraries" is specified at lib/rill/CMakeLists.txt


############################################################
############################################################
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
