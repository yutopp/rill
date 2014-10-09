############################################################
############################################################
#
# enable testing
#
enable_testing()


############################################################
############################################################
#
# Boost Libraries
#
set( Boost_USE_STATIC_LIBS ON )
find_package(
  Boost
  1.56.0
  COMPONENTS
    unit_test_framework
  REQUIRED
  )

include_directories( ${Boost_INCLUDE_DIRS} )


############################################################
############################################################
#
#
#
if( POLICY CMP0037 )
  cmake_policy( SET CMP0037 OLD )
endif()

add_custom_target( test )

#
macro( run_test test_target_name files libs )
  #
  add_executable( ${test_target_name} ${files} )

  #
  target_link_libraries( ${test_target_name} ${Boost_LIBRARIES} ${libs} )

  #
  add_custom_target( ${test_target_name}_runtest
                     COMMAND ${test_target_name}
                     DEPENDS ${test_target_name}
                     WORKING_DIRECTORY "${CMAKE_BINARY_DIR}"
                     )

  #
  add_dependencies( test ${test_target_name}_runtest )
endmacro()


############################################################
############################################################
#
# Add test suites
#
run_test( parser "syntax/syntax_test.cpp" "rill" ${REQ_LLVM_LIBRARIES} )
run_test( ast "ast/ast_test.cpp" "rill" ${REQ_LLVM_LIBRARIES} )
#run_test( tst "main.cc" "rill" ${REQ_LLVM_LIBRARIES} )
