file( GLOB_RECURSE files ${CMAKE_CURRENT_SOURCE_DIR}/src/*.cpp )

add_executable( rillc ${files} )
target_link_libraries(
  rillc
  rill
  ${REQ_LLVM_LIBRARIES}
  )

set_target_properties( rillc PROPERTIES LINKER_LANGUAGE CXX )