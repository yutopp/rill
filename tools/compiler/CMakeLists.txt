file( GLOB_RECURSE files ${CMAKE_CURRENT_SOURCE_DIR}/src/*.cpp )

add_executable( rillc ${files} )
target_link_libraries(
  rillc
  rill
  ${REQ_LLVM_LIBRARIES}
  )

set_target_properties( rillc PROPERTIES LINKER_LANGUAGE CXX )

#
# Boost Libraries
#
find_package(
  Boost
  1.55.0
  COMPONENTS
    program_options
  REQUIRED
  )

include_directories( ${Boost_INCLUDE_DIRS} )
target_link_libraries( rillc ${Boost_LIBRARIES} )
install( TARGETS rillc DESTINATION bin )