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
    system
    filesystem
  REQUIRED
  )

include_directories( ${Boost_INCLUDE_DIRS} )


############################################################
############################################################
file( GLOB_RECURSE files ${CMAKE_CURRENT_SOURCE_DIR}/src/*.cpp )

# librill
add_library( rill ${files} )

target_link_libraries( rill ${Boost_LIBRARIES} ${REQ_LLVM_LIBRARIES} )
target_link_libraries( rill rill-rt-without-entry )
set_target_properties( rill PROPERTIES VERSION ${PACKAGE_VERSION} SOVERSION ${PACKAGE_VERSION} )

install( TARGETS rill DESTINATION lib )
