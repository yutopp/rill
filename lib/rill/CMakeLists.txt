file( GLOB_RECURSE files ${CMAKE_CURRENT_SOURCE_DIR}/src/*.cpp )

add_library( rill ${files} )

target_link_libraries( rill ${REQ_LLVM_LIBRARIES} )
set_target_properties( rill PROPERTIES VERSION ${PACKAGE_VERSION} SOVERSION ${PACKAGE_VERSION} )

install( TARGETS rill DESTINATION lib )