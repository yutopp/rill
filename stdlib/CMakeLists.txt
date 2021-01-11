# temporal cmake file.

# c-srcs archives
# - ${PREFIX}/lib/rill-lib/${TARGET_TRIPLE}/lib/libstd-c.a
file(GLOB C_SOURCE_FILES
  src_ext/*.c
  )
function(rill_std_c_target target_triple)
  rill_c_library_target(std-c ${target_triple} "${C_SOURCE_FILES}")
endfunction()

foreach(target_triple IN ITEMS ${SUPPORTED_TRIPLES})
  if (${target_triple} IN_LIST RILL_TARGET_TRIPLES)
    rill_std_c_target(${target_triple})
  endif()
endforeach()

# rill-lib src
# - ${PREFIX}/lib/rill-lib/src/std/src/*
add_custom_target(std-src ALL
  COMMAND ${CMAKE_COMMAND} -E copy_directory
    ${CMAKE_CURRENT_SOURCE_DIR}/src
    ${CMAKE_BINARY_DIR}/${RILL_LIB_SRC_DIR}/std/src
    )

install(
  DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/src
  DESTINATION ${RILL_LIB_SRC_DIR}/std
  )
