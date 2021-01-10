# temporal cmake file.

# c-srcs archives
# - ${PREFIX}/lib/rill-lib/${TARGET_TRIPLE}/lib/libcore-c.a
file(GLOB C_SOURCE_FILES
  src_ext/*.c
  )
foreach(target_triple IN ITEMS ${SUPPORTED_TRIPLES})
  if (${target_triple} IN_LIST RILL_TARGET_TRIPLES)
    rill_set_build_vars_for_target(${target_triple})

    add_library(core-c-${target_triple}
      STATIC ${C_SOURCE_FILES}
      )
    set_target_properties(core-c-${target_triple} PROPERTIES OUTPUT_NAME "core-c")

    install(
      TARGETS core-c-${target_triple}
      DESTINATION ${RILL_LIB_TARGET_LIB_DIR}
      )
  endif()
endforeach()

# rill-lib src
# - ${PREFIX}/lib/rill-lib/src/core/src/*
add_custom_target(core-src ALL
  COMMAND ${CMAKE_COMMAND} -E copy_directory
    ${CMAKE_CURRENT_SOURCE_DIR}/src
    ${CMAKE_BINARY_DIR}/${RILL_LIB_SRC_DIR}/core/src
    )

install(
  DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/src
  DESTINATION ${RILL_LIB_SRC_DIR}/core
  )
