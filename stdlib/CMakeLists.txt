# temporal cmake file.

# c-srcs archives
# - ${PREFIX}/lib/rill-lib/${TARGET_TRIPLE}/lib/libstd-c.a
file(GLOB C_SOURCE_FILES
  src_ext/*.c
  )
add_library(std-c
  STATIC ${C_SOURCE_FILES}
  )

install(
  TARGETS std-c
  DESTINATION ${RILL_LIB_TARGET_LIB_DIR}
  )

# rill-lib src
# - ${PREFIX}/lib/rill-lib/src/std/src/*
FILE(
  COPY ${CMAKE_CURRENT_SOURCE_DIR}/src
  DESTINATION ${RILL_LIB_SRC_DIR}/std
  )

install(
  DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/src
  DESTINATION ${RILL_LIB_SRC_DIR}/std
  )
