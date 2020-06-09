# C sources
file(GLOB C_SOURCE_FILES
  src_ext/*.c
  )
add_library(std-c
  ${C_SOURCE_FILES}
  )
install(TARGETS std-c
  DESTINATION
    ${INSTALL_RILL_LIB_LINK_PATH}
  )

# Rill-lib
install(DIRECTORY src/std
  DESTINATION
    ${INSTALL_RILL_LIB_SRC_PATH}
  )
