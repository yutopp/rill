# C sources
file(GLOB C_SOURCE_FILES
  src_ext/*.c
  )
add_library(core-c
  ${C_SOURCE_FILES}
  )
install(TARGETS core-c
  DESTINATION
    ${INSTALL_RILL_LIB_LINK_PATH}
  )

# Rill-lib
install(DIRECTORY src/core
  DESTINATION
    ${INSTALL_RILL_LIB_SRC_PATH}
  )
