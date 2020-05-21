# C sources
file(GLOB C_SOURCE_FILES
  src_ext/*.c
  )
add_library(core-c
  ${C_SOURCE_FILES}
  )
install(TARGETS core-c
  DESTINATION
    rill-lib/lib
  )

# Rill-lib
install(DIRECTORY src/core
  DESTINATION
    rill-lib/src
  )
