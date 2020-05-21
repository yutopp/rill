# C sources
file(GLOB C_SOURCE_FILES
  src_ext/*.c
  )
add_library(std-c
  ${C_SOURCE_FILES}
  )
install(TARGETS std-c
  DESTINATION
    rill-lib/lib
  )

# Rill-lib
install(DIRECTORY src/std
  DESTINATION
    rill-lib/src
  )
