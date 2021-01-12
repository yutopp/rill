#
# Base compiler: rillc
# Rillc will be installed in the following directory structure.
#
# - ${PREFIX}/bin/rillc-*
#
include(ExternalProject)
ExternalProject_Add(
  rillc
  BUILD_COMMAND make build
  INSTALL_COMMAND make PREFIX=${CMAKE_BINARY_DIR} install
  SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/rillc"
  BUILD_IN_SOURCE On
  BUILD_ALWAYS On
  DOWNLOAD_COMMAND ""
  CONFIGURE_COMMAND ""
  LOG_BUILD On
  )

set(RILLC_COMMAND "${CMAKE_BINARY_DIR}/bin/rillc")

install(
  DIRECTORY ${CMAKE_BINARY_DIR}/bin/
  DESTINATION bin
  USE_SOURCE_PERMISSIONS
  )
