#
# Base compiler: rillc
# Rillc will be installed in the following directory structure.
#
# - ${PREFIX}/bin/rillc-*
# - ${PREFIX}/doc/rillc/*
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

install(
  PROGRAMS
  ${CMAKE_BINARY_DIR}/bin/rillc_compile
  ${CMAKE_BINARY_DIR}/bin/rillc_build
  TYPE BIN
  )

file(GLOB DOCS_FILES
  ${CMAKE_BINARY_DIR}/doc/rillc/*
  )
install(
  FILES ${DOCS_FILES}
  TYPE DOC
  )
