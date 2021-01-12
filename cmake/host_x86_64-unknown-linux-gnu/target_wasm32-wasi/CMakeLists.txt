rill_target_spec_json(${RILL_TARGET_TRIPLE})

# TODO: make it optional
set(RILL_TARGET_WASM32-WASI_WASI_SDK_VERSION 12)
set(RILL_TARGET_WASM32-WASI_WASI_SDK_URL
  https://github.com/WebAssembly/wasi-sdk/releases/download/wasi-sdk-${RILL_TARGET_WASM32-WASI_WASI_SDK_VERSION}/wasi-sdk-${RILL_TARGET_WASM32-WASI_WASI_SDK_VERSION}.0-linux.tar.gz
  )
set(RILL_TARGET_WASM32-WASI_WASI_SDK_URL_HASH
  fa47694d75bf6fce98fbe7927193298b25ee03dade27fca453d7dd530378973a
  )

function(install_wasi_sdk)
  rill_set_build_vars_for_target(wasm32-wasi)

  include(ExternalProject)
  ExternalProject_Add(
    wasi-sdk
    URL ${RILL_TARGET_WASM32-WASI_WASI_SDK_URL}
    URL_HASH SHA256=${RILL_TARGET_WASM32-WASI_WASI_SDK_URL_HASH}
    SOURCE_DIR ${CMAKE_BINARY_DIR}/${RILL_LIB_TARGET_TOOLCHAINS}
    CONFIGURE_COMMAND ""
    BUILD_COMMAND ""
    INSTALL_COMMAND ""
    )

  install(
    DIRECTORY ${CMAKE_BINARY_DIR}/${RILL_LIB_TARGET_TOOLCHAINS}/
    DESTINATION ${RILL_LIB_TARGET_TOOLCHAINS}
    USE_SOURCE_PERMISSIONS
    )
endfunction()

install_wasi_sdk()
