macro(rill_target_spec_json)
  add_custom_target(target_spec_json-${RILL_TARGET_TRIPLE} ALL
    COMMAND ${CMAKE_COMMAND} -E copy
      ${CMAKE_CURRENT_SOURCE_DIR}/target-spec.json
      ${CMAKE_BINARY_DIR}/${RILL_LIB_TARGET_SYSROOT}/
      )
endmacro()

set(RILL_TARGET_WASM32-WASI_WASI_SDK_VERSION 12)
set(RILL_TARGET_WASM32-WASI_WASI_SDK_URL
  https://github.com/WebAssembly/wasi-sdk/releases/download/wasi-sdk-${RILL_TARGET_WASM32-WASI_WASI_SDK_VERSION}/wasi-sdk-${RILL_TARGET_WASM32-WASI_WASI_SDK_VERSION}.0-linux.tar.gz
  )
set(RILL_TARGET_WASM32-WASI_WASI_SDK_URL_HASH
  fa47694d75bf6fce98fbe7927193298b25ee03dade27fca453d7dd530378973a
  )

function(wasi)
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
endfunction()
wasi()

foreach(target_triple IN ITEMS ${SUPPORTED_TRIPLES})
  if (${target_triple} IN_LIST RILL_TARGET_TRIPLES)
    rill_create_lib_target_sysroot(${target_triple})
  endif()
endforeach()

add_subdirectory(host_${RILL_HOST_TRIPLE})
