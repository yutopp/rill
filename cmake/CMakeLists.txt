macro(rill_target_spec_json)
  rill_create_lib_target_sysroot(${RILL_TARGET_TRIPLE})

  add_custom_target(rill_target_spec_json-${RILL_TARGET_TRIPLE} ALL
    COMMAND ${CMAKE_COMMAND} -E copy
      ${CMAKE_CURRENT_SOURCE_DIR}/target-spec.json
      ${CMAKE_BINARY_DIR}/${RILL_LIB_TARGET_SYSROOT}/
    DEPENDS
      ${CMAKE_BINARY_DIR}/${RILL_LIB_TARGET_SYSROOT}/
      )
endmacro()

function(rill_c_library_target target_base_name target_triple c_source_files)
  rill_set_build_vars_for_target(${target_triple})

  set(target_name ${target_base_name}-${target_triple})

  set(lib_prefix "${CMAKE_BINARY_DIR}/${RILL_LIB_TARGET_LIB_DIR}")
  set(lib "${lib_prefix}/lib${target_base_name}.a")

  # TODO: build c files by rill-toolchains

  set(output_dir "${CMAKE_CURRENT_BINARY_DIR}/${target_name}.custom.dir")
  add_custom_command(OUTPUT ${output_dir}
    COMMAND ${CMAKE_COMMAND} -E make_directory ${output_dir})

  set(output_objs)
  foreach(c_source_file IN ITEMS ${c_source_files})
    get_filename_component(object_name ${c_source_file} NAME)
    set(output_obj ${output_dir}/${object_name}.o)

    add_custom_command(
      OUTPUT ${output_obj}
      COMMAND ${RILLC_COMMAND} cc -c ${c_source_file} --target=${target_triple} -o ${output_obj}
      DEPENDS ${output_dir}
      )
    list(APPEND output_objs ${output_obj})
  endforeach()

  add_custom_command(
    OUTPUT ${lib}
    COMMAND ${RILLC_COMMAND} ar --target=${target_triple} ${lib} ${output_objs}
    DEPENDS ${output_objs}
    )
  add_custom_target(${target_name} ALL DEPENDS ${lib})

  install(
    FILES ${lib}
    DESTINATION ${RILL_LIB_TARGET_LIB_DIR}
    )
endfunction()

# TODO: make it optional
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
