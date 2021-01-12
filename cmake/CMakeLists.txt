function(rill_target_spec_json target_triple)
  rill_set_build_vars_for_target(${target_triple})

  set(output ${CMAKE_BINARY_DIR}/${RILL_LIB_TARGET_SYSROOT}/target-spec.json)
  add_custom_command(
    OUTPUT ${output}
    COMMAND ${CMAKE_COMMAND} -E copy
      ${CMAKE_CURRENT_SOURCE_DIR}/target-spec.json
      ${output}
      )
  add_custom_target(rill_target_spec_json-${target_triple} ALL
    DEPENDS ${output})

  install(
    FILES ${output}
    DESTINATION ${RILL_LIB_TARGET_SYSROOT}
    )
endfunction()

function(rill_c_library_target target_base_name target_triple c_source_files)
  rill_set_build_vars_for_target(${target_triple})

  set(target_name ${target_base_name}-${target_triple})

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

  set(output_lib ${output_dir}/lib${target_base_name}.a)
  add_custom_command(
    OUTPUT ${output_lib}
    COMMAND ${RILLC_COMMAND} ar --target=${target_triple} ${output_lib} ${output_objs}
    DEPENDS ${output_objs}
    )

  set(lib_prefix "${CMAKE_BINARY_DIR}/${RILL_LIB_TARGET_LIB_DIR}")
  set(lib "${lib_prefix}/lib${target_base_name}.a")

  add_custom_command(
    OUTPUT ${lib}
    COMMAND ${CMAKE_COMMAND} -E copy ${output_lib} ${lib}
    DEPENDS ${output_lib}
    )
  add_custom_target(${target_name} ALL DEPENDS ${lib})

  install(
    FILES ${lib}
    DESTINATION ${RILL_LIB_TARGET_LIB_DIR}
    )
endfunction()

foreach(target_triple IN ITEMS ${SUPPORTED_TRIPLES})
  if (${target_triple} IN_LIST RILL_TARGET_TRIPLES)
    rill_create_lib_target_sysroot(${target_triple})
  endif()
endforeach()

add_subdirectory(host_${RILL_HOST_TRIPLE})
