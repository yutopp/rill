function(rill_include_target target_triple)
  # set(RILL_HOST_TRIPLE)
  set(RILL_TARGET_TRIPLE ${target_triple})

  rill_set_build_vars_for_target(${target_triple})

  add_subdirectory(target_${target_triple})
endfunction()

foreach(target_triple IN ITEMS ${SUPPORTED_TRIPLES})
  if (${target_triple} IN_LIST RILL_TARGET_TRIPLES)
    rill_include_target(${target_triple})
  endif()
endforeach()
