cmake_minimum_required(VERSION 3.2)

project(rillc)

## rillc
set(RILLC_BIN_PATH ${CMAKE_CURRENT_SOURCE_DIR}/_build/default/bin/main.exe)
add_custom_target(rillc ALL
  DEPENDS
    ${RILLC_BIN_PATH}
)

add_custom_command(OUTPUT ${RILLC_BIN_PATH}
  WORKING_DIRECTORY
    ${CMAKE_CURRENT_SOURCE_DIR}
  COMMAND
    opam install . --deps-only --yes
  COMMAND
    dune build
  DEPENDS
    llvm-patched
  COMMENT
    "Building rillc"
  )

## ext_dirs
set(EXT_DIRS_PATH ${CMAKE_CURRENT_SOURCE_DIR}/_ext_dirs)
add_custom_target(ext_dirs ALL
  DEPENDS
    ${EXT_DIRS_PATH}
)
add_custom_command(OUTPUT ${EXT_DIRS_PATH}
  WORKING_DIRECTORY
    ${CMAKE_CURRENT_SOURCE_DIR}
  COMMAND
    mkdir _ext_dirs
  COMMENT
    "Creating _ext_dirs directory"
  )

set(LLVM_PKG llvm.10.0.0)

## llvm_src
set(EXT_LLVM_PATH ${EXT_DIRS_PATH}/${LLVM_PKG})
add_custom_target(llvm_src ALL
  DEPENDS
    ${EXT_LLVM_PATH}
  )
add_custom_command(OUTPUT ${EXT_LLVM_PATH}
  WORKING_DIRECTORY
    ${EXT_DIRS_PATH}
  COMMAND
    opam source ${LLVM_PKG} --strict -y
  DEPENDS
    ${EXT_DIRS_PATH}
  COMMENT
    "Getting a source code of llvm for OCaml"
  )

## llvm-patched
set(LLVM_PATCH_DIR ${CMAKE_CURRENT_SOURCE_DIR}/patches/${LLVM_PKG})
add_custom_target(llvm-patched ALL
  WORKING_DIRECTORY
    ${EXT_LLVM_PATH}
  COMMAND
    ${LLVM_PATCH_DIR}/apply_to.sh ${LLVM_PATCH_DIR}
  COMMAND
    opam install . --deps-only --yes
  COMMAND
    opam pin add ${LLVM_PKG}-rillc . --kind=path --yes
  DEPENDS
    llvm_src
  COMMENT
    "Building patched llvm for OCaml"
  )
