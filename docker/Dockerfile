FROM ubuntu:16.04

LABEL maintainer yutopp

# Setup
RUN apt-get update -y && \
    apt-get install -y git wget curl unzip \
                       g++ aspcud  cmake ninja-build python m4 pkg-config libffi-dev

# https://llvm.org/svn/llvm-project/llvm/tags/RELEASE_500/final/CMakeLists.txt
ENV llvm_version 5.0.0

# Download LLVM 5.0.0
RUN wget http://www.llvm.org/releases/${llvm_version}/llvm-${llvm_version}.src.tar.xz -O - | tar -xJ
RUN cd llvm-${llvm_version}.src && \
    mkdir build && \
    cd build && \
    cmake -G 'Ninja' \
          -DCMAKE_INSTALL_PREFIX=/usr/local \
          -DCMAKE_BUILD_TYPE=Release \
          -DLLVM_TARGETS_TO_BUILD="X86" \
          -DLLVM_TARGETS_WITH_JIT="X86" \
          -DLLVM_EXPERIMENTAL_TARGETS_TO_BUILD="WebAssembly" \
          -DLLVM_ENABLE_TERMINFO=OFF \
          -DLLVM_ENABLE_ZLIB=OFF \
          -DLLVM_BUILD_TOOLS=OFF \
          -DLLVM_INCLUDE_UTILS=OFF \
          -DLLVM_BUILD_UTILS=OFF \
          -DLLVM_INCLUDE_EXAMPLES=OFF \
          -DLLVM_INCLUDE_TESTS=OFF \
          -DLLVM_INCLUDE_GO_TESTS=OFF \
          -DLLVM_INCLUDE_DOCS=OFF \
          -DLLVM_BUILD_DOCS=OFF \
          ..

# Build LLVM for static lib
RUN cd llvm-${llvm_version}.src/build && \
    ninja
RUN cd llvm-${llvm_version}.src/build && \
    ninja llvm-config

# Install LLVM
RUN cd llvm-${llvm_version}.src/build && \
    ninja install && \
    cp bin/llvm-config /usr/local/bin/. # adhoc

# Clean up LLVM src
RUN rm -rf llvm-${llvm_version}.src

# OCaml version
ENV ocaml_version 4.04.2

# Install OCaml and packages
RUN wget https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh -O - | sh -s /usr/local/bin
ENV OPAMKEEPBUILDDIR false
ENV OPAMBUILDDOC false
ENV OPAMDOWNLOADJOBS 1
RUN opam init -y -a --comp=${ocaml_version}
RUN opam install omake.0.10.2 menhir batteries ctypes-foreign stdint ocamlgraph
RUN opam install ounit bisect_ppx ocveralls
# BEGIN: adhoc for WebAssembly
RUN opam install llvm.${llvm_version} --deps-only
RUN mkdir opam_source && \
    cd opam_source && \
    opam source llvm.${llvm_version}
COPY patches /patches
RUN cd opam_source/llvm.${llvm_version} && \
    patch install.sh /patches/opam_llvm.5.0.0_install.sh.patch && \
    patch opam /patches/opam_llvm.5.0.0_opam.patch && \
    opam pin add llvm-custom . --kind=path
# END: adhoc for WebAssembly
RUN cp ~/.opam/${ocaml_version}/lib/llvm/static/*.cmxa ~/.opam/${ocaml_version}/lib/llvm/. # adhoc
RUN rm ~/.opam/archives/* && \
    rm ~/.opam/repo/default/archives/* && \
    rm ~/.opam/${ocaml_version}/bin/*.byte && \
    rm -r ~/.opam/repo/default/packages/* && \
    rm -r ~/.opam/repo/default/compilers/*

#
RUN echo 'eval `opam config env`' > ~/.bashrc

RUN apt-get clean -qq -y && rm -rf /var/lib/apt/lists/*

#
RUN mkdir /cibase
WORKDIR /cibase

ENTRYPOINT ["/bin/bash"]
