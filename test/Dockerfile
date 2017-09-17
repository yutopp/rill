FROM ubuntu:16.04

LABEL maintainer yutopp

# Setup
RUN apt-get update -qq -y && \
    apt-get install -qq -y git wget curl unzip \
                           g++ aspcud  cmake python m4 pkg-config libffi-dev && \
    apt-get clean -qq -y && \
    rm -rf /var/lib/apt/lists/*

# Download LLVM 5.0.0
# https://llvm.org/svn/llvm-project/llvm/tags/RELEASE_500/final/CMakeLists.txt
ENV llvm_version 5.0.0

RUN wget -q http://www.llvm.org/releases/${llvm_version}/llvm-${llvm_version}.src.tar.xz -O - | tar -xJ
RUN cd llvm-${llvm_version}.src && \
    mkdir build && \
    cd build && \
    cmake -G 'Unix Makefiles' \
          -DCMAKE_INSTALL_PREFIX=/usr/local \
          -DCMAKE_BUILD_TYPE=Release \
          -DLLVM_TARGETS_TO_BUILD="X86" \
          -LLVM_TARGETS_WITH_JIT="X86" \
          -DLLVM_EXPERIMENTAL_TARGETS_TO_BUILD="WebAssembly" \
          -LLVM_ENABLE_TERMINFO=OFF \
          -LLVM_ENABLE_ZLIB=OFF \
          -LLVM_INSTALL_TOOLCHAIN_ONLY=ON \
          -DLLVM_BUILD_TOOLS=OFF \
          -DLLVM_INCLUDE_UTILS=OFF \
          -LLVM_BUILD_UTILS=OFF \
          -DLLVM_INCLUDE_EXAMPLES=OFF \
          -DLLVM_INCLUDE_TESTS=OFF \
          -DLLVM_INCLUDE_GO_TESTS=OFF \
          -LLVM_INCLUDE_DOCS=OFF \
          -LLVM_BUILD_DOCS=OFF \
          ..

# Build LLVM for static lib
RUN cd llvm-${llvm_version}.src/build && \
    cmake --build . --target package/fast -- -j4
RUN cd llvm-${llvm_version}.src/build && \
    cmake --build . --target llvm-config/fast -- -j4
RUN cd llvm-${llvm_version}.src/build && \
    cmake --build . --target install && \
    cp bin/llvm-config /usr/local/bin/.

# Clean up LLVM src
RUN rm -rf llvm-${llvm_version}.src

# Install OCaml and packages
RUN wget https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh -O - | sh -s /usr/local/bin
ENV OPAMKEEPBUILDDIR false
ENV OPAMBUILDDOC false
ENV OPAMDOWNLOADJOBS 1
RUN opam init -y -a --comp=4.04.0
RUN opam install omake.0.10.2 menhir batteries ctypes-foreign stdint ocamlgraph
RUN opam install ounit bisect_ppx ocveralls
RUN opam install llvm.${llvm_version}
RUN cp ~/.opam/4.04.0/lib/llvm/static/*.cmxa ~/.opam/4.04.0/lib/llvm/. # adhoc
RUN rm ~/.opam/archives/* && \
    rm ~/.opam/repo/default/archives/* && \
    rm ~/.opam/4.04.0/bin/*.byte && \
    rm -r ~/.opam/repo/default/packages/* && \
    rm -r ~/.opam/repo/default/compilers/*

#
RUN echo 'eval `opam config env`' > ~/.bashrc

#
RUN mkdir /cibase
WORKDIR /cibase

ENTRYPOINT ["/bin/bash"]
