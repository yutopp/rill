ARG OCAML_VERSION="4.10.0"
ARG OPAM_VERSION="2.0.7"

######################################################################
#
FROM ubuntu:20.04 as builder

#
RUN apt-get update && apt-get install -y \
    curl \
    cmake ninja-build gcc g++ python3 z3 m4 patch unzip \
 && rm -rf /var/lib/apt/lists/*

# opam
WORKDIR /tmp
RUN curl -sSL https://github.com/ocaml/opam/releases/download/${OPAM_VERSION}/opam-${OPAM_VERSION}-x86_64-linux -o opam
RUN install opam /usr/local/bin/opam

# OCaml
WORKDIR /
ENV OPAMKEEPBUILDDIR false
ENV OPAMBUILDDOC false
ENV OPAMDOWNLOADJOBS 1
RUN opam init -y -a --disable-sandboxing --comp=${OCAML_VERSION}

######################################################################
#
FROM yutopp/rill-build-env:llvm-11.0.0-x86_64 as llvm

######################################################################
#
FROM ubuntu:20.04

LABEL maintainer="yutopp@gmail.com"

ENV DEBIAN_FRONTEND=noninteractive

#
RUN apt-get update && apt-get install -y \
    curl \
    cmake make gcc g++ python3 z3 m4 patch unzip pkg-config git rsync \
 && rm -rf /var/lib/apt/lists/*

# Copy LLVM
COPY --from=llvm /usr/local/llvm/lib/* /usr/local/lib/
COPY --from=llvm /usr/local/llvm/include/* /usr/local/include/
COPY --from=llvm /usr/local/llvm/bin/llvm-config /usr/local/bin/.

# Copy opam
COPY --from=builder /tmp/opam /tmp/opam
RUN install /tmp/opam /usr/local/bin/opam

# Copy OCaml
RUN mkdir -p /root/.opam/
COPY --from=builder /root/.opam/config /root/.opam/config
COPY --from=builder /root/.opam/${OCAML_VERSION} /root/.opam/${OCAML_VERSION}
COPY --from=builder /root/.opam/opam-init /root/.opam/opam-init
COPY --from=builder /root/.opam/repo/repos-config /root/.opam/repo/repos-config
