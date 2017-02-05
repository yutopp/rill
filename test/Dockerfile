FROM ubuntu:16.04

MAINTAINER yutopp

# add LLVM repository (3.9)
RUN echo "deb http://apt.llvm.org/xenial/ llvm-toolchain-xenial-3.9 main" \
         >> /etc/apt/sources.list && \
    echo "deb-src http://apt.llvm.org/xenial/ llvm-toolchain-xenial-3.9 main" \
         >> /etc/apt/sources.list

#
RUN echo "nameserver 8.8.8.8" >> /etc/resolv.conf && \
    apt-get update -qq -y -o Acquire::http::Timeout=30 && \
    apt-get install -qq -y git wget curl unzip && \
    \
    wget -O - http://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add - && \
    apt-get install -qq -y --allow-unauthenticated llvm-3.9 llvm-3.9-dev && \
    update-alternatives --install /usr/bin/llc llc /usr/bin/llc-3.9 39 && \
    \
    apt-get install -qq -y g++ aspcud \
                           cmake python m4 pkg-config && \
    \
    apt-get clean -qq -y && \
    rm -rf /var/lib/apt/lists/*

#
RUN wget https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh -O - | sh -s /usr/local/bin && \
    OPAMKEEPBUILDDIR=false OPAMBUILDDOC=false opam init -y -a --comp=4.04.0 && \
    OPAMKEEPBUILDDIR=false OPAMBUILDDOC=false OPAMDOWNLOADJOBS=1 \
    opam install omake.0.10.2 menhir batteries ctypes-foreign \
                 stdint ocamlgraph llvm.3.9 \
                 ounit bisect_ppx ocveralls && \
    \
    rm ~/.opam/archives/* && \
    rm ~/.opam/repo/default/archives/* && \
    rm ~/.opam/4.04.0/bin/*.byte && \
    rm -r ~/.opam/repo/default/packages/* && \
    rm -r ~/.opam/repo/default/compilers/*

#
RUN mkdir /cibase
WORKDIR /cibase