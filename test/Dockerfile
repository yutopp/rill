FROM ubuntu:16.04

MAINTAINER yutopp

# system
RUN apt-get update -qq -y
RUN apt-get install -qq -y git wget unzip

# get ocaml and opam
RUN apt-get install -qq -y ocaml g++
RUN apt-get install -qq -y aspcud
RUN wget https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh -O - | sh -s /usr/local/bin

# packages
RUN apt-get install -qq -y software-properties-common
RUN add-apt-repository ppa:ubuntu-toolchain-r/test
RUN apt-add-repository -s "deb http://apt.llvm.org/xenial/ llvm-toolchain-xenial-3.9 main"
RUN apt-get update -qq -y

RUN wget -O - http://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add -
RUN apt-get install -qq -y --allow-unauthenticated libllvm3.9 llvm-3.9 llvm-3.9-dev

RUN apt-get install -qq -y cmake python
RUN apt-get install -qq -y m4
RUN apt-get install -qq -y pkg-config

RUN opam init -y -a
RUN opam switch 4.03.0

RUN OPAMDOWNLOADJOBS=1 opam install omake menhir batteries ctypes-foreign stdint ocamlgraph
RUN OPAMDOWNLOADJOBS=1 opam install llvm.3.9
RUN OPAMDOWNLOADJOBS=1 opam install ounit                   # for unit testing
RUN OPAMDOWNLOADJOBS=1 opam install bisect_ppx ocveralls    # for testing

#
RUN update-alternatives --install /usr/bin/llc llc /usr/bin/llc-3.9 39

#
RUN mkdir /cibase
WORKDIR /cibase