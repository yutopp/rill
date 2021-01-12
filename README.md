# Rill Programming Language

*This repository is heavely under development...*

![chi-](http://yutopp.net/image/chi-.png "Bun")
[![Circle CI](https://circleci.com/gh/yutopp/rill.png?style=badge)](https://circleci.com/gh/yutopp/rill)
[![Coverage Status](https://coveralls.io/repos/github/yutopp/rill/badge.svg?branch=master)](https://coveralls.io/github/yutopp/rill?branch=master)

Rill-lang is a programming language which is designed for systems programming.

This repository contains the implementation of Rill-lang.

## Influences

+ Freedom of C++
+ Compiletime features, UFCS, modules of Dlang
+ Resource management of Rust
+ Macros of Scala

# How to build

## Supported Environments

- Arch Linux (host: x86_64)
- Ubuntu 20.04 (host: x86_64)

## Prerequisites

* OCaml (>= 4.09.0)
* OPAM (>= 2.1.0~beta2)
  * dune (>= 2.5)
* CMake (>= 3.5.1)
* LLVM (>= 11.0.0)
* GCC (>= 10)

## Steps

### Clone files

```bash
$ git clone https://github.com/yutopp/rill.git
$ cd rill
```

### Setup rillc (compiler only) environments

See [rillc/README](./rillc/README.md).

### Generate project files by using CMake

```bash
rill$ cmake -B build
```

The project will be created under the `build` directory, which can be named any way you like.

### Build a toolchain

```bash
rill$ cmake --build build
```

or

```bash
rill$ cd build
build$ make
```

### Run tests

```bash
rill$ cd build
build$ make CTEST_OUTPUT_ON_FAILURE=1 test
```

### Install a toolchain

```
cmake --install build --prefix /usr/local
```

A toolchain will be installed under the specified `prefix`.

## License

Boost License Version 1.0
