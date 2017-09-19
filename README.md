# Rill Programming Language

*This repository is heavely under development...*

![chi-](http://yutopp.net/image/chi-.png "Bun")
[![Circle CI](https://circleci.com/gh/yutopp/rill.png?style=badge)](https://circleci.com/gh/yutopp/rill)
[![Coverage Status](https://coveralls.io/repos/github/yutopp/rill/badge.svg?branch=master)](https://coveralls.io/github/yutopp/rill?branch=master)

Rill-lang is a programming language for java sparrow.  
This repository contains the implementation of Rill language.

Rill is designed for systems programming.

## Influences

+ Freedom of C++
+ Compiletime features, UFCS, modules of Dlang
+ Resource management of Rust
+ Macros of Scala

# How to build

## Requirements

+ OCaml {build} (>= 4.04.0)
+ OPAM {build} (>= 1.2)
+ OMake {build} (= 0.10.2)
+ LLVM (>= 4.0.0)
+ GCC (>= 5.4.0)

## Development

### Supported Environments

- Arch Linux (x86_64)
- Ubuntu 16.04 (x86_64)

### Preparation

Install packages to a host machine directly or use docker.

#### Install packages directly

```
opam install omake.0.10.2 menhir batteries ctypes-foreign stdint ocamlgraph llvm.5.0.0
opam install ounit                  # for unit testing
opam install bisect_ppx ocveralls   # for coverage
eval `opam config env`
```

`opam update` might be required to install these packages.  
NOTE: Some packages(e.g. LLVM) required native libraries. Please build or install native libraries before installing opam packages.

#### Use docker

The docker image `yutopp/rill-build-env` will provide an enviroment which satisfies requirements for developing the Rill language. Pull or build the image like below.
`docker pull yutopp/rill-build-env:latest`  
or  
`docker build --squash -t yutopp/rill-build-env -f ./docker/Dockerfile ./docker`

Then, enter into the container.
`docker run -it --rm -v $(pwd):/cibase yutopp/rill-build-env`

### Build

`omake`

### Test

`omake test`

or `omake unit_test` and `omake e2e_test`

### Try

`rillc/src/rillc test/pass/hello_world.rill -o a.out && ./a.out`

## Release

### Use OPAM

```
opam pin add rill .
opam install rill.0.0.1-dev
```

and

```
opam upgrade rill.0.0.1-dev
```

### Use OMake directly

First, please install libraries and packages. See [Requirements](#requirements) and [Preparation](#preparation).  
Next, run the commands below.

```
omake RELEASE=true
omake install
```

You can use these variables.

|variable|default value|
|:---|:---|
|PREFIX|`/usr/local`|
|RELEASE|`false`|
|COVERAGE|`false`|
|USE_LOCAL_DEV_LIB|not $(RELEASE)|

If you change these values, please run `omake clean` every time.

## License

Boost License Version 1.0
