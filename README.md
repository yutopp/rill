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
+ OCaml (>= 4.02.3) {build}
+ OPAM {build}
+ LLVM (>= 3.8.0)
+ GCC (>= 4.9.0?)

## Development
### Preparation
```
opam install omake menhir batteries ctypes-foreign stdint llvm.3.8
opam install ounit                  # for unit testing
opam install bisect_ppx ocveralls   # for coverage
eval `opam config env`
```
`opam update` might be required to install these packages.

### Build
`omake`

### Test
`omake test`

or `omake unit_test` and `omake e2e_test`

### Try
`rillc/src/rillc test/compilable/hello_world.rill && ./a.out`

## Release
### Use OPAM
```
opam pin add rill .
opam install rill.0.0.1
```
and
```
opam upgrade rill.0.0.1
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
|:--|:--|:--|
|PREFIX|`/usr/local`|
|RELEASE|`false`|
|COVERAGE|`false`|
|USE_LOCAL_DEV_LIB|not $(RELEASE)|
if you change these values, please run `omake clean` every time.

## License
Boost License Version 1.0
