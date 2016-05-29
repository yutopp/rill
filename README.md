# Rill Programming Language

*This repository is heavely under development...*

![chi-](http://yutopp.net/image/chi-.png "Bun")
[![Circle CI](https://circleci.com/gh/yutopp/rill.png?style=badge)](https://circleci.com/gh/yutopp/rill)

Rill is a programming language for java sparrow.  
This repository contains the implementation of Rill language.

Rill is designed for systems programming.

## Influences
+ Freedom of C++
+ Compiletime features, UFCS, modules of Dlang
+ Resource management of Rust
+ Macros of Scala


# How to build
## Requirements
+ OCaml (>= 4.02.3)
+ OPAM
+ LLVM (>= 3.8)

## Preparation
```
opam install omake menhir batteries ctypes-foreign stdint llvm.3.8
eval `opam config env`
```
`opam update` might be required to install these packages.

## Build
`omake`

## Try
`rillc/src/rillc test/compilable/hello_world.rill && ./a.out`


## License
Boost License Version 1.0
