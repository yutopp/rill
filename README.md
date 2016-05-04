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
## Preparation
```
opam install omake menhir batteries ctypes-foreign llvm.3.7
eval `opam config env`
```

## Build
`omake`


## Try
`rillc/src/rillc test/compilable/hello_world.rill && ./a.out`


# Trouble shoorting
## Failed to opam install llvm

If you have not installed LLVM yet, please install LLVM-3.7 before `opam install llvm.3.7`.

Still error happens and if you are GNU/Linux user, it maybe lack of a symbolic link.  
An error message said `/usr/bin/ld: cannot find -lLLVM-3.7`?  
If so, commands like below will solve this problem.
```
sudo ln -s /usr/lib/libLLVM.so.3.7 /usr/lib/libLLVM-3.7.so
```


## License
Boost License Version 1.0
