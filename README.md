# Rill Programming Language

*This repository is heavely under development...*

Rill is a programming language for java sparrow.  
This repository contains the implementation of Rill language.

Rill is designed for systems programming.


# How to build
## Preparation
```
opam install omake menhir batteries llvm.3.7
eval `opam config env`
```

## Build
`omake`


## Try
`omake && rillc/src/rillc && ./a.out`


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
