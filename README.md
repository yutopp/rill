# Rill Programming Language
**experimental...**
![chi-](http://yutopp.net/image/chi-.png "Bun")
[![Circle CI](https://circleci.com/gh/yutopp/rill.png?style=badge)](https://circleci.com/gh/yutopp/rill)

Rill is a programming language for java sparrow.  
This repository contains the implementation of Rill language.

文鳥の文鳥による文鳥のためのプログラミング言語 Rill の処理系です  
ブン！ (◔⊖◔) < "ひとまず動くようになるまでゴリ押しで書くのでコードは汚いゾ"


## Sample Codes of Rill
[testcases](test/integration)  
[samples](tools/compiler/samples)


## How to build
### Requirement
#### Compiler
- Clang >= 3.5.0(C++14)

#### Libraries
- Boost >= 1.56.0
- LLVM == 3.5.0

#### Tools
- CMake >= 2.8.8

If you are Arch/Antergos user, install these packages like below
```
sudo pacman -S clang boost llvm cmake
```

### Build and install
For example,
```
git clone git@github.com:yutopp/rill.git
cd rill
mkdir build
cd build
cmake ../.
make -j4
sudo make install
```
Rill specific variables for CMake

|Name|Description|Default|
|:--|:--|:--|
|LLVM_CONFIG_PATH | location path of `llvm-config` | `/usr/bin/llvm-config` |
|BOOST_ROOT| location path of boost libraries | `/usr/local` |
|RILL_CXX_COMPILER| compiler which is used when building Rill | `clang++` |
|RUN_TEST| set `ON` if you would like to run tests | `OFF` |
|RILL_LINK_RUNTIME_LIB_PATH|| `${CMAKE_INSTALL_PREFIX}/lib/librill-rt.a` |
|RILL_IMPORT_RUNTIME_LIB_DIR|| `${CMAKE_INSTALL_PREFIX}/lib/rill-rt` |
Please change these variables to fit your environment.
e.g.
```
cmake ../. -DBOOST_ROOT=/usr -DLLVM_CONFIG_PATH=/usr/bin/llvm-config -DRUN_TEST=ON
```

After that, execute `make`, (`make test`),  and `sudo make install`.


#### Other configuration
You can specify paths that dependent libraries are installed by using `CMAKE_PREFIX_PATH` . e.g.

```
cmake ../. -DCMAKE_PREFIX_PATH=/usr/local
```

And, you can specify the path that Rill will be installed by using `CMAKE_INSTALL_PREFIX` . e.g.

```
cmake ../. -DCMAKE_INSTALL_PREFIX=/usr/local/torigoya
```

If you want to use GCC, call CMake like below.

```
cmake ../. -DRILL_CXX_COMPILER=g++
```

## Test
```
cd build
make test
```

### Auto testing
At first time,
```
bundle install --path vendor/bundle
```

Next, run cmake under the `build` directory.  
After that, execute below
```
cd build
bundle exec guard -i -G ../Guardfile -w ../
```


## How to use
e.g. (on the directory of rill)
```
rillc tools/compiler/samples/...
```
and then, executable file `a.out` will be generated.  
To see detail, execute `rillc --help`.


## Reference
under construction


## Special Thanks
- [AUR / rill-git](https://aur.archlinux.org/packages/rill-git/)
- [Emacs / rill-mode](https://github.com/Johniel/rill-mode)


## License
Boost License Version 1.0
