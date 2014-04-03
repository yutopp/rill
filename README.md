# Rill Programming Language
**experimental**

Rill is a programming language for java sparrow. :bird:

This repository contains the implementation of Rill language.

文鳥の文鳥による文鳥のためのプログラミング言語 Rill の処理系です

ブン！ (◔⊖◔) < "ひとまず動くようになるまでゴリ押しで書くのでコードは汚いゾ"


## How to build
### Requirement
#### Compiler
- GCC >= 4.7.2
- Clang >= 3.3

#### Libraries
- Boost 1.55.0
- LLVM 3.4

If you are Ubuntu/Mint user, these links will be useful...
- [boost-latest](https://launchpad.net/~boost-latest/+archive/ppa "boost-latest")
- [LLVM Debian/Ubuntu nightly packages](http://llvm.org/apt/ "LLVM Debian/Ubuntu nightly packages")


### build and install
For example,
```
git clone git@github.com:yutopp/rill.git
cd rill
mkdir test_build
cd test_build
cmake ../. -DLLVM_CONFIG_PATH=YOUR_LLVM_CONFIG_PATH -DBOOST_ROOT=YOUR_BOOST_INSTALLED_PATH
```
please change `YOUR_LLVM_CONFIG_PATH`(e.g. `/usr/bin/llvm-config-3.4` ) and `YOUR_BOOST_INSTALLED_PATH`(e.g. `/usr/local` ) to fit your environment. But the variable `YOUR_BOOST_INSTALLED_PATH` is *optional*.

then, execute `make` and `sudo make install` !



You can specify paths that dependent libraries are installed by using `CMAKE_PREFIX_PATH` . e.g.

```
cmake ../. -DCMAKE_PREFIX_PATH=/usr/local
```

And, you can specify the path that Rill will be installed by using `CMAKE_INSTALL_PREFIX` . e.g.

```
cmake ../. -DCMAKE_INSTALL_PREFIX=/usr/local/torigoya
```

If you want to use Clang, call CMake like below.

```
cmake ../. -DCMAKE_CXX_COMPILER=clang++ -DCMAKE_C_COMPILER=clang
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


## Sample Code of Rill

```
def main(): int
{
    print( "hello, bunchou lang!!!" );
    test();

    return 0;
}

def test(): void
{
    extern_print_string( foo( ( 10*2 )*(1+2*2   ), 10 ) + 2 * 5 );
}

extern def extern_print_string( :int ): void "put_string2";


def foo( fuga: int, hoge: int ): int
{
    return foo(fuga) * hoge;
}

def foo( a: int ): int
{
    return a;
}

// comment
;/*empty statment*/;;;
```


## License

Boost License Version 1.0
