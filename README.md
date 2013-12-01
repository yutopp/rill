Rill
--

**実験中**

文鳥の文鳥による文鳥のためのプログラミング言語Rillの処理系です．

ブン！ (◔⊖◔)つ < "ひとまず動くようになるまでゴリ押しで書くのでコードは汚いゾ"

構文解析その他諸々にBoost，コード生成にLLVMを用いています．
また，コードはC++11で書かれています．

値やリソースの扱いを細かく指定できるようにし，文鳥が楽しくプログラミングを出来るような言語を目指します．


# Using Library

## Boost 1.55.0
## LLVM 3.3


# How to build

### GCC >= 4.7.3
### Clang >= 3.3


(using MSVC)



(using GCC)


    
(using Clang)

    mkdir build
    cd build
    cmake ../. -DCMAKE_CXX_COMPILER=clang++ -DCMAKE_C_COMPILER=clang -DLLVM_ROOT=/usr/local
    make
    sudo make install


Reference
--
under construction


Sample Code of Rill
--

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



License
--

Boost License Version 1.0

