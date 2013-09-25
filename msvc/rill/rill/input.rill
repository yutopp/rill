Rill
--

**実験中**

文鳥の文鳥による文鳥のためのプログラミング言語Rillの処理系です．

ブン！ (◔⊖◔)つ < "ひとまず動くようになるまでゴリ押しで書くのでコードは汚いゾ"

構文解析その他諸々にBoost，コード生成にLLVMを用いています．
また，コードはC++11で書かれています．

特定の環境のVisual Studio 2013 RC (v120)でしかテストしていません．


値やリソースの扱いを細かく指定できるようにし，文鳥が楽しくプログラミングを出来るような言語を目指します．


Using Library
--
Boost-trunk rev.84700

LLVM-trunk rev.190764



How to build
--

### MSVC >= v1800 [Visual Studio 2013 RC (v120)]
Please use project file.
And, please change library pathes of the project file.

### gcc >= 4.7.2

    cd tools/compile
    export BOOST_ROOT=(YOUR BOOST PATH)
Please change **(YOUR BOOST PATH)** to your Boost downloaded path.
If you have not used to Boost Libraries, please execute **bootstrap.sh** in your Boost path. Then, a executable file named **b2** will be created on there. It is required.
Next,

    $BOOST_ROOT/b2 toolset=gcc
Then, a executable file named **rillc** will be created in "bin" subdirectories.



Sample
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

