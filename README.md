ブン
=

**実験中**
文鳥の文鳥による文鳥のための言語

ブン！ (◔⊖◔)つ < "ひとまず動くようになるまでゴリ押しで書いたのでコードは汚い"


Using Library
-
Boost 1.53.0


How to build
-

### MSVC >= v120_CTP_Nov2012
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
-

    4 *    	(3 + 1)
    	* 8
	
    	+ 6;

    foo( 20 );

    def foo( hoge: int ): int
    {
        return hoge*10;
    }

    foo( 10 ) + 2 * 5;

    foo( 5 + 5 * 2 + (20 + 30*2 + 5) ) + 2 * 5;


License
-

Boost License Version 1.0  