(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

%token  <(int * int * bool)>    INT
%token  <string>        ID STRING
%token  <string>        PLUS MINUS TIMES DIV MOD
                        GT GTE LT LTE
                        LSHIFT ARSHIFT LRSHIFT
                        EQUALS NOT_EQUALS
                        ASSIGN
                        LOGICAL_OR LOGICAL_AND
                        BITWISE_AND BITWISE_OR BITWISE_XOR
                        INCREMENT DECREMENT
                        NOT
%token                  LPAREN RPAREN
                        LBLOCK RBLOCK
                        LBRACKET RBRACKET
%token                  COMMA DOT
                        COLON SEMICOLON
                        COLONCOLON
                        (*BACKQUOTE*) SINGLEQUOTE
                        FAT_ARROW SHARP
%token                  EOF
%token                  LIT_TRUE LIT_FALSE
                        KEYWORD_DEF
                        KEYWORD_CLASS
                        KEYWODD_PACKAGE
                        KEYWODD_MODULE
                        KEYWORD_VAL
                        KEYWORD_REF
                        KEYWORD_LET
                        KEYWORD_IMMUTABLE
                        KEYWORD_CONST
                        KEYWORD_MUTABLE
                        KEYWORD_EXTERN
                        KEYWORD_RETURN
                        KEYWORD_OPERATOR
                        KEYWORD_IMPORT
                        KEYWORD_UU_STMT_TRAITS
                        KEYWORD_ONLY_META
                        KEYWORD_META
                        KEYWORD_RUNTIME
                        KEYWORD_ONLY_RUNTIME
                        KEYWORD_IF
                        KEYWORD_LOOP
                        KEYWORD_BREAK
                        KEYWORD_FOR
                        KEYWORD_ELSE
                        KEYWORD_WHEN
                        KEYWORD_WITH
                        KEYWORD_PRE
                        KEYWORD_POST
                        KEYWORD_UNARY
                        KEYWORD_STATIC
                        KEYWORD_GC
                        KEYWORD_UNMANAGED
                        KEYWORD_PUBLIC
%%
