%token  <int>           INT
%token  <string>        ID STRING
%token  <string>        PLUS MINUS TIMES DIV MOD
                        GT GTE LT LTE
                        LSHIFT RSHIFT
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
                        FAT_ARROW SHARP
%token                  EOF
%token                  LIT_TRUE LIT_FALSE
                        KEYWORD_DEF
                        KEYWORD_CLASS
                        KEYWORD_VAL
                        KEYWORD_REF
                        KEYWORD_EXTERN
                        KEYWORD_OPERATOR
                        KEYWORD_IMPORT

%%
