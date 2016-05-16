(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

{
  open Tokens
  open Lexing

  exception UnexpectedToken of char
  exception LexerError of string

  let _lex_string_lit_buffer = Buffer.create 80
}

let blank = [' ' '\t']+
let newline = "\r\n" | '\r' | '\n'

rule token = parse
  | blank               { token lexbuf }
  | newline             { new_line lexbuf; token lexbuf }

  | "//"                { oneline_comment lexbuf }
  | "/*"                { multiline_comment lexbuf }
  | "/+"                { nested_multiline_comment 0 lexbuf }

  | "__statement_traits"{ KEYWORD_UU_STMT_TRAITS }
  | "operator"          { KEYWORD_OPERATOR }
  | "import"            { KEYWORD_IMPORT }
  | "def"               { KEYWORD_DEF }
  | "class"             { KEYWORD_CLASS }
  | "val"               { KEYWORD_VAL }
  | "ref"               { KEYWORD_REF }
  | "immutable"         { KEYWORD_IMMUTABLE }
  | "const"             { KEYWORD_CONST }
  | "mutable"           { KEYWORD_MUTABLE }
  | "extern"            { KEYWORD_EXTERN }
  | "return"            { KEYWORD_RETURN }
  | "onlymeta"          { KEYWORD_ONLY_META }
  | "meta"              { KEYWORD_META }
  | "runtime"           { KEYWORD_RUNTIME }
  | "onlyruntime"       { KEYWORD_ONLY_RUNTIME }
  | "if"                { KEYWORD_IF }
  | "for"               { KEYWORD_FOR }
  | "else"              { KEYWORD_ELSE }
  | "when"              { KEYWORD_WHEN }
  | "pre"               { KEYWORD_PRE }
  | "post"              { KEYWORD_POST }
  | "unary"             { KEYWORD_UNARY }

  | "true"              { LIT_TRUE }
  | "false"             { LIT_FALSE }

  | "=>"                { FAT_ARROW }

  (* string state *)
  | '"'                 { Buffer.clear _lex_string_lit_buffer;
                          read_string_lit _lex_string_lit_buffer lexbuf
                        }

  | ['0'-'9']+ as i     { INT (int_of_string i) }
  | ("0x" ['0'-'9' 'a'-'f' 'A'-'F']+) as i  { INT (int_of_string i) }

  | ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* as s
                        { ID s }

  | "++" as op          { INCREMENT op }
  | "--" as op          { DECREMENT op }

  | "||" as op          { LOGICAL_OR op }
  | "&&" as op          { LOGICAL_AND op }

  | "==" as op          { EQUALS op }
  | "!=" as op          { NOT_EQUALS op }

  | "<<" as op          { LSHIFT op }
  | ">>" as op          { RSHIFT op }

  | ">=" as op          { GTE op }
  | "<=" as op          { LTE op }

  | '|' as op           { BITWISE_OR (Char.escaped op) }
  | '^' as op           { BITWISE_XOR (Char.escaped op) }
  | '&' as op           { BITWISE_AND (Char.escaped op) }

  | '>' as op           { GT (Char.escaped op) }
  | '<' as op           { LT (Char.escaped op) }

  | '+' as op           { PLUS (Char.escaped op) }
  | '-' as op           { MINUS (Char.escaped op) }
  | '*' as op           { TIMES (Char.escaped op) }
  | '/' as op           { DIV (Char.escaped op) }
  | '%' as op           { MOD (Char.escaped op) }

  | '=' as op           { ASSIGN (Char.escaped op) }

  | '!' as op           { NOT (Char.escaped op) }

  | '#'                 { SHARP }
  | '['                 { LBRACKET }
  | ']'                 { RBRACKET }

  | '('                 { LPAREN }
  | ')'                 { RPAREN }

  | '{'                 { LBLOCK }
  | '}'                 { RBLOCK }

  | ','                 { COMMA }
  | '.'                 { DOT }
  | ':'                 { COLON }
  | ';'                 { SEMICOLON }

  | eof                 { EOF }

  | _ as s              { raise (UnexpectedToken s) }

and read_string_lit buf = parse
  | '"'             { STRING (Buffer.contents buf) }
  | '\\' '/'        { Buffer.add_char buf '/'; read_string_lit buf lexbuf }
  | '\\' '\\'       { Buffer.add_char buf '\\'; read_string_lit buf lexbuf }
  | '\\' 'n'        { Buffer.add_char buf '\n'; read_string_lit buf lexbuf }
  | '\\' 'r'        { Buffer.add_char buf '\r'; read_string_lit buf lexbuf }
  | '\\' 't'        { Buffer.add_char buf '\t'; read_string_lit buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string_lit buf lexbuf
    }
  | _   { raise (LexerError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (LexerError ("String is not terminated")) }

and oneline_comment = parse
  | newline         { new_line lexbuf; token lexbuf }
  | eof             { EOF }
  | _               { oneline_comment lexbuf }

and multiline_comment = parse
  | "*/"            { token lexbuf }
  | eof             { EOF }
  | _               { multiline_comment lexbuf }

and nested_multiline_comment n = parse
  | "+/"            {
                        if n = 0 then
                          token lexbuf
                        else
                          nested_multiline_comment (n-1) lexbuf
                    }
  | "/+"            { nested_multiline_comment (n+1) lexbuf }
  | eof             { EOF }
  | _               { nested_multiline_comment n lexbuf }
