{
  open Parser
  open Lexing

  exception UnexpectedToken of char
  exception LexerError of string

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
                 pos_lnum = pos.pos_lnum + 1
      }

  let _lex_string_lit_buffer = Buffer.create 80
}

let blank = [' ' '\t']+
let newline = "\r\n" | '\r' | '\n'

rule token = parse
  | blank               { token lexbuf }
  | newline             { next_line lexbuf; token lexbuf }

  | "def"               { DECL_DEF }
  | "val"               { KEYWORD_VAL }
  | "ref"               { KEYWORD_REF }
  | "extern"            { KEYWORD_EXTERN }
  | "=>"                { FAT_ARROW }

  | "true"              { LIT_TRUE }
  | "false"             { LIT_FALSE }

  (* string state *)
  | '"'                 { Buffer.clear _lex_string_lit_buffer;
                          read_string_lit _lex_string_lit_buffer lexbuf
                        }

  | ['0'-'9']+ as i     { INT (int_of_string i) }
  | ['a'-'z' '_']+ as s { ID s }

  | "||"                { LOGICAL_OR }
  | "&&"                { LOGICAL_AND }

  | "=="                { EQUALS }
  | "!="                { NOT_EQUALS }

  | "<<"                { LSHIFT }
  | ">>"                { RSHIFT }

  | ">="                { GTE }
  | "<="                { LTE }

  | '|'                 { BITWISE_OR }
  | '^'                 { BITWISE_XOR }
  | '&'                 { BITWISE_AND }

  | '>'                 { GT }
  | '<'                 { LT }

  | '+'                 { PLUS }
  | '-'                 { MINUS }
  | '*'                 { TIMES }
  | '/'                 { DIV }
  | '%'                 { MOD }

  | '='                 { ASSIGN }

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
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/'; read_string_lit buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string_lit buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string_lit buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string_lit buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string_lit buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string_lit buf lexbuf
    }
  | _ { raise (LexerError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof { raise (LexerError ("String is not terminated")) }