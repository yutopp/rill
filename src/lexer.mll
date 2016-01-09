{
  open Parser
  open Lexing

  exception UnexpectedToken of char

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
                 pos_lnum = pos.pos_lnum + 1
      }
}

let blank = [' ' '\t']+
let newline = "\r\n" | '\r' | '\n'

rule token = parse
    blank               { token lexbuf }
  | newline             { next_line lexbuf; token lexbuf }

  | "def"               { DECL_DEF }
  | "=>"                { FAT_ARROW }

  | ['0'-'9']+ as i     { INT (int_of_string i) }
  | ['a'-'z' '_']+ as s { ID s }

  | '+'                 { PLUS }
  | '-'                 { MINUS }
  | '*'                 { TIMES }
  | '/'                 { DIV }
  | '='                 { ASSIGN }

  | '('                 { LPAREN }
  | ')'                 { RPAREN }

  | '{'                 { LBLOCK }
  | '}'                 { RBLOCK }

  | ','                 { COMMA }
  | ';'                 { SEMICOLON }

  | eof                 { EOF }

  | _ as s              { raise (UnexpectedToken s) }
