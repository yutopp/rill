(*
 * Copyright yutopp 2019 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

module Span = Common.Span

module I = Parser.MenhirInterpreter

type t = {
  path: string; (* TODO: fix *)
  lexbuf: Lexing.lexbuf;
  supplier: I.supplier;
  mutable tokens: (Parser.token * Lexing.position * Lexing.position) list;
  mutable index: int;
}

let create ~path ~lexbuf =
  {
    path;
    lexbuf;
    supplier = I.lexer_lexbuf_to_supplier Lexer.token lexbuf;
    tokens = [];
    index = 0;
  }

let top sup : (Parser.token * Lexing.position * Lexing.position) =
  List.hd_exn sup.tokens

let push sup tup : unit =
  sup.tokens <- tup :: sup.tokens

let next sup =
  let tup =
    if (List.length sup.tokens) <= sup.index then
      let tup = sup.supplier () in
      sup.tokens <- tup :: sup.tokens;
      tup
    else
      List.nth_exn sup.tokens ((List.length sup.tokens) - sup.index - 1)
  in
  sup.index <- sup.index + 1;

  let (tok, s, e) = tup in

  tup

let get sup : I.supplier =
  let f () =
    next sup
  in
  f

let start_pos sup =
  let lexbuf = sup.lexbuf in
  Lexing.lexeme_start_p lexbuf

let end_pos sup =
  let lexbuf = sup.lexbuf in
  Lexing.lexeme_end_p lexbuf

let create_span_with_lex_loc sup lex_loc =
  Span.create_from_lex_loc ~path:sup.path ~lex_loc

let create_span sup =
  let lexbuf = sup.lexbuf in
  let lex_loc = (start_pos sup, end_pos sup) in
  create_span_with_lex_loc sup lex_loc
