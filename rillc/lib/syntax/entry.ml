(*
 * Copyright yutopp 2019 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

module Span = Common.Span
module Diagnostics = Common.Diagnostics

module I = Parser.MenhirInterpreter

module Ast = Ast

exception CannotRecoverly of string * (Lexing.position * Lexing.position)

let succeed dm is_incomplete (v : Ast.t) =
  (Some v, is_incomplete, dm)

let rec fail dm is_incomplete sup inputneeded checkpoint =
  match checkpoint with
  | I.HandlingError env ->
     let lex_loc = I.positions env in
     let span = Supplier.create_span_with_lex_loc sup lex_loc in

     (* TODO: implement error recovery *)
     let reason = new Reasons.invalid_syntax in
     let d = Diagnostics.create ~reason ~span ~phase:Diagnostics.PhaseParsing in
     (None, is_incomplete, Diagnostics.Multi.append dm d)

  | _ ->
     failwith "[ICE]"

let entry dm sup =
  try
    let pos = Supplier.start_pos sup in
    let start = (Parser.Incremental.program_entry pos) in
    I.loop_handle_undo (succeed dm false) (fail dm false sup) (Supplier.get sup) start
  with
  | Lexer.LexerError detail ->
     let span = Supplier.create_span sup in
     (* TODO: use typed reason instead of detail *)
     let reason = new Reasons.invalid_token ~token:detail in
     let d = Diagnostics.create ~reason ~span ~phase:Diagnostics.PhaseParsing in
     (None, false, Diagnostics.Multi.append dm d)

  | Lexer.UnexpectedToken tok ->
     let span = Supplier.create_span sup in
     let reason = new Reasons.unexpected_token ~ch:tok in
     let d = Diagnostics.create ~reason ~span ~phase:Diagnostics.PhaseParsing in
     (None, false, Diagnostics.Multi.append dm d)
