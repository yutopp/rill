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

type t = { ds : Diagnostics.t; is_complete : bool }

exception CannotRecoverly of string * (Lexing.position * Lexing.position)

let rec entry sup ~ds : (Ast.t * t, Diagnostics.Elem.t) Result.t =
  let p_state = { ds; is_complete = false } in
  try
    let pos = Supplier.start_pos sup in
    let start = Parser.Incremental.program_entry pos in
    Ok
      (I.loop_handle_undo (succeed p_state) (fail p_state sup)
         (Supplier.get sup) start)
  with
  | Lexer.LexerError detail ->
      let span = Supplier.create_span sup in
      (* TODO: use typed reason instead of detail *)
      let e = new Reasons.invalid_token ~token:detail in
      let elm = Diagnostics.Elem.error ~span e in
      Error elm
  | Lexer.UnexpectedToken tok ->
      let span = Supplier.create_span sup in
      let e = new Reasons.unexpected_token ~ch:tok in
      let elm = Diagnostics.Elem.error ~span e in
      Error elm
  | CannotRecoverly (_msg, position) ->
      let span = Supplier.create_span_with_lex_loc sup position in
      let e = new Reasons.invalid_syntax in
      let elm = Diagnostics.Elem.error ~span e in
      Error elm

and succeed p_state (v : Ast.t) = (v, { p_state with is_complete = true })

and fail p_state sup inputneeded checkpoint =
  match checkpoint with
  | I.HandlingError env ->
      let positions = I.positions env in
      raise (CannotRecoverly ("", positions))
  | _ -> failwith "[ICE]"
