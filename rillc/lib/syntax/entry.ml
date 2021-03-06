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
module Ast = Ast

type t = { ds : Diagnostics.t; is_complete : bool }

exception CannotRecoverly of string * (Lexing.position * Lexing.position)

let rec entry ~sup ~ds parser : (Ast.t * t, Diagnostics.Elem.t) Result.t =
  let p_state = { ds; is_complete = false } in
  try
    let pos = Supplier.start_pos sup in
    let start = parser pos in
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
  | CannotRecoverly (msg, position) ->
      let span = Supplier.create_span_with_lex_loc sup position in
      let e = new Reasons.invalid_syntax ~msg in
      let elm = Diagnostics.Elem.error ~span e in
      Error elm

and succeed p_state (v : Ast.t) = (v, { p_state with is_complete = true })

and fail p_state sup inputneeded checkpoint =
  match checkpoint with
  | I.HandlingError env ->
      let positions = I.positions env in
      let recovery =
        match I.top env with
        | Some (I.Element (state, _, _, _)) ->
            let state_num = I.number state in
            let msg = Printf.sprintf "Internal state (%d)" state_num in
            raise (CannotRecoverly (msg, positions))
        | _ -> raise (CannotRecoverly ("stack is empty", positions))
      in
      recovery
  | _ -> failwith "[ICE]"

let from_entry ~sup ~ds : (Ast.t * t, Diagnostics.Elem.t) Result.t =
  entry ~sup ~ds Parser.Incremental.program_entry

let from_expr ~sup ~ds : (Ast.t * t, Diagnostics.Elem.t) Result.t =
  entry ~sup ~ds Parser.Incremental.expr_entry
