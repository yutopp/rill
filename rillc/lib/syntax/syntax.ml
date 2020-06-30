(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module Span = Common.Span

(* exports *)
module Ast = Entry.Ast
module Supplier = Supplier
module Entry = Entry

type state_t = Complete | Incomplete

let parse_from_file ~ds path : (state_t * Ast.t, Diagnostics.Elem.t) Result.t =
  let f path chan =
    let open Result.Let_syntax in
    let lexbuf = chan |> Lexing.from_channel in
    let sup = Supplier.create ~path ~lexbuf in
    let%bind (node, p_state) = Entry.from_entry ~sup ~ds in
    let state = if p_state.Entry.is_complete then Complete else Incomplete in
    Ok (state, node)
  in
  try Stdio.In_channel.with_file ~binary:true path ~f:(f path)
  with exn ->
    let span = Span.create_path ~path in
    let e = new Diagnostics.Reasons.internal_exception ~e:exn in
    let elm = Diagnostics.Elem.error ~span e in
    Error elm
