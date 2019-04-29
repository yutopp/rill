(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

module Span = Common.Span
module Diagnostics = Common.Diagnostics

(* exports *)
module Ast = Entry.Ast

type t =
  | Complete of Ast.t
  | Incomplete of Ast.t
  | Failed

let parse_from_file dm path : t * Diagnostics.Multi.t =
  let f path chan =
    let lexbuf = chan |> Lexing.from_channel in
    let sup = Supplier.create ~path ~lexbuf in
    let (node_opt, is_incomplete, dm) = Entry.entry dm sup in
    let result = match node_opt with
      | Some node when is_incomplete ->
         Incomplete node
      | Some node ->
         Complete node
      | None ->
         Failed
    in
    (result, dm)
  in
  try Stdio.In_channel.with_file ~binary:true path ~f:(f path) with
  | e ->
     let span = Span.create_path ~path in
     let reason = Diagnostics.InternalException e in
     let d = Diagnostics.create ~reason ~span ~phase:Diagnostics.PhaseParsing in
     (Failed, Diagnostics.Multi.append dm d)
