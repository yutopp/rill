(*
 * Copyright yutopp 2018 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

type t = {
  kind: kind_t;
  ty: Hir.Ty.t;
  span: Span.t;
}
[@@deriving sexp]

and kind_t =
  | Module of {nodes: t list}
  | FuncDecl of {name: string; params: t list; body: t}
  | Seq of {nodes: t list}
  | Let of {name: string; expr: t}
  | Call of {name: string; args: string list}
  | Var of string
  | Undef
[@@deriving sexp]

let fresh_id =
  let i = ref 0 in
  let f () =
    let v = !i in
    i := v + 1;
    Printf.sprintf "%d" v
  in
  f

let insert_let k_form k =
  match k_form with
  | {kind = Var id; _} ->
     k id
  | {span; _} ->
     let new_id = fresh_id () in
     let let_stmt = {
       kind = Let {name = new_id; expr = k_form};
       ty = Hir.Ty.Unit;
       span
     } in
     match k new_id with
     | {kind = Seq {nodes}; ty; span} ->
        {kind = Seq {nodes = let_stmt :: nodes}; ty; span}
     | {ty; span; _} as node ->
        {kind = Seq {nodes = [let_stmt; node]}; ty; span}

let rec generate node =
  match node.Hir.kind with
  | Hir.Module nodes ->
     List.iter nodes ~f:generate

  | _ ->
     failwith ""
