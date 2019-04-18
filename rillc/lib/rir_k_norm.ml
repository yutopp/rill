(*
 * Copyright yutopp 2018 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

module Span = Common.Span
module Diagnostics = Common.Diagnostics

type t = {
  kind: kind_t;
  ty: Hir.Ty.t;
  span: Span.t;
}

and kind_t =
  | Module of {nodes: t list}
  | FuncDef of {name: string; params: t list; body: t}
  | Let of {name: string; expr: t; body: t}
  | Seq of t list
  | Call of {name: string; args: string list}
  | Var of string
  | LitString of string
  | LitUnit
  | Undef
[@@deriving sexp_of]

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
     let new_expr = k new_id in
     {
       kind = Let {name = new_id; expr = k_form; body = new_expr};
       ty = Hir.Ty.Unit;
       span
     }

let rec generate tnode : (t, Diagnostics.t) Result.t =
  match tnode with
  | Hir.{kind = Module nodes; ty; span} ->
     let ctx = () in
     let nodes' = List.map nodes ~f:(generate' ctx) in
     Ok ({kind = Module {nodes = nodes'}; ty; span})

  | k ->
     failwith @@
       Printf.sprintf "Unknown node: %s"
                      (k |> Hir.sexp_of_t |> Sexp.to_string_hum ~indent:2)

and generate' ctx node : t =
  match node with
  | Hir.{kind = FunctionDefStmt {name; body}; ty; span} ->
     let expr = generate_expr ctx body in
     {kind = FuncDef {name; params = []; body = expr}; ty; span}

  | Hir.{kind = ExternFunctionDeclStmt {name}; span; _} ->
     {kind = Undef; ty = Hir.Ty.Unknown; span}

  | Hir.{kind = Empty; span; _} ->
     {kind = Undef; ty = Hir.Ty.Unknown; span}

  | k ->
     failwith @@
       Printf.sprintf "Unknown node': %s"
                      (k |> Hir.sexp_of_t |> Sexp.to_string_hum ~indent:2)

and generate_expr ctx node : t =
  match node with
  | Hir.{kind = StmtExpr expr; ty; span} ->
     let k = insert_let (generate_expr ctx expr) in
     k (fun _id -> {kind = LitUnit; ty; span})

  | Hir.{kind = ExprCompound exprs; ty; span} ->
     {kind = Seq (exprs |> List.map ~f:(generate_expr ctx)); ty; span}

  | Hir.{kind = ExprCall (r, args); ty; span} ->
     let rk = insert_let (generate_expr ctx r) in
     let rec bind xs args =
       match args with
       | [] ->
          rk (fun r' -> {kind = Call {name = r'; args = List.rev xs}; ty; span})
       | a :: args ->
          let k = insert_let (generate_expr ctx a) in
          k (fun k -> bind (k :: xs) args)
     in
     bind [] args

  | Hir.{kind = ID s; ty; span} ->
     {kind = Var s; ty; span}

  | Hir.{kind = LitString s; ty; span} ->
     {kind = LitString s; ty; span}

  | k ->
     failwith @@
       Printf.sprintf "Unknown node_expr: %s"
                      (k |> Hir.sexp_of_t |> Sexp.to_string_hum ~indent:2)
