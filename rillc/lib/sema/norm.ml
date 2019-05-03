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

module Ast = Syntax.Ast

module NAst = struct
  type t = {
    kind: kind_t;
    span: Span.t sexp_opaque;
  }

  and kind_t =
    | Func of func_kind_t
    | Let of {name: string; expr: t; body: t}
    | Seq of t list
    | Call of {name: string; args: string list}
    | LitBool of bool
    | LitInt of {value: int; bits: int; signed: bool}
    | LitString of string
    | LitUnit
    | ID of string

  and func_kind_t =
    | FuncKindDecl
    | FuncKindDef of t
    | FuncKindExtern of t
  [@@deriving sexp_of]
end

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
  | NAst.{kind = ID id; _} ->
     k id
  | NAst.{span; _} ->
     let new_id = fresh_id () in
     let new_expr = k new_id in
     NAst.{
       kind = Let {name = new_id; expr = k_form; body = new_expr};
       span
     }

(* Currently K-normalize *)
let rec normalize ast =
  match ast with
  (* toplevels *)
  | Ast.{kind = FunctionDeclStmt _; span; _} ->
     NAst.{kind = Func (FuncKindDecl); span}

  | Ast.{kind = FunctionDefStmt {body; _}; span} ->
     let body' = normalize body in
     NAst.{kind = Func (FuncKindDef body'); span}

  | Ast.{kind = ExternFunctionDeclStmt {symbol_name; _}; span; _} ->
     let symbol_name' = normalize symbol_name in
     NAst.{kind = Func (FuncKindExtern symbol_name'); span}

  (* others *)
  | Ast.{kind = StmtExpr expr; span; _} ->
     let k = insert_let (normalize expr) in
     k (fun _id -> NAst.{kind = LitUnit; span})

  | Ast.{kind = ExprCompound exprs; span; _} ->
     (* TODO: consider scopes *)
     NAst.{kind = Seq (exprs |> List.map ~f:normalize); span}

  | Ast.{kind = ExprBinaryOp (lhs, op, rhs); span; _} ->
     (* TODO: reconstruction *)
     let k = insert_let (normalize op) in
     k (fun op_id ->
         let k = insert_let (normalize lhs) in
         k (fun lhs_id ->
             let k = insert_let (normalize rhs) in
             k (fun rhs_id ->
                 NAst.{kind = Call {name = op_id; args = [lhs_id; rhs_id]}; span}
               )
           )
       )

  | Ast.{kind = ExprCall (r, args); span; _} ->
     let rk = insert_let (normalize r) in
     let rec bind xs args =
       match args with
       | [] ->
          rk (fun r' -> NAst.{kind = Call {name = r'; args = List.rev xs}; span})
       | a :: args ->
          let k = insert_let (normalize a) in
          k (fun k -> bind (k :: xs) args)
     in
     bind [] args

  | Ast.{kind = ID s; span; _} ->
     NAst.{kind = ID s; span}

  | Ast.{kind = LitBool v; span; _} ->
     NAst.{kind = LitBool v; span}

  | Ast.{kind = LitInt (value, bits, signed); span; _} ->
     NAst.{kind = LitInt {value; bits; signed}; span}

  | Ast.{kind = LitString v; span; _} ->
     NAst.{kind = LitString v; span}

  | k ->
     failwith @@
       Printf.sprintf "Unknown node': %s"
                      (k |> Ast.sexp_of_t |> Sexp.to_string_hum ~indent:2)

let normalize (ast, env) =
  let n = normalize ast in
  Stdio.printf "K form = \n%s\n" (NAst.sexp_of_t n |> Sexp.to_string_hum ~indent:2);
  (n, env)

let transform m =
  Module.map ~f:normalize m
