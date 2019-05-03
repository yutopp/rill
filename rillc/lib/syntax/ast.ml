(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

module Span = Common.Span

type t = {
  kind: kind;
  span: Span.t sexp_opaque;
}
[@@deriving sexp]

and kind =
  | Module of t list
  | FunctionDeclStmt of {
    name: string;
    ret_ty: t option;
    params: t list;
  }
  | FunctionDefStmt of {
    name: string;
    ret_ty: t option;
    params: t list;
    body: t;
  }
  | ParamDecl of {
    name: string;
    ty_spec: t;
  }
  | ExternFunctionDeclStmt of {
    name: string;
    ret_ty: t option;
    params: t list;
    symbol_name: t;
  }
  | StmtExpr of t
  | StmtReturn of t option
  | ExprCompound of t list
  | ExprIf of t * t * t option
  | ExprBinaryOp of t * t * t
  | ExprCall of t * t list
  | ID of string
  | LitBool of bool
  | LitInt of int * int * bool (* value * bits * signed *)
  | LitString of string
[@@deriving sexp]
