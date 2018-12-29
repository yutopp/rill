(*
 * Copyright yutopp 2018 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

module Ty = struct
  type t =
    | Module
    | Function
    | String
    | Unit
    | Unknown
  [@@deriving sexp]
end

type t = {
  kind: kind_t;
  ty: Ty.t;
  span: Span.t;
}
[@@deriving sexp]

and kind_t =
  | Module of t list
  | ExternFunctionDeclStmt of {
    name: string;
  }
  | FunctionDefStmt of {
    name: string;
    body: t;
  }
  | StmtExpr of t
  | ExprCompound of t list
  | ExprCall of t * t list
  | LitString of string
  | ID of string
  | Empty
