(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module Span = Common.Span

type t = { kind : kind_t; span : (Span.t[@sexp.opaque]) }

and kind_t =
  | Module of t list
  | ParamDecl of { name : string; ty_spec : t }
  (* top declarations *)
  | DeclExternFunc of {
      name : string;
      ret_ty : t;
      params : t list;
      symbol_name : t;
    }
  | DeclFunc of { name : string; ret_ty : t; params : t list }
  (* top definitions *)
  | DefFunc of { name : string; ret_ty : t; params : t list; body : t }
  (* top statements *)
  | StmtExpr of t
  | StmtReturn of t option
  (* expressions *)
  | ExprCompound of t list (* TODO: rename *)
  | ExprIf of t * t * t option
  | ExprBinaryOp of t * t * t
  | ExprCall of t * t list
  (* primitives *)
  | ID of string
  | LitBool of bool
  | LitInt of int * int * bool (* value * bits * signed *)
  | LitString of string
  | LitUnit
[@@deriving sexp_of]

let param_decl_name ast =
  match ast with
  | { kind = ParamDecl { name; _ }; _ } -> name
  | _ -> failwith ""
