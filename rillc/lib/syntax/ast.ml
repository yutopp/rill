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
  kind : kind_t;
  span : (Span.t[@sexp.opaque] [@printer fun fmt _ -> fprintf fmt ""]);
}

and kind_t =
  | Module of t list
  | ParamDecl of { name : string; ty_spec : t }
  | VarDecl of { attr : t; name : string; ty_spec : t option; expr : t }
  (* *)
  | Import of { pkg : t; mods : t list }
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
  | DefStruct of { name : string }
  (* statements *)
  | StmtExpr of t
  | StmtExprApply of t
  | StmtLet of t
  | StmtReturn of t option
  (* expressions *)
  | ExprGrouping of t
  | ExprBlock of t list
  | ExprIf of t * t * t option
  | ExprLoop of t
  | ExprBreak
  | ExprAssign of { lhs : t; rhs : t }
  | ExprBinaryOp of { op : t; lhs : t; rhs : t }
  | ExprCall of t * t list
  | ExprIndex of t * t
  | ExprRef of t * t
  | ExprDeref of t
  | ExprStruct of { path : t (* add fields *) }
  (* primitives *)
  | ID of string
  | IDWildcard
  | LitBool of bool
  | LitInt of int * int * bool (* value * bits * signed *)
  | LitString of string
  | LitUnit
  | LitArrayElems of t list
  | DeclAttrMutable
  | DeclAttrImmutable
[@@deriving sexp_of, show]

let param_decl_name ast =
  match ast with
  | { kind = ParamDecl { name; _ }; _ } -> name
  | _ -> failwith ""
