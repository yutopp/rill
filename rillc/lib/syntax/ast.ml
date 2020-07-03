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
  (* aux *)
  | ParamDecl of { attr : t; name : string; ty_spec : t }
  | TyParamDecl of { name : string }
  | VarDecl of { attr : t; name : string; ty_spec : t option; expr : t }
  (* top levels *)
  | Import of { pkg : t; mods : t list }
  | DeclExternFunc of {
      name : string;
      ty_params : t list;
      params : t list;
      ret_ty : t;
      symbol_name : t;
    }
  | DeclExternStaticVar of { attr : t; name : string; ty_spec : t }
  | DefFunc of {
      name : string;
      ty_params : t list;
      params : t list;
      ret_ty : t;
      body : t;
    }
  | DefStruct of { name : string }
  | DefTrait of { name : string }
  | DefImplFor of { name : string; for_ty : t }
  | DefTypeAlias of { name : string; alias_ty : t }
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
  | ExprAs of { expr : t; ty_expr : t }
  | ExprAssign of { lhs : t; rhs : t }
  | ExprBinaryOp of { op : t; lhs : t; rhs : t }
  | ExprCall of t * t list
  | ExprIndex of t * t
  | ExprRef of t * t
  | ExprDeref of t
  | ExprStruct of { path : t (* add fields *) }
  (* primitives *)
  | Path of { root : t; elems : t list }
  | ID of string
  | IDWildcard
  | LitBool of bool
  | LitInt of int * int * bool (* value * bits * signed *)
  | LitString of string
  | LitUnit
  | LitArrayElems of t list
  | DeclAttrMutable
  | DeclAttrImmutable
  (* meta *)
  | TypeExprArray of { elem : t; len : t }
  | TypeExprPointer of { attr : t; elem : t }
[@@deriving sexp_of, show]

let param_decl_name ast =
  match ast with
  | { kind = ParamDecl { name; _ }; _ } -> name
  | _ -> failwith ""
