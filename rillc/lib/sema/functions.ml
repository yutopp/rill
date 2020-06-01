(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module Ast = Syntax.Ast

let linkage_of ast =
  match ast with
  | Ast.{ kind = DeclExternFunc { name; params; ret_ty; symbol_name }; span } ->
      let linkage =
        match symbol_name with
        | Ast.{ kind = LitString s; _ } -> Typing.Type.LinkageC s
        | _ -> failwith "[ICE] unexpected token"
      in
      linkage
  | Ast.{ kind = DeclFunc { name; params; ret_ty; _ }; span }
  | Ast.{ kind = DefFunc { name; params; ret_ty; _ }; span } ->
      let linkage = Typing.Type.LinkageRillc in
      linkage
  | _ -> failwith "[ICE] unexpected node"
