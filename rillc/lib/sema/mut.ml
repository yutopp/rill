(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module Ast = Syntax.Ast

let mutability_of attr =
  match attr with
  | Ast.{ kind = DeclAttrImmutable; _ } -> Typing.Type.MutImm
  | Ast.{ kind = DeclAttrMutable; _ } -> Typing.Type.MutMut
  | _ -> failwith "[ICE]"
