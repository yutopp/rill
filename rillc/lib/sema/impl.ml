(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

type t = {
  trait_name : Name.t;
  for_ty : Typing.Type.t;
  mapping : (Typing.Type.t Common.Chain.Layer.t * Name.t) list;
}
[@@deriving show]
