(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

type t =
    Pure of string
  | UnaryPreOp of string
  | UnaryPostOp of string
  | BinaryOp of string

let to_string id_s =
  match id_s with
  | Pure s -> s
  | UnaryPreOp s -> "op_unary_pre_" ^ s
  | UnaryPostOp s -> "op_unary_post_" ^ s
  | BinaryOp s -> "op_binary_" ^ s
