(*
 * Copyright yutopp 2019 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

type var_t = int
[@@deriving sexp]

module Primitive = struct
  type t =
    | Var of var_t
    | Unit
    | Int
    | String
    | Func of t list * t
    | Bottom
  [@@deriving sexp]
end
include Primitive

module Scheme = struct
  type t =
    | Scheme of var_t list * Primitive.t
  [@@deriving sexp]

  let of_ty ty =
    Scheme ([], ty)
end

(* for debbuging. TODO: remove *)
let print ty =
  Stdio.printf "ty = %s\n"
               (sexp_of_t ty |> Sexp.to_string_hum)
