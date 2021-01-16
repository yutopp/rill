(*
 * Copyright yutopp 2021 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

module T : sig
  type t [@@deriving sexp_of, yojson_of, ord, show]

  val create : owner:int -> fresh:Counter.Value.t -> t

  val owner : t -> int

  val to_string : t -> string
end = struct
  type t = int * Counter.Value.t [@@deriving sexp_of, ord, show]

  let create ~owner ~fresh = (owner, fresh)

  let owner v =
    let (a, b) = v in
    a

  (* TODO *)
  let yojson_of_t v : Yojson.Safe.t = `Int 10

  let to_string v =
    let (a, b) = v in
    Printf.sprintf "%d.%d" a b
end

include T
include Comparable.Make (T)
