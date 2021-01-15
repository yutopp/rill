(*
 * Copyright yutopp 2021 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

module T = struct
  type t = int * int [@@deriving sexp_of, ord, yojson_of, show]

  let to_string v =
    let (a, b) = v in
    Printf.sprintf "%d.%d" a b
end

include T
include Comparable.Make (T)
