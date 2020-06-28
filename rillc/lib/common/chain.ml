(*
 * Copyright yutopp 2019 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

module Nest = struct
  type t = nest_t list

  and nest_t = { name : string; kind : kind_t }

  and kind_t = Module | Type | Var [@@deriving sexp_of, yojson_of, show]

  let create () = []

  let join_rev nest n = n :: nest

  let to_unique_id nest =
    let to_s n =
      let kind_s =
        match n.kind with Module -> "mod" | Type -> "ty" | Var -> "var"
      in
      Printf.sprintf "%s(%s)" n.name kind_s
    in
    nest |> List.map ~f:to_s |> String.concat ~sep:"-"
end

type t = Local of string | Global of Nest.t
[@@deriving sexp_of, yojson_of, show]
