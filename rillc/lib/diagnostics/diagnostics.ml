(*
 * Copyright yutopp 2018 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

(* exports *)
module Elem = Elem
module Unit = Unit
module Error_info = Error_info
module Reasons = Reasons

type t = { mutable elems_rev : Elem.t list }

let create () : t = { elems_rev = [] }

let append ds elem = ds.elems_rev <- elem :: ds.elems_rev

let append_all ds ds' = ds.elems_rev <- List.append ds'.elems_rev ds.elems_rev

let iter ds ~f = List.rev ds.elems_rev |> List.iter ~f

let errors ds = List.rev ds.elems_rev

let warnings ds = []
