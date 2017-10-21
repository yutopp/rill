(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

type t =
  | OnlyMeta
  | Meta
  | Runtime
  | OnlyRuntime

let to_int = function
  | OnlyMeta    -> 0
  | Meta        -> 1
  | Runtime     -> 2
  | OnlyRuntime -> 3

let of_int = function
  | 0 -> OnlyMeta
  | 1 -> Meta
  | 2 -> Runtime
  | 3 -> OnlyRuntime
  | _ -> failwith "invalid"

let meta_than_or_equal_to a b =
  (to_int a) <= (to_int b)

(* returns ml near to runtime *)
let bottom a b =
  of_int (max (to_int a) (to_int b))

let calc_bottom = function
  | [] -> Meta
  | xs -> List.fold_left bottom OnlyMeta xs

let to_string = function
  | OnlyMeta    -> "OnlyMeta"
  | Meta        -> "Meta"
  | Runtime     -> "Runtime"
  | OnlyRuntime -> "OnlyRuntime"

let is_convertiable_to src trg =
  match (src, trg) with
  | (OnlyMeta, OnlyMeta)
  | (OnlyRuntime, OnlyRuntime) -> true
  | (OnlyMeta, _)
  | (_, OnlyMeta)
  | (OnlyRuntime, _)
  | (_, OnlyRuntime) -> false
  | _ -> (to_int src) <= (to_int trg)

let has_meta_spec ml =
  match ml with
  | OnlyMeta | Meta -> true
  | _ -> false
