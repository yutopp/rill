(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

(* used for id of environments *)
module EnvUniqId = Generic_counter.Counter(Int32)

module EnvId =
  struct
    type t = E of EnvUniqId.t * t option

    let undef = E (EnvUniqId.undef, None)

    let compare a b =
      match (a, b) with
      | (E (u_a, _), E (u_b, _)) -> EnvUniqId.(compare u_a u_b)

    let to_string v =
      match v with
      | E (u_v, _) -> EnvUniqId.to_string u_v
  end

module NestLevel = Int32
