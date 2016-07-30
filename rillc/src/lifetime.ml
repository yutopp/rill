(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

type t =
  | LtStatic
  | LtHeap
  (* env_id * nest_levet * (aux counter) *)
  | LtDynamic of Env_system.EnvId.t * Env_system.NestLevel.t * int
  | LtUndef
