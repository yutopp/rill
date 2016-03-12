(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

(* used for id of environments *)
module EnvId = Int32
let id_counter = ref (EnvId.of_int 1)
let undef_id = (EnvId.of_int 0)

let generate_new_env_id () =
  let cur_id = !id_counter in
  if cur_id = EnvId.max_int then
    failwith "env id is reached to limit";

  id_counter := EnvId.add !id_counter EnvId.one;
  cur_id


module NestLevel = Int32
