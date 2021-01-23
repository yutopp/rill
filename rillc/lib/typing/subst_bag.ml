(*
 * Copyright yutopp 2021 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module Counter = Common.Counter

type 'a t = {
  fresh_counter : Counter.t;
  refs : (Counter.Value.t, 'a) Hashtbl.t;
}

let create () : 'a t =
  {
    fresh_counter = Counter.create ();
    refs = Hashtbl.create (module Counter.Value);
  }

let factory bag ~f =
  let v = Counter.fresh bag.fresh_counter in
  let inner = f ~bag ~id:v in
  Hashtbl.add_exn bag.refs ~key:v ~data:inner;
  inner

let find_subst bag ~owner_id = Hashtbl.find_exn bag.refs owner_id
