(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module Mod = Sema.Mod

type t = { root_mod_env : Sema.Env.t; rel : (string, Mod_state.t) Hashtbl.t }

let create root_mod_env = { root_mod_env; rel = Hashtbl.create (module String) }

let update dict ms =
  let Mod_state.{ m; _ } = ms in
  let key = m.Mod.path in
  Hashtbl.set dict.rel ~key ~data:ms

let to_alist d = Hashtbl.to_alist d.rel
