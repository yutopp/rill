(*
 * Copyright yutopp 2019 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

type 'a t = {
  env: Env.t;
  toplevels: (string, 'a) Hashtbl.t;
  typer: Typer.t
}

let create menv =
  {
    env = menv;
    toplevels = Hashtbl.create (module String);
    typer = Typer.create ();
  }

let insert m name body =
  (* TODO: check duplication *)
  let _ = Hashtbl.add m.toplevels ~key:name ~data:body in
  ()

let iteri ~f m =
  Hashtbl.iteri ~f m.toplevels

let map ~f m =
  let toplevels = Hashtbl.map ~f m.toplevels in
  {m with toplevels}
