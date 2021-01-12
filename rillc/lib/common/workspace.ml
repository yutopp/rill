(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

type t = {
  dir : string;
  host_triple : Triple.tag_t;
  target_triple : Triple.tag_t;
  pkgs : (Package.id_t, Package.t) Hashtbl.t;
  counter : Counter.t;
}

let create ~dir ~host_triple ~target_triple : t =
  {
    dir;
    host_triple;
    target_triple;
    pkgs = Hashtbl.create (module Int);
    counter = Counter.create ();
  }

let host ~workspace = Triple.to_triple_preset workspace.host_triple

let target ~workspace = Triple.to_triple_preset workspace.target_triple

let issue_pkg_id ~workspace : Package.id_t = Counter.fresh workspace.counter

let register_pkg ~workspace pkg : unit =
  Hashtbl.add_exn workspace.pkgs ~key:pkg.Package.id ~data:pkg
