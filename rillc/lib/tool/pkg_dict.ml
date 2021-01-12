(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module Package = Common.Package

type t = { rel : (Package.id_t, Package.t * Mod_dict.t) Hashtbl.t }

let create () = { rel = Hashtbl.create (module Int) }

let update d ~key ~data : unit =
  Hashtbl.add_exn d.rel ~key:key.Package.id ~data:(key, data)

let get d ~key =
  let (_, mod_dict) = Hashtbl.find_exn d.rel key.Package.id in
  mod_dict

let to_alist d = Hashtbl.to_alist d.rel |> List.map ~f:snd
