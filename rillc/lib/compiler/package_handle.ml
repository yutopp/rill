(*
 * Copyright yutopp 2021 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module Mod = Sema.Mod

module Mods_dict = struct
  type t = { rel : (string, Mod_handle.t) Hashtbl.t }

  let create () = { rel = Hashtbl.create (module String) }

  let add_mod_handle dict ~mh =
    let name = Mod_handle.inner mh |> Mod.path in
    Hashtbl.set dict.rel ~key:name ~data:mh

  let mod_handles dict =
    Hashtbl.to_alist dict.rel |> List.map ~f:(fun (_, v) -> v)
end

type t = {
  structure : Structure.t;
  mutable subst : Typing.Subst.t;
  mutable root_mod : Mod.t option;
  dict : Mods_dict.t;
}

let create ~structure ~subst : t =
  { structure; subst; root_mod = None; dict = Mods_dict.create () }

let structure handle = handle.structure

let subst handle = handle.subst

let update_subst handle ~subst = handle.subst <- subst

let root_mod handle = Option.value_exn handle.root_mod

let tag handle = Structure.tag (structure handle)

let name_of handle =
  let tag = tag handle in
  Group.Pkg_tag.name tag

let set_root_mod handle ~root_mod = handle.root_mod <- Some root_mod

let add_mod_handle handle ~mh = Mods_dict.add_mod_handle handle.dict ~mh

let mod_handles handle = Mods_dict.mod_handles handle.dict
