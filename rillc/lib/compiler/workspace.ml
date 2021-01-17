(*
 * Copyright yutopp 2021 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

module VersionDict = struct
  type t = (string, Package_handle.t) Hashtbl.t

  let empty () : t = Hashtbl.create (module String)

  let register dict ph =
    let version =
      let tag = Package_handle.tag ph in
      Group.Pkg_tag.version tag
    in
    Hashtbl.add_exn dict ~key:version ~data:ph

  let find dict ~tag =
    let version = Group.Pkg_tag.version tag in
    Hashtbl.find dict version
end

module PackageDict = struct
  type t = (string, VersionDict.t) Hashtbl.t

  let empty () : t = Hashtbl.create (module String)

  let register dict ph =
    let name =
      let tag = Package_handle.tag ph in
      Group.Pkg_tag.name tag
    in
    let version_dict =
      Hashtbl.find_or_add dict name ~default:VersionDict.empty
    in
    VersionDict.register version_dict ph

  let find dict ~tag =
    let name = Group.Pkg_tag.name tag in
    Hashtbl.find dict name |> Option.bind ~f:(VersionDict.find ~tag)
end

type t = {
  dict : PackageDict.t;
  builtin : Sema.Builtin.t;
  substs : Typing.Subst.bag_t;
}

let create () : t =
  let builtin = Sema.Builtin.create () in
  { dict = PackageDict.empty (); builtin; substs = Typing.Subst_bag.create () }

let builtin ws = ws.builtin

let create_subst ws = Typing.Subst_bag.factory ws.substs ~f:Typing.Subst.create2

let register_pkg ws ~pkg_handle = PackageDict.register ws.dict pkg_handle

let new_pkg ws ~pkg_struct =
  let subst = create_subst ws in
  let ph = Package_handle.create ~structure:pkg_struct ~subst in
  register_pkg ws ~pkg_handle:ph;
  ph

let find_pkg_handle ws ~tag = PackageDict.find ws.dict ~tag
