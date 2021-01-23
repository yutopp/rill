(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module Package = Common.Package

type t = {
  tag : Group.Mod_tag.t;
  menv : Env.t;
  mutable subst : (Typing.Subst.t option[@printer fun fmt _ -> fprintf fmt ""]);
  ds : (Diagnostics.t[@printer fun fmt _ -> fprintf fmt ""]);
}
[@@deriving show]

let create ~tag ~menv =
  let ds = Diagnostics.create () in
  { tag; menv; subst = None; ds }

let tag m = m.tag

let path m = Group.Mod_tag.path m.tag

let menv m = m.menv

let has_errors m = match Diagnostics.errors m.ds with [] -> false | _ -> true

let has_warnings m =
  match Diagnostics.warnings m.ds with [] -> false | _ -> true

let subst_of m =
  match m.subst with
  | Some s -> s
  | None -> failwith (Printf.sprintf "[ICE] has no subst: path = %s" (path m))

let set_latest_subst m subst = m.subst <- Some subst

let ds m = m.ds
