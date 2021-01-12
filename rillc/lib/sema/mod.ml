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
  path : string;
  menv : Env.t;
  mutable subst : (Typing.Subst.t option[@printer fun fmt _ -> fprintf fmt ""]);
  pkg : Package.t;
  ds : (Diagnostics.t[@printer fun fmt _ -> fprintf fmt ""]);
}
[@@deriving show]

let create ~path ~menv ~pkg =
  let ds = Diagnostics.create () in
  { path; menv; subst = None; pkg; ds }

let has_errors m = match Diagnostics.errors m.ds with [] -> false | _ -> true

let has_warnings m =
  match Diagnostics.warnings m.ds with [] -> false | _ -> true

let subst_of m =
  match m.subst with
  | Some s -> s
  | None -> failwith (Printf.sprintf "[ICE] has no subst: path = %s" m.path)

let set_latest_subst m subst = m.subst <- Some subst
