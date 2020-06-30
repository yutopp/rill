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
  ds : (Diagnostics.t[@printer fun fmt _ -> fprintf fmt ""]);
  mutable subst : (Typing.Subst.t[@printer fun fmt _ -> fprintf fmt ""]);
  pkg : Package.t;
}
[@@deriving show]

let create ~path ~subst ~pkg =
  let ds = Diagnostics.create () in
  { path; ds; subst; pkg }

let has_errors m = match Diagnostics.errors m.ds with [] -> false | _ -> true

let has_warnings m =
  match Diagnostics.warnings m.ds with [] -> false | _ -> true

let subst_of m = m.subst
