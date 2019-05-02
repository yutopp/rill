(*
 * Copyright yutopp 2018 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

type t = {
  tysc: Type.Scheme.t;
  params: Term.placeholder_t list;
  mutable ret_var: t option;
  mutable bbs: (string, Term.BB.t) Hashtbl.t;
}
[@@deriving sexp_of]

let create ~tysc =
  {
    tysc;
    params = [];
    ret_var = None;
    bbs = Hashtbl.create (module String);
  }

let update_tysc f tysc =
  {f with tysc}

let insert_bb f bb =
  Hashtbl.add_exn f.bbs ~key:bb.Term.BB.name ~data:bb

let get_entry_bb f =
  Hashtbl.find_exn f.bbs "entry"
