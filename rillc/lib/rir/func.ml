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
  bbs: (string, Term.BB.t) Hashtbl.t;
  extern_name: string option;
}


[@@deriving sexp_of]

let create ?(extern_name=None) ~tysc =
  {
    tysc;
    params = [];
    bbs = Hashtbl.create (module String);
    extern_name;
  }

let update_tysc f tysc =
  {f with tysc}

let insert_bb f bb =
  Hashtbl.add_exn f.bbs ~key:bb.Term.BB.name ~data:bb

let get_entry_bb f =
  Hashtbl.find_exn f.bbs "entry"
