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
  param_names: Term.placeholder_t list;
  bbs: (string, Term.BB.t) Hashtbl.t;
  extern_name: string option;
}


[@@deriving sexp_of]

let create ?(extern_name=None) ~tysc ~param_names =
  {
    tysc;
    param_names;
    bbs = Hashtbl.create (module String);
    extern_name;
  }

let update_tysc f tysc =
  {f with tysc}

let insert_bb f bb =
  Hashtbl.add_exn f.bbs ~key:bb.Term.BB.name ~data:bb

let get_entry_bb f =
  Hashtbl.find_exn f.bbs "entry"

let get_func_ty f =
  let Type.Scheme.Scheme (_, ty) = f.tysc in
  match ty with
  | Type.Func (params_tys, ret_ty) -> (params_tys, ret_ty)
  | _ -> failwith "[ICE]"

let get_ret_ty f =
  let (_, ret_ty) = get_func_ty f in
  ret_ty
