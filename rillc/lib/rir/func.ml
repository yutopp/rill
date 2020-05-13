(*
 * Copyright yutopp 2018 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

module BBs = struct
  type t = (string, Term.BB.t) Hashtbl.t

  let pp ppf values =
    Hashtbl.iteri values ~f:(fun ~key ~data ->
        Caml.Format.fprintf ppf "@[<1>%s: %s@]@." key (Term.BB.show data))
end

type t = {
  ty : (Typing.Type.t[@printer fun fmt _ -> fprintf fmt ""]);
  bbs : BBs.t;
  extern_name : string option;
}
[@@deriving show]

let create ?(extern_name = None) ~ty =
  { ty; bbs = Hashtbl.create (module String); extern_name }

let insert_bb f bb = Hashtbl.add_exn f.bbs ~key:bb.Term.BB.name ~data:bb

let get_entry_bb f = Hashtbl.find_exn f.bbs "entry"

let get_func_ty f =
  match f.ty with
  | Typing.Type.{ ty = Func (params_tys, ret_ty); _ } -> (params_tys, ret_ty)
  | _ -> failwith "[ICE]"

let get_ret_ty f =
  let (_, ret_ty) = get_func_ty f in
  ret_ty
