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

module LocalVars = struct
  type t = { vars : (string, unit) Hashtbl.t; mutable auto_gen : int }

  let create () : t = { vars = Hashtbl.create (module String); auto_gen = 0 }

  let fresh_id vars =
    let id = vars.auto_gen in
    vars.auto_gen <- vars.auto_gen + 1;
    Printf.sprintf "$%d" id
end

type t = {
  ty : (Typing.Type.t[@printer fun fmt _ -> fprintf fmt ""]);
  bbs : BBs.t;
  mutable bbs_names_rev : string list;
  local_vars : (LocalVars.t[@printer fun fmt _ -> fprintf fmt ""]);
  extern_name : string option;
}
[@@deriving show]

let create ?(extern_name = None) ~ty =
  {
    ty;
    bbs = Hashtbl.create (module String);
    bbs_names_rev = [];
    local_vars = LocalVars.create ();
    extern_name;
  }

let insert_bb f bb =
  let name = bb.Term.BB.name in
  Hashtbl.add_exn f.bbs ~key:name ~data:bb;
  f.bbs_names_rev <- name :: f.bbs_names_rev;
  ()

let list_bbs f =
  List.rev f.bbs_names_rev
  |> List.map ~f:(fun bb_name -> Hashtbl.find_exn f.bbs bb_name)

let get_entry_bb f = Hashtbl.find_exn f.bbs "entry"

let get_func_ty f =
  match f.ty with
  | Typing.Type.{ ty = Func (params_tys, ret_ty); _ } -> (params_tys, ret_ty)
  | _ -> failwith "[ICE]"

let get_ret_ty f =
  let (_, ret_ty) = get_func_ty f in
  ret_ty

let gen_local_var f = LocalVars.fresh_id f.local_vars
