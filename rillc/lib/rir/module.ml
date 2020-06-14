(*
 * Copyright yutopp 2018 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

(* TODO: fix *)
type t = {
  ctx : (Context.t[@printer fun fmt _ -> fprintf fmt ""]);
  module_name : string;
  mutable funcs_rev : func_assoc_t list;
  mutable types_rev : type_assoc_t list;
}

and func_assoc_t = string * Func.t

and type_assoc_t = {
  ty_name : string;
  ty_struct_tag : Typing.Type.struct_tag_t;
  ty_rir : Type.t;
}
[@@deriving show]

let create ~ctx : t = { ctx; module_name = ""; funcs_rev = []; types_rev = [] }

let append_func m name f = m.funcs_rev <- (name, f) :: m.funcs_rev

let append_type m name struct_tag ty_rir =
  let assoc = { ty_name = name; ty_struct_tag = struct_tag; ty_rir } in
  m.types_rev <- assoc :: m.types_rev

let funcs m : func_assoc_t list = List.rev m.funcs_rev

let types m : type_assoc_t list = List.rev m.types_rev

let to_string m =
  let indent = 0 in
  let buf = Buffer.create 256 in
  Buffer.add_string buf (Printf.sprintf "Module: name=%s\n" m.module_name);

  List.iter (types m) ~f:(fun t ->
      let { ty_name = name; ty_rir; _ } = t in
      let s = Type.to_string ~indent:(indent + 2) name ty_rir in
      Buffer.add_string buf s);

  List.iter (funcs m) ~f:(fun (name, func) ->
      let s = Func.to_string ~indent:(indent + 2) name func in
      Buffer.add_string buf s);

  Buffer.contents buf
