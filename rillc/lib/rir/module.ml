(*
 * Copyright yutopp 2018 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module StringMap = Map.M (String)

(* TODO: fix *)
type t = {
  ctx : (Context.t[@printer fun fmt _ -> fprintf fmt ""]);
  module_name : string;
  mutable funcs : funcs_t;
  mutable types_rev : type_assoc_t list;
}

and funcs_t = {
  funcs_rev : Func.t list;
  funcs_map : (Func.t StringMap.t[@printer fun fmt _ -> fprintf fmt ""]);
}

and type_assoc_t = {
  ty_name : string;
  ty_struct_tag : Typing.Type.struct_tag_t;
  ty_rir : Type.t;
}
[@@deriving show]

let create ~ctx : t =
  let funcs = { funcs_rev = []; funcs_map = Map.empty (module String) } in
  { ctx; module_name = ""; funcs; types_rev = [] }

let declare_func m name ty =
  let { funcs_map; funcs_rev } = m.funcs in
  (* TODO: type checking *)
  let id = Common.Chain.Nest.to_unique_id name in

  let f = Func.create ~name ~ty in
  match Map.add funcs_map ~key:id ~data:f with
  | `Ok new_map ->
      let funcs_map = new_map in
      m.funcs <- { funcs_map; funcs_rev };
      f
  | `Duplicate -> Map.find_exn funcs_map id

let find_func m name =
  let { funcs_map; _ } = m.funcs in
  let id = Common.Chain.Nest.to_unique_id name in
  Map.find_exn funcs_map id

let mark_func_as_defined m f =
  let { funcs_rev; _ } = m.funcs in
  let funcs_rev = f :: funcs_rev in
  m.funcs <- { m.funcs with funcs_rev }

let append_type m name struct_tag ty_rir =
  let assoc = { ty_name = name; ty_struct_tag = struct_tag; ty_rir } in
  m.types_rev <- assoc :: m.types_rev

let all_funcs m : Func.t list =
  let { funcs_map; _ } = m.funcs in
  Map.data funcs_map

let defined_funcs m : Func.t list =
  let { funcs_rev; _ } = m.funcs in
  List.rev funcs_rev

let declared_funcs m : Func.t list =
  let to_ids fs =
    List.map fs ~f:(fun func -> Common.Chain.Nest.to_unique_id func.Func.name)
  in

  let defined = defined_funcs m |> to_ids |> Set.of_list (module String) in
  let entire = all_funcs m |> to_ids |> Set.of_list (module String) in
  let declared = Set.diff entire defined in

  let { funcs_map; _ } = m.funcs in
  Set.to_list declared |> List.map ~f:(fun k -> Map.find_exn funcs_map k)

let types m : type_assoc_t list = List.rev m.types_rev

let to_string m =
  let indent = 0 in
  let buf = Buffer.create 256 in
  Buffer.add_string buf (Printf.sprintf "Module: name=%s\n" m.module_name);

  List.iter (types m) ~f:(fun t ->
      let { ty_name = name; ty_rir; _ } = t in
      let s = Type.to_string ~indent:(indent + 2) name ty_rir in
      Buffer.add_string buf s);

  List.iter (defined_funcs m) ~f:(fun func ->
      let s = Func.to_string ~indent:(indent + 2) func in
      Buffer.add_string buf s);

  Buffer.contents buf
