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
  mutable global_vars : global_vars_t;
  mutable funcs : funcs_t;
  mutable types : types_t;
}

and global_vars_t = {
  global_vars_map : (Global.t StringMap.t[@printer fun fmt _ -> fprintf fmt ""]);
}

and funcs_t = {
  funcs_rev : Func.t list;
  funcs_map : (Func.t StringMap.t[@printer fun fmt _ -> fprintf fmt ""]);
}

and types_t = { types_rev : Type.t list } [@@deriving show]

let create ~ctx : t =
  let global_vars = { global_vars_map = Map.empty (module String) } in
  let funcs = { funcs_rev = []; funcs_map = Map.empty (module String) } in
  let types = { types_rev = [] } in
  { ctx; module_name = ""; global_vars; funcs; types }

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

let declare_global_var m name ty =
  let { global_vars_map } = m.global_vars in
  (* TODO: type checking *)
  let id = Common.Chain.Nest.to_unique_id name in

  let g = Global.create ~name ~ty in
  match Map.add global_vars_map ~key:id ~data:g with
  | `Ok new_map ->
      let global_vars_map = new_map in
      m.global_vars <- { global_vars_map };
      g
  | `Duplicate -> Map.find_exn global_vars_map id

let all_global_vars m : Global.t list =
  let { global_vars_map; _ } = m.global_vars in
  Map.data global_vars_map

let define_type m name inner_ty =
  let { types_rev; _ } = m.types in

  let rir_ty = Type.create ~name ~inner_ty in

  let types_rev = rir_ty :: types_rev in
  m.types <- { m.types with types_rev }

let all_types m : Type.t list =
  let { types_rev; _ } = m.types in
  List.rev types_rev

let to_string m =
  let indent = 0 in
  let buf = Buffer.create 256 in
  Buffer.add_string buf (Printf.sprintf "Module: name=%s\n" m.module_name);

  List.iter (all_types m) ~f:(fun rir_ty ->
      let s = Type.to_string ~indent:(indent + 2) rir_ty in
      Buffer.add_string buf s);

  List.iter (defined_funcs m) ~f:(fun func ->
      let s = Func.to_string ~indent:(indent + 2) func in
      Buffer.add_string buf s);

  Buffer.contents buf
