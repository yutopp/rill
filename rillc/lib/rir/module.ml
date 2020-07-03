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
module Func_generics = struct
  type t = {
    mutable funcs_rev : instances_t list;
    mutable funcs_map :
      (instances_t StringMap.t
      [@printer fun fmt _ -> fprintf fmt ""]);
  }

  and instances_t = {
    base : Func.t;
    kind : base_kind_t;
    mutable instances :
      (Func.t StringMap.t
      [@printer fun fmt _ -> fprintf fmt ""]);
  }

  and base_kind_t = Generics | Standard [@@deriving show]

  let create () = { funcs_rev = []; funcs_map = Map.empty (module String) }

  let create_overload ~f ~kind =
    { base = f; kind; instances = Map.empty (module String) }

  let declare_func funcs name ty_sc =
    let { funcs_map; funcs_rev } = funcs in
    let id = Symbol.to_generic_id name in

    match Map.find funcs_map id with
    | None ->
        let f = Func.create ~name ~ty_sc in

        let kind = if Symbol.has_generics name then Generics else Standard in
        let set = create_overload ~f ~kind in

        let funcs_map = Map.add_exn funcs_map ~key:id ~data:set in
        funcs.funcs_map <- funcs_map;

        let funcs_rev = set :: funcs_rev in
        funcs.funcs_rev <- funcs_rev;

        f
    | Some _ -> failwith "[ICE]"

  let find_generic_func funcs name =
    let { funcs_map; _ } = funcs in
    let id = Symbol.to_generic_id name in

    let set = Map.find_exn funcs_map id in
    match set.kind with Generics -> set.base | _ -> failwith "[ICE]"

  let declare_instance_func funcs name ty_sc =
    let { funcs_map; funcs_rev } = funcs in
    let id = Symbol.to_generic_id name in

    let set =
      match Map.find funcs_map id with
      | Some set -> set
      | None -> failwith "[ICE]"
    in

    let instance_id = Symbol.to_param_args_id name in
    match Map.find set.instances instance_id with
    | None ->
        let f = Func.create ~name ~ty_sc in

        let instances = Map.add_exn set.instances ~key:instance_id ~data:f in
        set.instances <- instances;

        f
    | Some _ -> failwith "[ICE]"

  let base_funcs funcs : Func.t list =
    let { funcs_map; _ } = funcs in
    Map.data funcs_map |> List.map ~f:(fun i -> i.base)

  let defined_funcs funcs : Func.t list =
    let { funcs_rev; _ } = funcs in
    List.rev funcs_rev
    |> List.concat_map ~f:(fun i ->
           match i.kind with
           | Standard -> [ i.base ]
           | Generics -> i.instances |> Map.data)
end

type t = {
  ctx : (Context.t[@printer fun fmt _ -> fprintf fmt ""]);
  module_name : string;
  mutable global_vars : global_vars_t;
  mutable funcs : Func_generics.t;
  mutable types : types_t;
  mutable hints : (Symbol.t StringMap.t[@printer fun fmt _ -> fprintf fmt ""]);
}

and global_vars_t = {
  global_vars_map : (Global.t StringMap.t[@printer fun fmt _ -> fprintf fmt ""]);
}

and types_t = { types_rev : Type.t list } [@@deriving show]

let create ~ctx : t =
  let global_vars = { global_vars_map = Map.empty (module String) } in
  let funcs = Func_generics.create () in
  let types = { types_rev = [] } in
  let hints = Map.empty (module String) in
  { ctx; module_name = ""; global_vars; funcs; types; hints }

let add_hint_for_specialization m name =
  if Symbol.is_generics name then
    let { hints; _ } = m in
    let signature = Symbol.to_signatured_id name in
    match Map.mem hints signature with
    | true -> ()
    | false ->
        let hints = Map.add_exn hints ~key:signature ~data:name in
        m.hints <- hints

let declare_func m name ty_sc = Func_generics.declare_func m.funcs name ty_sc

let declare_instance_func m name ty_sc =
  Func_generics.declare_instance_func m.funcs name ty_sc

let find_generic_func m name = Func_generics.find_generic_func m.funcs name

let base_funcs m : Func.t list = Func_generics.base_funcs m.funcs

let defined_funcs m : Func.t list = Func_generics.defined_funcs m.funcs

let declare_global_var m name ty_sc =
  let { global_vars_map } = m.global_vars in
  (* TODO: type checking *)
  let id = Symbol.to_id name in

  let g = Global.create ~name ~ty_sc in
  match Map.add global_vars_map ~key:id ~data:g with
  | `Ok new_map ->
      let global_vars_map = new_map in
      m.global_vars <- { global_vars_map };
      g
  | `Duplicate -> Map.find_exn global_vars_map id

let all_global_vars m : Global.t list =
  let { global_vars_map; _ } = m.global_vars in
  Map.data global_vars_map

let define_type m name ty_sc =
  let { types_rev; _ } = m.types in

  let rir_ty = Type.create ~name ~ty_sc in

  let types_rev = rir_ty :: types_rev in
  m.types <- { m.types with types_rev }

let all_types m : Type.t list =
  let { types_rev; _ } = m.types in
  List.rev types_rev

let hints m =
  let { hints; _ } = m in
  hints

let to_string m =
  let indent = 0 in
  let buf = Buffer.create 256 in

  Buffer.add_string buf (Printf.sprintf "Module: name = '%s'\n" m.module_name);

  Buffer.add_string buf "== generics\n";
  Map.iteri m.hints ~f:(fun ~key ~data ->
      let indent = indent + 2 in
      Buffer.add_string buf (String.make indent ' ');
      let s = Printf.sprintf "%s\n" key in
      Buffer.add_string buf s);

  Buffer.add_string buf "== types\n";
  List.iter (all_types m) ~f:(fun rir_ty ->
      let s = Type.to_string ~indent:(indent + 2) rir_ty in
      Buffer.add_string buf s);

  Buffer.add_string buf "== base funcs\n";
  List.iter (base_funcs m) ~f:(fun func ->
      let s = Func.to_string ~indent:(indent + 2) func in
      Buffer.add_string buf s);

  Buffer.add_string buf "== defined funcs\n";
  List.iter (defined_funcs m) ~f:(fun func ->
      let s = Func.to_string ~indent:(indent + 2) func in
      Buffer.add_string buf s);

  Buffer.contents buf
