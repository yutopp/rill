(*
 * Copyright yutopp 2018 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module StringMap = Map.M (String)

let to_placeholder_generic path ~subst_type =
  let Path.{ tag; _ } = path in
  let rec convert layers rev_layers trait_scope_rev_layers has_self_layer =
    match layers with
    | [] ->
        let name = Path.create ~tag (rev_layers |> List.rev) in
        let placeholder =
          match (trait_scope_rev_layers, has_self_layer) with
          | (Some rev_nest, Some layer) ->
              Term.PlaceholderGlobal2 { name; dispatch = true }
          | _ -> Term.PlaceholderGlobal2 { name; dispatch = false }
        in
        placeholder
    | l :: rest ->
        let Path.Name.{ kind; generics_vars; has_self; _ } = l in
        let generics_vars = List.map generics_vars ~f:subst_type in
        let l = Path.Name.{ l with generics_vars } in
        let rev_layers = l :: rev_layers in

        let trait_scope_rev_layers =
          match kind with
          | Path.Name.Type -> Some rev_layers
          | _ -> trait_scope_rev_layers
        in
        let has_self_layer =
          match has_self with true -> Some l | false -> has_self_layer
        in
        convert rest rev_layers trait_scope_rev_layers has_self_layer
  in
  let layers = Path.to_list path in
  convert layers [] None None

let to_placeholder ~subst path =
  to_placeholder_generic path ~subst_type:(fun v ->
      Typing.Subst.subst_type subst v)

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

  let is_declared funcs ~path =
    let { funcs_map; _ } = funcs in
    let id = Symbol.to_generic_id path in
    Map.mem funcs_map id

  let declare_func funcs ~name ~ty_sc =
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
    | Some _ ->
        failwith (Printf.sprintf "[ICE]: function '%s' is already declared" id)

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

module Impl = struct
  type t = {
    name : Typing.Type.t Path.t;
    mutable members :
      (Typing.Type.t Path.t StringMap.t
      [@printer fun fmt _ -> fprintf fmt ""]);
  }
  [@@deriving show]

  let create ~name =
    let members = Map.empty (module String) in
    { name; members }

  let add_member trait tag name =
    let { members; _ } = trait in
    let tag = Symbol.to_generic_layer tag in
    let members = Map.add_exn members ~key:tag ~data:name in
    trait.members <- members;

    ()

  let find impl name =
    let { members; _ } = impl in
    let id = Symbol.to_generic_layer name in
    Map.find_exn members id

  let to_string ~indent trait =
    let buf = Buffer.create 256 in
    Buffer.add_string buf (String.make indent ' ');

    let { name; members; _ } = trait in
    Buffer.add_string buf
      (Printf.sprintf "Impl: name = '%s'\n"
         (Path.to_string ~to_s:Typing.Type.to_string name));

    Map.iteri members ~f:(fun ~key ~data ->
        Buffer.add_string buf (String.make (indent + 2) ' ');
        Buffer.add_string buf
          (Printf.sprintf "%s -> '%s'\n" key
             (Path.to_string ~to_s:Typing.Type.to_string data)));

    Buffer.contents buf
end

module Trait = struct
  type t = {
    name : Typing.Type.t Path.t;
    mutable impls : (Impl.t StringMap.t[@printer fun fmt _ -> fprintf fmt ""]);
  }
  [@@deriving show]

  let create ~name =
    let impls = Map.empty (module String) in
    { name; impls }

  let add_impl trait impl =
    let { impls; _ } = trait in
    let id = Symbol.to_signatured_id impl.Impl.name in

    let impls = Map.add_exn impls ~key:id ~data:impl in
    trait.impls <- impls;

    ()

  let find trait name =
    let { impls; _ } = trait in
    let id = Symbol.to_signatured_id' name in
    Map.find_exn impls id

  let to_string ~indent trait =
    let buf = Buffer.create 256 in
    Buffer.add_string buf (String.make indent ' ');

    let { name; impls; _ } = trait in
    Buffer.add_string buf
      (Printf.sprintf "Trait: name = '%s'\n"
         (Path.to_string ~to_s:Typing.Type.to_string name));

    Map.iteri impls ~f:(fun ~key ~data ->
        Buffer.add_string buf (Impl.to_string ~indent:(indent + 2) data));

    Buffer.contents buf
end

module Trait_generics = struct
  type t = {
    mutable instances_map :
      (instances_t StringMap.t
      [@printer fun fmt _ -> fprintf fmt ""]);
  }

  and instances_t = {
    base : Trait.t;
    mutable instances :
      (Trait.t StringMap.t
      [@printer fun fmt _ -> fprintf fmt ""]);
  }
  [@@deriving show]

  let create () = { instances_map = Map.empty (module String) }

  let create_overload ~trait =
    { base = trait; instances = Map.empty (module String) }

  let register_dict traits name impl =
    let { instances_map; _ } = traits in
    (* TODO: support generics *)
    let id = Symbol.to_generic_id name in
    [%loga.debug "register_dict -> %s" id];

    let instances =
      match Map.find instances_map id with
      | Some t -> t
      | None ->
          let trait = Trait.create ~name in
          let instances = create_overload ~trait in
          instances
    in
    let trait = instances.base in
    Trait.add_impl trait impl;
    let instances_map = Map.set instances_map ~key:id ~data:instances in
    traits.instances_map <- instances_map

  let find traits name =
    let { instances_map; _ } = traits in
    let id = Symbol.to_generic_id' name in
    [%loga.debug "find_dict -> %s" id];

    let set = Map.find_exn instances_map id in
    set.base

  let base traits : Trait.t list =
    let { instances_map; _ } = traits in
    Map.data instances_map |> List.map ~f:(fun i -> i.base)
end

module Monom = struct
  module StringSet = Set.M (String)

  type t = {
    mutable hints : (StringSet.t[@printer fun fmt _ -> fprintf fmt ""]);
    mutable candidates : (Symbol.t list[@printer fun fmt _ -> fprintf fmt ""]);
  }
  [@@deriving show]

  let create () =
    let hints = Set.empty (module String) in
    let candidates = [] in
    { hints; candidates }

  let register_candidate m name =
    [%loga.debug
      "register_candidate: %s :: %b"
        (Path.to_string ~to_s:Typing.Type.to_string name)
        (Symbol.has_generics name)];

    if Symbol.is_generics name && not (Symbol.has_generics name) then
      let { hints; candidates; _ } = m in
      let signature = Symbol.to_signatured_id name in
      match Set.mem hints signature with
      | true -> ()
      | false ->
          let hints = Set.add hints signature in
          let candidates = name :: candidates in
          [%loga.debug "candidates: %d" (List.length candidates)];
          m.hints <- hints;
          m.candidates <- candidates

  let drain_candidates m =
    let { candidates; _ } = m in
    m.candidates <- [];
    [%loga.debug "candidates: %d" (List.length candidates)];
    candidates

  let list_registerd m =
    let { hints; _ } = m in
    Set.to_list hints
end

type t = {
  ctx : (Context.t[@printer fun fmt _ -> fprintf fmt ""]);
  module_name : string;
  mutable global_vars : global_vars_t;
  mutable funcs : Func_generics.t;
  mutable types : types_t;
  monom : Monom.t;
  mutable global_traits : Trait_generics.t;
  mutable used_externals : Typing.Type.t Path.t list;
}

and global_vars_t = {
  global_vars_map : (Global.t StringMap.t[@printer fun fmt _ -> fprintf fmt ""]);
}

and types_t = { types_rev : Type.t list } [@@deriving show]

let create ~ctx : t =
  let global_vars = { global_vars_map = Map.empty (module String) } in
  let funcs = Func_generics.create () in
  let types = { types_rev = [] } in
  let monom = Monom.create () in
  let global_traits = Trait_generics.create () in
  {
    ctx;
    module_name = "";
    global_vars;
    funcs;
    types;
    monom;
    global_traits;
    used_externals = [];
  }

let declare_func ~subst m name ty_sc =
  let f = Func_generics.declare_func m.funcs ~name ~ty_sc in
  let placeholder = to_placeholder ~subst name in
  let () =
    match placeholder with
    | Term.PlaceholderGlobal2 _ -> ()
    | _ -> failwith "[ICE]"
  in
  f

let define_impl ~subst m trait_name for_name mapping =
  let impl = Impl.create ~name:for_name in
  let () =
    List.iter mapping ~f:(fun (key, data) -> Impl.add_member impl key data)
  in
  Trait_generics.register_dict m.global_traits trait_name impl

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

let register_monomorphization_candidate m name =
  let { monom; _ } = m in
  Monom.register_candidate monom name

let drain_monomorphization_candidates m =
  let { monom; _ } = m in
  Monom.drain_candidates monom

let mono m =
  let { monom; _ } = m in
  Monom.list_registerd monom

let base_traits m : Trait.t list = Trait_generics.base m.global_traits

let find_trait m name = Trait_generics.find m.global_traits name

let append_used_external m path = m.used_externals <- path :: m.used_externals

let to_string m =
  let indent = 0 in
  let buf = Buffer.create 256 in

  Buffer.add_string buf (Printf.sprintf "Module: name = '%s'\n" m.module_name);

  Buffer.add_string buf "== mono\n";
  List.iter (mono m) ~f:(fun key ->
      let indent = indent + 2 in
      Buffer.add_string buf (String.make indent ' ');
      let s = Printf.sprintf "%s\n" key in
      Buffer.add_string buf s);

  Buffer.add_string buf "== types\n";
  List.iter (all_types m) ~f:(fun rir_ty ->
      let s = Type.to_string ~indent:(indent + 2) rir_ty in
      Buffer.add_string buf s);

  Buffer.add_string buf "== base traits\n";
  List.iter (base_traits m) ~f:(fun trait ->
      let s = Trait.to_string ~indent:(indent + 2) trait in
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
