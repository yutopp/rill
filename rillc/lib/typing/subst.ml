(*
 * Copyright yutopp 2019 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module Counter = Common.Counter

module FreshMap = struct
  module FreshMap = Map.M (Common.Fresh)

  module Redirector = struct
    type ('subst, 'map) t = {
      self_id : Counter.Value.t;
      bag : 'subst Subst_bag.t;
      extractor : 'subst -> 'map;
    }

    let create ~self_id ~bag ~e = { self_id; bag; extractor = e }

    let assert_same r ~key =
      let owner_id = Common.Fresh.owner key in
      if owner_id <> r.self_id then
        failwith
          (Printf.sprintf "[ICE] id is not same: target=%d, self=%d" owner_id
             r.self_id)

    let find_alt r ~key =
      let owner_id = Common.Fresh.owner key in
      if owner_id <> r.self_id then
        let alt = Subst_bag.find_subst r.bag ~owner_id |> r.extractor in
        Some alt
      else None
  end

  type ('a, 'b) t = {
    redirector : ('a, ('a, 'b) t) Redirector.t option;
    map : 'b FreshMap.t;
  }

  let create_generic () =
    { redirector = None; map = Map.empty (module Common.Fresh) }

  let create ~self_id ~bag ~e =
    let redirector = Redirector.create ~self_id ~bag ~e in
    let self = create_generic () in
    { self with redirector = Some redirector }

  let add_exn m ~key ~data =
    let () = Option.iter m.redirector ~f:(Redirector.assert_same ~key) in
    let map = Map.add_exn m.map ~key ~data in
    { m with map }

  let set m ~key ~data =
    let () = Option.iter m.redirector ~f:(Redirector.assert_same ~key) in
    let map = Map.set m.map ~key ~data in
    { m with map }

  let rec mem m key =
    let alt = Option.bind m.redirector ~f:(Redirector.find_alt ~key) in
    match alt with Some m -> mem m key | None -> Map.mem m.map key

  let rec find m key =
    let alt = Option.bind m.redirector ~f:(Redirector.find_alt ~key) in
    match alt with Some m -> find m key | None -> Map.find m.map key
end

type t = {
  id : Counter.Value.t;
  fresh_counter : Counter.t;
  ty_subst : (t, Type.t) FreshMap.t;
  ki_subst : (t, int) FreshMap.t;
  mut_subst : (t, Type.mutability_t) FreshMap.t;
  ln_subst : (t, Type.func_linkage_t) FreshMap.t;
  struct_tags : struct_tags_t;
  subtypes : (t, subtypes_t) FreshMap.t;
}

and bag_t = t Subst_bag.t

and struct_tags_t = { st_fresh_counter : Counter.t }

(* TODO: change to more efficient data structures to find constraints *)
and subtypes_t = subtype_t list

and subtype_t = { sub_target_ty : Type.t }

let create_struct_tags () = { st_fresh_counter = Counter.create () }

let create2 ~bag ~id : t =
  let ty_subst = FreshMap.create ~self_id:id ~bag ~e:(fun s -> s.ty_subst) in
  let ki_subst = FreshMap.create ~self_id:id ~bag ~e:(fun s -> s.ki_subst) in
  let mut_subst = FreshMap.create ~self_id:id ~bag ~e:(fun s -> s.mut_subst) in
  let ln_subst = FreshMap.create ~self_id:id ~bag ~e:(fun s -> s.ln_subst) in
  let subtypes = FreshMap.create ~self_id:id ~bag ~e:(fun s -> s.subtypes) in
  {
    id;
    fresh_counter = Counter.create ();
    ty_subst;
    ki_subst;
    mut_subst;
    ln_subst;
    struct_tags = create_struct_tags ();
    subtypes;
  }

let create_generic () : t =
  let ty_subst = FreshMap.create_generic () in
  let ki_subst = FreshMap.create_generic () in
  let mut_subst = FreshMap.create_generic () in
  let ln_subst = FreshMap.create_generic () in
  let subtypes = FreshMap.create_generic () in
  {
    id = 0 (* TODO *);
    fresh_counter = Counter.create ();
    ty_subst;
    ki_subst;
    mut_subst;
    ln_subst;
    struct_tags = create_struct_tags ();
    subtypes;
  }

(* has side effects *)
let fresh_var subst : Common.Type_var.t =
  let owner = subst.id in
  let fresh = Counter.fresh subst.fresh_counter in
  Common.Fresh.create ~owner ~fresh

(* has side effects *)
let fresh_ty_generic ~span ~bound ~label subst : Type.t =
  let v = fresh_var subst in
  let binding_mut = Type.MutImm in
  Type.{ ty = Var { var = v; bound; label }; binding_mut; span }

let fresh_ty ~span subst : Type.t =
  fresh_ty_generic ~span ~bound:Type.BoundWeak ~label:"tmp" subst

let fresh_forall_ty ~span ~label subst : Type.t =
  fresh_ty_generic ~span ~bound:Type.BoundForall ~label subst

let fresh_mut subst : Type.mutability_t =
  let v = fresh_var subst in
  Type.MutVar v

let fresh_linkage subst : Type.func_linkage_t =
  let v = fresh_var subst in
  Type.LinkageVar v

(* has side effects *)
let fresh_struct_tag subst =
  let { struct_tags; _ } = subst in
  let v = Counter.fresh struct_tags.st_fresh_counter in
  v

let get_struct_fields_from_name subst name = []

let update_mut subst uni_id mut =
  let { mut_subst; _ } = subst in
  let mut_subst = FreshMap.add_exn mut_subst ~key:uni_id ~data:mut in
  { subst with mut_subst }

let rec subst_mut (subst : t) mut =
  let { mut_subst; _ } = subst in
  match mut with
  | Type.MutVar uni_id ->
      FreshMap.find mut_subst uni_id
      |> Option.value_map ~default:mut ~f:(subst_mut subst)
  | alt -> alt

let rec subst_linkage (subst : t) linkage =
  let { ln_subst; _ } = subst in
  match linkage with
  | Type.LinkageVar uni_id ->
      FreshMap.find ln_subst uni_id
      |> Option.value_map ~default:linkage ~f:(subst_linkage subst)
  | alt -> alt

let update_type subst src dst =
  let { ty_subst; _ } = subst in
  let uni_id = Type.assume_var_id src in
  let ty_subst = FreshMap.set ty_subst ~key:uni_id ~data:dst in
  { subst with ty_subst }

let append_impl subst trait_var_id ~relation =
  let { subtypes; _ } = subst in
  let relations =
    FreshMap.find subtypes trait_var_id |> Option.value ~default:[]
  in
  let relations = relation :: relations in
  let subtypes = FreshMap.set subtypes ~key:trait_var_id ~data:relations in
  { subst with subtypes }

let find_subtype_rels subst var_id =
  let { subtypes; _ } = subst in
  let relations = FreshMap.find subtypes var_id in
  Option.value_exn relations

let is_bound subst ty =
  let { ty_subst; _ } = subst in
  let uni_id = Type.assume_var_id ty in
  (uni_id, FreshMap.mem ty_subst uni_id)

let assume_not_bound subst ty = (* TODO: implement *) ()

let rec subst_type' ~(subst : t) ty : Type.t =
  match ty with
  (* *)
  | Type.{ ty = Var { var = uni_id; _ }; binding_mut; span } -> (
      let { ty_subst; _ } = subst in
      match FreshMap.find ty_subst uni_id with
      | Some ty' ->
          let ty = subst_type' ~subst ty' in
          Type.{ ty with binding_mut }
      | None -> ty )
  (* *)
  | Type.{ ty = Array { elem; n }; _ } as tty ->
      let elem = subst_type' ~subst elem in
      Type.{ tty with ty = Array { elem; n } }
  (* *)
  | Type.{ ty = Func { params; ret; linkage }; _ } as tty ->
      let params = List.map ~f:(subst_type' ~subst) params in
      let ret = subst_type' ~subst ret in
      let linkage = subst_linkage subst linkage in
      Type.{ tty with ty = Func { params; ret; linkage } }
  (* *)
  | Type.{ ty = Pointer { mut; elem }; _ } as tty ->
      let mut = subst_mut subst mut in
      let elem = subst_type' ~subst elem in
      Type.{ tty with ty = Pointer { mut; elem } }
  (* *)
  | Type.{ ty = Args { recv; args }; _ } as tty ->
      let subst =
        List.fold_left args ~init:subst ~f:(fun subst apply ->
            let Type.{ apply_src_ty = src; apply_dst_ty = dst } = apply in
            update_type subst src dst)
      in
      let recv = subst_type' ~subst recv in
      Type.{ tty with ty = Args { recv; args } }
  (* meta *)
  | Type.{ ty = Type inner_ty; _ } as tty ->
      let inner_ty = subst_type' ~subst inner_ty in
      Type.{ tty with ty = Type inner_ty }
  | alt -> alt

let subst_type subst ty = subst_type' ~subst ty

let subst_cond subst cond =
  let Pred.{ cond_trait; cond_var } = cond in
  let cond_trait = subst_type subst cond_trait in
  let cond_var = subst_type subst cond_var in
  Pred.{ cond_trait; cond_var }

let subst_pred (subst : t) ty_pred : Pred.t =
  let (Pred.Pred { conds; ty; _ }) = ty_pred in
  let ty = subst_type subst ty in
  let conds = conds |> List.map ~f:(subst_cond subst) in
  Pred.Pred { conds; ty }

let subst_scheme (subst : t) ty_sc : Scheme.t =
  let (Scheme.ForAll { implicits; vars; ty }) = ty_sc in
  let ty = subst_pred subst ty in
  Scheme.ForAll { implicits; vars; ty }
