(*
 * Copyright yutopp 2019 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module FreshMap = Map.M (Common.Fresh)
module Counter = Common.Counter

type t = {
  fresh_counter : Counter.t;
  ty_subst : Type.t FreshMap.t;
  ki_subst : int FreshMap.t;
  mut_subst : Type.mutability_t FreshMap.t;
  ln_subst : Type.func_linkage_t FreshMap.t;
  struct_tags : struct_tags_t;
  subtypes : subtypes_t FreshMap.t;
}

and struct_tags_t = { st_fresh_counter : Counter.t }

(* TODO: change to more efficient data structures to find constraints *)
and subtypes_t = subtype_t list

and subtype_t = { sub_target_ty : Type.t }

let create_struct_tags () = { st_fresh_counter = Counter.create () }

let create () =
  let ty_subst = Map.empty (module Common.Fresh) in
  let ki_subst = Map.empty (module Common.Fresh) in
  let mut_subst = Map.empty (module Common.Fresh) in
  let ln_subst = Map.empty (module Common.Fresh) in
  let subtypes = Map.empty (module Common.Fresh) in
  {
    fresh_counter = Counter.create ();
    ty_subst;
    ki_subst;
    mut_subst;
    ln_subst;
    struct_tags = create_struct_tags ();
    subtypes;
  }

(* has side effects *)
let fresh_var subst : Common.Type_var.t = (0, Counter.fresh subst.fresh_counter)

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
  let mut_subst = Map.add_exn mut_subst ~key:uni_id ~data:mut in
  { subst with mut_subst }

let rec subst_mut (subst : t) mut =
  let { mut_subst; _ } = subst in
  match mut with
  | Type.MutVar uni_id ->
      Map.find mut_subst uni_id
      |> Option.value_map ~default:mut ~f:(subst_mut subst)
  | alt -> alt

let rec subst_linkage (subst : t) linkage =
  let { ln_subst; _ } = subst in
  match linkage with
  | Type.LinkageVar uni_id ->
      Map.find ln_subst uni_id
      |> Option.value_map ~default:linkage ~f:(subst_linkage subst)
  | alt -> alt

let update_type subst src dst =
  let { ty_subst; _ } = subst in
  let uni_id = Type.assume_var_id src in
  let ty_subst = Map.set ty_subst ~key:uni_id ~data:dst in
  { subst with ty_subst }

let append_impl subst trait_var_id ~relation =
  let { subtypes; _ } = subst in
  let relations = Map.find subtypes trait_var_id |> Option.value ~default:[] in
  let relations = relation :: relations in
  let subtypes = Map.set subtypes ~key:trait_var_id ~data:relations in
  { subst with subtypes }

let find_subtype_rels subst var_id =
  let { subtypes; _ } = subst in
  let relations = Map.find_exn subtypes var_id in
  relations

let is_bound subst ty =
  let { ty_subst; _ } = subst in
  let uni_id = Type.assume_var_id ty in
  (uni_id, Map.mem ty_subst uni_id)

let assume_not_bound subst ty = (* TODO: implement *) ()

let rec subst_type' ~(subst : t) ty : Type.t =
  match ty with
  (* *)
  | Type.{ ty = Var { var = uni_id; _ }; binding_mut; span } -> (
      let { ty_subst; _ } = subst in
      match Map.find ty_subst uni_id with
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
