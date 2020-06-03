(*
 * Copyright yutopp 2019 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module Diagnostics = Common.Diagnostics
module IntMap = Map.M (Int)
module Counter = Common.Counter

type t = {
  subst_id : int;
  fresh_counter : Counter.t;
  ty_subst : Type.t IntMap.t;
  ki_subst : int IntMap.t;
  ln_subst : Type.func_linkage_t IntMap.t;
}

let create subst_id : t =
  let ty_subst = Map.empty (module Int) in
  let ki_subst = Map.empty (module Int) in
  let ln_subst = Map.empty (module Int) in
  { subst_id; fresh_counter = Counter.create (); ty_subst; ki_subst; ln_subst }

(* has side effects *)
let fresh_var subst : Type.var_t = Counter.fresh subst.fresh_counter

(* has side effects *)
let fresh_ty ~span subst : Type.t =
  let v = fresh_var subst in
  let binding_mut = Type.MutImm in
  Type.{ ty = Var { var = v; subst_id = subst.subst_id }; binding_mut; span }

let fresh_linkage subst : Type.func_linkage_t =
  let v = fresh_var subst in
  Type.LinkageVar v

let rec subst_linkage (subst : t) linkage =
  let { ln_subst; _ } = subst in
  match linkage with
  | Type.LinkageVar uni_id ->
      Map.find ln_subst uni_id
      |> Option.value_map ~default:linkage ~f:(subst_linkage subst)
  | alt -> alt

let rec subst_type (subst : t) ty : Type.t =
  let { ty_subst; _ } = subst in
  match ty with
  | Type.{ ty = Var { var = uni_id; _ }; binding_mut; span } -> (
      match Map.find ty_subst uni_id with
      | Some ty' ->
          let ty = subst_type subst ty' in
          Type.{ ty with binding_mut }
      | None -> ty )
  | Type.{ ty = Func { params; ret; linkage }; binding_mut; span } ->
      let params = List.map ~f:(subst_type subst) params in
      let ret = subst_type subst ret in
      let linkage = subst_linkage subst linkage in
      Type.{ ty = Func { params; ret; linkage }; binding_mut; span }
  | alt -> alt

let subst_tysc (subst : t) tysc : Scheme.t =
  match tysc with
  | Scheme.(Scheme ([], tys)) ->
      let tys' = subst_type subst tys in
      Scheme.Scheme ([], tys')
  | _ -> failwith "subst_tysc"
