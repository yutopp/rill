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
  fresh_id : Counter.t;
  ty_subst : Type.t IntMap.t;
  ki_subst : int IntMap.t;
}

let create () : t =
  let ty_subst = Map.empty (module Int) in
  let ki_subst = Map.empty (module Int) in
  { fresh_id = Counter.create (); ty_subst; ki_subst }

(* has side effects *)
let fresh_var subst : Type.var_t = Counter.fresh subst.fresh_id

(* has side effects *)
let fresh_ty ~span subst : Type.t =
  let v = fresh_var subst in
  Type.{ ty = Var v; span }

let rec subst_type (subst : t) ty : Type.t =
  let { ty_subst; ki_subst; _ } = subst in
  match ty with
  | Type.{ ty = Var uni_id; span } -> (
      match Map.find ty_subst uni_id with
      | Some ty' -> subst_type subst ty'
      | None -> ty )
  | Type.{ ty = Func (params, ret); span } ->
      let params' = List.map ~f:(subst_type subst) params in
      let ret' = subst_type subst ret in
      Type.{ ty = Func (params', ret'); span }
  | alt -> alt

let subst_tysc (subst : t) tysc : Scheme.t =
  match tysc with
  | Scheme.(Scheme ([], tys)) ->
      let tys' = subst_type subst tys in
      Scheme.Scheme ([], tys')
  | _ -> failwith ""
