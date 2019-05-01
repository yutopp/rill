(*
 * Copyright yutopp 2019 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

type t = {
  mutable id: Type.var_t;
}

let create () =
  {
    id = 0;
  }

let fresh_var typer : Type.var_t =
  let v = typer.id in
  typer.id <- Type.next_var typer.id;
  v

let fresh_ty typer : Type.t =
  let v = fresh_var typer in
  Type.Var v

module Subst = struct
  module IntMap = Map.M(Int)

  type t =
    (Type.t IntMap.t * int IntMap.t)

  let create () : t =
    let ty_subst = Map.empty (module Int) in
    let ki_subst = Map.empty (module Int) in
    (ty_subst, ki_subst)

  let rec subst_type (subst : t) ty =
    let (ty_subst, ki_subst) = subst in

    match ty with
    | Type.Var uni_id ->
       begin match Map.find ty_subst uni_id with
       | Some ty' -> subst_type subst ty'
       | None -> ty
       end

    | Type.Func (params, ret) ->
       let params' = List.map ~f:(subst_type subst) params in
       let ret' = subst_type subst ret in
       Type.Func (params', ret')

    | alt ->
       alt

  let subst_tysc (subst : t) tysc =
    match tysc with
    | Type.Scheme.(Scheme ([], tys)) ->
       let tys' = subst_type subst tys in
       Type.Scheme.Scheme ([], tys')

    | _ ->
       failwith ""

  let rec unify subst lhs_ty rhs_ty =
    let (ty_subst, ki_subst) = subst in

    let s_lhs_ty = subst_type subst lhs_ty in
    let s_rhs_ty = subst_type subst rhs_ty in
    match (s_lhs_ty, s_rhs_ty) with
    | Type.(Var a, Var b) when a <> b ->
       let ty_subst = Map.add_exn ty_subst ~key:a ~data:s_rhs_ty in
       (ty_subst, ki_subst)

    | Type.(Func (a_params, a_ret), Func (b_params, b_ret)) ->
       begin match (List.length a_params, List.length b_params) with
       | (an, bn) when an = bn ->
          let subst' =
            List.fold_left ~f:(fun subst (a_param, b_param) ->
                             unify subst a_param b_param)
                           ~init:subst
                           (List.zip_exn a_params b_params)
          in
          unify subst' a_ret b_ret

       | _ ->
          failwith ""
       end

    | Type.(Var v, ty')
    | Type.(ty', Var v) ->
       let ty_subst = Map.add_exn ty_subst ~key:v ~data:ty' in
       (ty_subst, ki_subst)

    | (lhs, rhs) when Poly.equal lhs rhs ->
       subst

    | _ ->
       failwith ""
end
