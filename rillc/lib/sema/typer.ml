(*
 * Copyright yutopp 2019 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

module IntMap = Map.M(Int)

(* TODO: move elsewhere *)
type err_t =
  | ErrTypeMismatch of Type.t * Type.t

module Counter = struct
  type t = {
    c: int ref
  }

  let create () =
    {
      c = ref 0;
    }

  let count c =
    !(c.c)

  let incr c =
    Int.incr c.c
end

type t =
  {
    fresh_id: Counter.t;
    ty_subst: Type.t IntMap.t;
    ki_subst: int IntMap.t
  }

let create () : t =
  let ty_subst = Map.empty (module Int) in
  let ki_subst = Map.empty (module Int) in
  {
    fresh_id = Counter.create ();
    ty_subst;
    ki_subst;
  }

(* has side effects *)
let fresh_var subst : Type.var_t =
  let v = Counter.count subst.fresh_id in
  let () = Counter.incr subst.fresh_id in
  v

(* has side effects *)
let fresh_ty subst : Type.t =
  let v = fresh_var subst in
  Type.Var v

let rec subst_type (subst : t) ty : Type.t =
  let {ty_subst; ki_subst; _} = subst in

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

let subst_tysc (subst : t) tysc : Type.Scheme.t =
  match tysc with
  | Type.Scheme.(Scheme ([], tys)) ->
     let tys' = subst_type subst tys in
     Type.Scheme.Scheme ([], tys')

  | _ ->
     failwith ""

let rec unify (subst : t) lhs_ty rhs_ty : (t, err_t) Result.t =
  let {ty_subst; ki_subst; _} = subst in

  let s_lhs_ty = subst_type subst lhs_ty in
  let s_rhs_ty = subst_type subst rhs_ty in
  match (s_lhs_ty, s_rhs_ty) with
  | Type.(Var a, Var b) when a <> b ->
     let ty_subst = Map.add_exn ty_subst ~key:a ~data:s_rhs_ty in
     Ok {subst with ty_subst; ki_subst}

  | Type.(Func (a_params, a_ret), Func (b_params, b_ret)) ->
     begin match (List.length a_params, List.length b_params) with
     | (an, bn) when an = bn ->
        let open Result.Let_syntax in
        (* unify args *)
        let rec f subst a_params b_params index =
          match (a_params, b_params) with
          | (a_param :: a_params, b_param :: b_params) ->
             let%bind subst =
               (* TODO: return more detailed info *)
               unify subst a_param b_param
             in
             subst |> return
          | _ ->
             failwith "[ICE] Invalid length"
        in
        let%bind subst =
          f subst a_params b_params 0
          (* TODO: return more detailed info *)
          |> Result.map_error ~f:(fun _ -> ErrTypeMismatch (s_lhs_ty, s_rhs_ty))
        in
        (* unify ret *)
        let%bind subst =
          unify subst a_ret b_ret
          (* TODO: return more detailed info *)
          |> Result.map_error ~f:(fun _ -> ErrTypeMismatch (s_lhs_ty, s_rhs_ty))
        in
        subst |> return

     | _ ->
        failwith "(TODO): type parameter length"
     end

  | Type.(Var v, ty')
  | Type.(ty', Var v) ->
     let ty_subst = Map.add_exn ty_subst ~key:v ~data:ty' in
     Ok {subst with ty_subst}

  | (lhs, rhs) when Poly.equal lhs rhs ->
     Ok subst

  | (lhs, rhs) ->
     Error (ErrTypeMismatch (lhs, rhs))
