(*
 * Copyright yutopp 2019 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module Span = Common.Span
module Diagnostics = Common.Diagnostics
module IntMap = Map.M (Int)

let rec unify_elem ~span (subst : Typing.Subst.t) lhs_ty rhs_ty :
    (Typing.Subst.t, Typer_err.t) Result.t =
  let open Result.Let_syntax in
  let Typing.Subst.{ ty_subst; ki_subst; _ } = subst in

  let s_lhs_ty = Typing.Subst.subst_type subst lhs_ty in
  let s_rhs_ty = Typing.Subst.subst_type subst rhs_ty in
  match (s_lhs_ty, s_rhs_ty) with
  (* *)
  | Typing.Type.({ ty = Var a; _ }, { ty = Var b; _ }) when a <> b ->
      [%Loga.debug "Unify var(%d) = var(%d)" a b];

      let ty_subst = Map.add_exn ty_subst ~key:a ~data:s_rhs_ty in
      Ok Typing.Subst.{ subst with ty_subst; ki_subst }
  (* *)
  | Typing.Type.
      ({ ty = Func (a_params, a_ret); _ }, { ty = Func (b_params, b_ret); _ })
    ->
      [%Loga.debug "Unify func() = func()"];

      let subst_ret =
        match (List.length a_params, List.length b_params) with
        | (an, bn) when an = bn ->
            (* unify args *)
            let rec unity_args subst a_params b_params index =
              match (a_params, b_params) with
              | ([], []) -> return subst
              | (a_param :: a_params', b_param :: b_params') ->
                  [%Loga.debug
                    "param(%d) ty(%s) = ty(%s)" index
                      (Typing.Type.to_string a_param)
                      (Typing.Type.to_string b_param)];

                  let%bind subst =
                    unify_elem ~span subst a_param b_param
                    |> Result.map_error ~f:(fun d ->
                           let kind = Typer_err.ErrFuncArgs index in
                           Typer_err.
                             {
                               lhs = a_param;
                               rhs = b_param;
                               kind;
                               nest = Some d;
                             })
                  in
                  unity_args subst a_params' b_params' (index + 1)
              | _ ->
                  failwith
                    (Printf.sprintf "[ICE] %d/%d" (List.length a_params)
                       (List.length b_params))
            in
            let%bind subst = unity_args subst a_params b_params 0 in
            (* unify ret *)
            let%bind subst =
              [%Loga.debug
                "ret ty(%s) = ty(%s)"
                  (Typing.Type.to_string a_ret)
                  (Typing.Type.to_string b_ret)];

              unify_elem ~span subst a_ret b_ret
              |> Result.map_error ~f:(fun d ->
                     let kind = Typer_err.ErrUnify in
                     Typer_err.{ lhs = a_ret; rhs = b_ret; kind; nest = Some d })
            in
            return subst
        (* *)
        | (an, bn) ->
            let kind = Typer_err.ErrFuncArgLength { r = an; l = bn } in
            let e =
              Typer_err.{ lhs = s_lhs_ty; rhs = s_rhs_ty; kind; nest = None }
            in
            Error e
      in
      let%bind subst =
        subst_ret
        |> Result.map_error ~f:(fun d ->
               let kind = Typer_err.ErrFuncArgRet in
               Typer_err.{ lhs = s_lhs_ty; rhs = s_rhs_ty; kind; nest = Some d })
      in
      Ok subst
  (* *)
  | Typing.Type.({ ty = Var v; _ }, ty') | Typing.Type.(ty', { ty = Var v; _ })
    ->
      [%Loga.debug "Unify var(%d) = ty(%s)" v (Typing.Type.to_string ty')];

      let ty_subst = Map.add_exn ty_subst ~key:v ~data:ty' in
      Ok Typing.Subst.{ subst with ty_subst }
  (* *)
  | (Typing.Type.{ ty = lhs_ty; _ }, Typing.Type.{ ty = rhs_ty; _ })
    when Poly.equal lhs_ty rhs_ty ->
      Ok subst
  (* *)
  | (lhs, rhs) ->
      [%Loga.debug
        "Cannot unify ty(%s) = ty(%s)"
          (Typing.Type.to_string lhs)
          (Typing.Type.to_string rhs)];

      let e = Typer_err.{ lhs; rhs; kind = ErrUnify; nest = None } in
      Error e

let unify ~span (subst : Typing.Subst.t) lhs_ty rhs_ty :
    (Typing.Subst.t, Diagnostics.Elem.t) Result.t =
  let open Result.Let_syntax in
  let%bind subst =
    unify_elem ~span subst lhs_ty rhs_ty
    |> Result.map_error ~f:(fun detail ->
           let e = new Reasons.type_mismatch ~detail in
           Diagnostics.Elem.error ~span e)
  in
  Ok subst
