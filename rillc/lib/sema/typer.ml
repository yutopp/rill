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
  | Typing.Type.({ ty = Var { var = a; _ }; _ }, { ty = Var { var = b; _ }; _ })
    when a <> b ->
      [%loga.debug "Unify var(%d) = var(%d)" a b];

      let ty_subst = Map.add_exn ty_subst ~key:a ~data:s_rhs_ty in
      Ok Typing.Subst.{ subst with ty_subst; ki_subst }
  (* *)
  | Typing.Type.
      ( { ty = Array { elem = a_elem; n = a_n }; _ },
        { ty = Array { elem = b_elem; n = b_n }; _ } ) ->
      [%loga.debug "Unify array() = array()"];
      let%bind subst =
        if a_n = b_n then
          unify_elem ~span subst a_elem b_elem
          |> Result.map_error ~f:(fun d ->
                 let kind = Typer_err.ErrArrayElem in
                 let diff = Typer_err.Type { lhs = a_elem; rhs = b_elem } in
                 Typer_err.{ diff; kind; nest = Some d })
        else
          let kind = Typer_err.ErrArrayLength { r = a_n; l = b_n } in
          let diff = Typer_err.Type { lhs = s_lhs_ty; rhs = s_rhs_ty } in
          let e = Typer_err.{ diff; kind; nest = None } in
          Error e
      in
      Ok subst
  (* *)
  | Typing.Type.
      ( ( {
            ty = Func { params = a_params; ret = a_ret; linkage = a_linkage };
            _;
          } as a ),
        ( {
            ty = Func { params = b_params; ret = b_ret; linkage = b_linkage };
            _;
          } as b ) ) ->
      [%loga.debug "Unify func() = func()"];

      let subst_ret =
        match (List.length a_params, List.length b_params) with
        | (an, bn) when an = bn ->
            (* unify args *)
            let rec unity_args subst a_params b_params index =
              match (a_params, b_params) with
              | ([], []) -> return subst
              | (a_param :: a_params', b_param :: b_params') ->
                  [%loga.debug
                    "param(%d) ty(%s) = ty(%s)" index
                      (Typing.Type.to_string a_param)
                      (Typing.Type.to_string b_param)];

                  let%bind subst =
                    unify_elem ~span subst a_param b_param
                    |> Result.map_error ~f:(fun d ->
                           let kind = Typer_err.ErrFuncArgs index in
                           let diff =
                             Typer_err.Type { lhs = a_param; rhs = b_param }
                           in
                           Typer_err.{ diff; kind; nest = Some d })
                  in
                  unity_args subst a_params' b_params' (index + 1)
              | _ ->
                  failwith
                    (Printf.sprintf "[ICE] %d/%d" (List.length a_params)
                       (List.length b_params))
            in
            let%bind subst = unity_args subst a_params b_params 0 in
            (* unify ret *)
            [%loga.debug
              "ret ty(%s) = ty(%s)"
                (Typing.Type.to_string a_ret)
                (Typing.Type.to_string b_ret)];
            let%bind subst =
              unify_elem ~span subst a_ret b_ret
              |> Result.map_error ~f:(fun d ->
                     let kind = Typer_err.ErrUnify in
                     let diff = Typer_err.Type { lhs = a_ret; rhs = b_ret } in
                     Typer_err.{ diff; kind; nest = Some d })
            in
            (* unify linkage *)
            let%bind subst =
              unify_lifetime ~span subst a_linkage b_linkage
              |> Result.map_error ~f:(fun d ->
                     let kind = Typer_err.ErrFuncLinkage in
                     let diff =
                       Typer_err.Linkage { lhs = a_linkage; rhs = b_linkage }
                     in
                     Typer_err.{ diff; kind; nest = Some d })
            in
            return subst
        (* *)
        | (an, bn) ->
            let kind = Typer_err.ErrFuncArgLength { r = an; l = bn } in
            let diff = Typer_err.Type { lhs = s_lhs_ty; rhs = s_rhs_ty } in
            let e = Typer_err.{ diff; kind; nest = None } in
            Error e
      in
      let%bind subst =
        subst_ret
        |> Result.map_error ~f:(fun d ->
               let kind = Typer_err.ErrFuncArgRet in
               let diff = Typer_err.Type { lhs = s_lhs_ty; rhs = s_rhs_ty } in
               Typer_err.{ diff; kind; nest = Some d })
      in
      Ok subst
  (* *)
  | Typing.Type.({ ty = Var { var = v; _ }; _ }, ty')
  | Typing.Type.(ty', { ty = Var { var = v; _ }; _ }) ->
      [%loga.debug "Unify var(%d) = ty(%s)" v (Typing.Type.to_string ty')];

      let ty_subst = Map.add_exn ty_subst ~key:v ~data:ty' in
      Ok Typing.Subst.{ subst with ty_subst }
  (* *)
  | (Typing.Type.{ ty = lhs_ty; _ }, Typing.Type.{ ty = rhs_ty; _ })
    when Poly.equal lhs_ty rhs_ty ->
      Ok subst
  (* *)
  | (lhs, rhs) ->
      [%loga.debug
        "Cannot unify ty(%s) = ty(%s)"
          (Typing.Type.to_string lhs)
          (Typing.Type.to_string rhs)];

      let kind = Typer_err.ErrUnify in
      let diff = Typer_err.Type { lhs; rhs } in
      let e = Typer_err.{ diff; kind; nest = None } in
      Error e

and unify_lifetime ~span subst lhs_linkage rhs_linkage =
  let Typing.Subst.{ ln_subst; _ } = subst in

  let s_lhs_linkage = Typing.Subst.subst_linkage subst lhs_linkage in
  let s_rhs_linkage = Typing.Subst.subst_linkage subst rhs_linkage in
  match (s_lhs_linkage, s_rhs_linkage) with
  (* *)
  | Typing.Type.(LinkageVar a, LinkageVar b) when a <> b ->
      let ln_subst = Map.add_exn ln_subst ~key:a ~data:s_rhs_linkage in
      Ok Typing.Subst.{ subst with ln_subst }
  (* *)
  | Typing.Type.(LinkageVar v, linkage') | Typing.Type.(linkage', LinkageVar v)
    ->
      let ty_subst = Map.add_exn ln_subst ~key:v ~data:linkage' in
      Ok Typing.Subst.{ subst with ln_subst }
  (* *)
  | (lhs_linkage, rhs_linkage) when Poly.equal lhs_linkage rhs_linkage ->
      Ok subst
  (* *)
  | (lhs_linkage, rhs_linkage) ->
      let kind = Typer_err.ErrUnify in
      let diff = Typer_err.Linkage { lhs = lhs_linkage; rhs = rhs_linkage } in
      let e = Typer_err.{ diff; kind; nest = None } in
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
