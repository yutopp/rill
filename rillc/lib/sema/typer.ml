(*
 * Copyright yutopp 2019 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module Span = Common.Span
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
                 let diff = Typer_err.diff_of_types ~subst a_elem b_elem in
                 Typer_err.{ diff; kind; nest = Some d })
        else
          let kind = Typer_err.ErrArrayLength { r = a_n; l = b_n } in
          let diff = Typer_err.diff_of_types ~subst s_lhs_ty s_rhs_ty in
          let e = Typer_err.{ diff; kind; nest = None } in
          Error e
      in
      Ok subst
  (* *)
  | Typing.Type.
      ( { ty = Pointer { mut = a_mut; elem = a_elem }; _ },
        { ty = Pointer { mut = b_mut; elem = b_elem }; _ } ) ->
      [%loga.debug "Unify pointer() = pointer()"];
      let%bind subst =
        unify_elem ~span subst a_elem b_elem
        |> Result.map_error ~f:(fun d ->
               let kind = Typer_err.ErrPointerElem in
               let diff = Typer_err.diff_of_types ~subst a_elem b_elem in
               Typer_err.{ diff; kind; nest = Some d })
      in
      (* unify mut *)
      let%bind subst =
        unify_mut ~span subst a_mut b_mut
        |> Result.map_error ~f:(fun d ->
               let kind = Typer_err.ErrPointerElem in
               let diff = Typer_err.diff_of_types ~subst s_lhs_ty s_rhs_ty in
               Typer_err.{ diff; kind; nest = Some d })
      in
      Ok subst
  (* *)
  | Typing.Type.({ ty = Type a; _ }, { ty = Type b; _ }) ->
      [%loga.debug "Unify type() = type()"];
      let%bind subst =
        unify_elem ~span subst a b
        |> Result.map_error ~f:(fun d ->
               let kind = Typer_err.ErrTypeElem in
               let diff = Typer_err.diff_of_types ~subst a b in
               Typer_err.{ diff; kind; nest = Some d })
      in
      Ok subst
  (* *)
  | Typing.Type.
      ( ({ ty = Num { bits = a_bits; signed = a_signed }; _ } as a),
        ({ ty = Num { bits = b_bits; signed = b_signed }; _ } as b) ) ->
      [%loga.debug "Unify num() = num()"];
      let%bind () =
        if a_bits = b_bits then Ok ()
        else
          let kind = Typer_err.ErrNumBits { r = a_bits; l = b_bits } in
          let diff = Typer_err.diff_of_types ~subst a b in
          Error Typer_err.{ diff; kind; nest = None }
      in
      let%bind () =
        if Bool.equal a_signed b_signed then Ok ()
        else
          let kind = Typer_err.ErrNumSigned { r = a_signed; l = b_signed } in
          let diff = Typer_err.diff_of_types ~subst a b in
          Error Typer_err.{ diff; kind; nest = None }
      in
      Ok subst
  (* *)
  | Typing.Type.
      ( ({ ty = Size { signed = a_signed }; _ } as a),
        ({ ty = Size { signed = b_signed }; _ } as b) ) ->
      [%loga.debug "Unify size() = size()"];
      let%bind () =
        if Bool.equal a_signed b_signed then Ok ()
        else
          let kind = Typer_err.ErrNumSigned { r = a_signed; l = b_signed } in
          let diff = Typer_err.diff_of_types ~subst a b in
          Error Typer_err.{ diff; kind; nest = None }
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
                             Typer_err.diff_of_types ~subst a_param b_param
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
                     let diff = Typer_err.diff_of_types ~subst a_ret b_ret in
                     Typer_err.{ diff; kind; nest = Some d })
            in
            (* unify linkage *)
            let%bind subst =
              unify_linkage ~span subst a_linkage b_linkage
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
            let diff = Typer_err.diff_of_types ~subst s_lhs_ty s_rhs_ty in
            let e = Typer_err.{ diff; kind; nest = None } in
            Error e
      in
      let%bind subst =
        subst_ret
        |> Result.map_error ~f:(fun d ->
               let kind = Typer_err.ErrFuncArgRet in
               let diff = Typer_err.diff_of_types ~subst s_lhs_ty s_rhs_ty in
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
      let diff = Typer_err.diff_of_types ~subst lhs rhs in
      let e = Typer_err.{ diff; kind; nest = None } in
      Error e

and unify_mut ~span subst lhs_mut rhs_mut =
  let Typing.Subst.{ ln_subst; _ } = subst in

  let s_lhs_mut = Typing.Subst.subst_mut subst lhs_mut in
  let s_rhs_mut = Typing.Subst.subst_mut subst rhs_mut in
  match (s_lhs_mut, s_rhs_mut) with
  (* *)
  | Typing.Type.(MutVar a, MutVar b) when a <> b ->
      let subst = Typing.Subst.update_mut subst a s_rhs_mut in
      Ok subst
  (* *)
  | Typing.Type.(MutVar v, mut') | Typing.Type.(mut', MutVar v) ->
      let subst = Typing.Subst.update_mut subst v mut' in
      Ok subst
  (* *)
  | (lhs_mut, rhs_mut) when Poly.equal lhs_mut rhs_mut -> Ok subst
  (* *)
  | (lhs_mut, rhs_mut) ->
      let kind = Typer_err.ErrUnify in
      let diff = Typer_err.Mutability { lhs = lhs_mut; rhs = rhs_mut } in
      let e = Typer_err.{ diff; kind; nest = None } in
      Error e

and unify_linkage ~span subst lhs_linkage rhs_linkage =
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
