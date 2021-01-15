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

let rec unify_var ~span (subst : Typing.Subst.t) ~from ~to_ =
  let Typing.Subst.{ ty_subst; _ } = subst in

  let from = Typing.Subst.subst_type subst from in
  let to_ = Typing.Subst.subst_type subst to_ in
  match (from, to_) with
  (* *)
  | Typing.Type.({ ty = Var { var = a; _ }; _ }, { ty = Var { var = b; _ }; _ })
    when not (Common.Type_var.equal a b) ->
      [%loga.debug
        "Unify var(%s in) = var(%s)"
          (Common.Type_var.to_string a)
          (Common.Type_var.to_string b)];

      let ty_subst = Map.add_exn ty_subst ~key:a ~data:to_ in
      Ok Typing.Subst.{ subst with ty_subst }
  (* *)
  | Typing.Type.({ ty = Var { var = a; _ }; _ }, { ty = Var { var = b; _ }; _ })
    when Common.Type_var.equal a b ->
      Ok subst
  (* *)
  | Typing.Type.({ ty = Var { var = v; _ }; _ }, ty')
  | Typing.Type.(ty', { ty = Var { var = v; _ }; _ }) ->
      [%loga.debug
        "Unify var(%s) -> ty(%s)"
          (Common.Type_var.to_string v)
          (Typing.Type.to_string ty')];

      let ty_subst = Map.add_exn ty_subst ~key:v ~data:ty' in
      Ok Typing.Subst.{ subst with ty_subst }
  | _ -> failwith "[ICE]"

let as_applied ty =
  match ty with Typing.Type.{ ty = Args { recv; _ }; _ } -> recv | _ -> ty

let rec unify_elem ~span ~(subst : Typing.Subst.t) ~preconds lhs_ty rhs_ty :
    (Typing.Subst.t, Typer_err.t) Result.t =
  let open Result.Let_syntax in
  let Typing.Subst.{ ty_subst; ki_subst; _ } = subst in

  let s_lhs_ty = Typing.Subst.subst_type subst lhs_ty in
  let s_rhs_ty = Typing.Subst.subst_type subst rhs_ty in
  match (s_lhs_ty, s_rhs_ty) with
  (* *)
  | Typing.Type.
      ( { ty = Var { var = a; bound = BoundWeak; _ }; _ },
        { ty = Var { var = b; bound = BoundWeak; _ }; _ } )
    when not (Common.Type_var.equal a b) ->
      [%loga.debug
        "Unify var(W%s) = var(W%s)"
          (Common.Type_var.to_string a)
          (Common.Type_var.to_string b)];

      let ty_subst = Map.add_exn ty_subst ~key:a ~data:s_rhs_ty in
      Ok Typing.Subst.{ subst with ty_subst; ki_subst }
  (* *)
  | Typing.Type.({ ty = Var { var = a; _ }; _ }, { ty = Var { var = b; _ }; _ })
    when Common.Type_var.equal a b ->
      Ok subst
  (* *)
  | Typing.Type.(({ ty = Var { var = v; bound = BoundWeak; _ }; _ } as vty), ty')
  | Typing.Type.(ty', ({ ty = Var { var = v; bound = BoundWeak; _ }; _ } as vty))
    ->
      [%loga.debug
        "Unify var(W%s) -> ty(%s)"
          (Common.Type_var.to_string v)
          (Typing.Type.to_string ty')];

      let%bind () =
        List.fold_result preconds ~init:() ~f:(fun _ cond ->
            let Typing.Pred.{ cond_trait = src; cond_var = dst } = cond in
            let dst_var_id = Typing.Type.assume_var_id dst in
            if Poly.equal dst_var_id v then
              let found = judge_subtype ~span ~subst ~src ~ty:ty' in
              match found with
              | true -> Ok ()
              | false ->
                  let kind = Typer_err.ErrUnify in
                  let diff = Typer_err.diff_of_types ~subst s_lhs_ty s_rhs_ty in
                  let e = Typer_err.{ diff; kind; nest = None } in
                  Error e
            else Ok ())
      in

      let ty_subst = Map.add_exn ty_subst ~key:v ~data:ty' in
      Ok Typing.Subst.{ subst with ty_subst }
  (* *)
  | Typing.Type.({ ty = Var { var = v; bound = BoundForall; _ }; _ }, ty')
  | Typing.Type.(ty', { ty = Var { var = v; bound = BoundForall; _ }; _ }) ->
      let kind = Typer_err.ErrUnify in
      let diff = Typer_err.diff_of_types ~subst s_lhs_ty s_rhs_ty in
      let e = Typer_err.{ diff; kind; nest = None } in
      Error e
  (* *)
  | Typing.Type.(a, { ty = Args { recv = b; _ }; _ }) ->
      unify_elem ~span ~subst ~preconds a b
  | Typing.Type.({ ty = Args { recv = b; _ }; _ }, a) ->
      unify_elem ~span ~subst ~preconds b a
  (* *)
  | Typing.Type.
      ( { ty = Array { elem = a_elem; n = a_n }; _ },
        { ty = Array { elem = b_elem; n = b_n }; _ } ) ->
      [%loga.debug "Unify array() = array()"];
      let%bind subst =
        if a_n = b_n then
          unify_elem ~span ~subst ~preconds a_elem b_elem
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
        unify_elem ~span ~subst ~preconds a_elem b_elem
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
        unify_elem ~span ~subst ~preconds a b
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
                    unify_elem ~span ~subst ~preconds a_param b_param
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
              unify_elem ~span ~subst ~preconds a_ret b_ret
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
  | Typing.Type.(MutVar a, MutVar b) when not (Common.Type_var.equal a b) ->
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
  | Typing.Type.(LinkageVar a, LinkageVar b)
    when not (Common.Type_var.equal a b) ->
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

and judge_subtype ~span ~subst ~src ~ty =
  let trait_id = Typing.Type.assume_trait_id src in
  [%loga.debug
    "Is %s a subclass of %s / %s" (Typing.Type.to_string ty)
      (Typing.Type.to_string src)
      (Common.Type_var.to_string trait_id)];

  let found =
    let relations = Typing.Subst.find_subtype_rels subst trait_id in
    List.exists relations ~f:(fun rel ->
        let Typing.Subst.{ sub_target_ty; _ } = rel in
        [%loga.debug
          "rel? %s <= %s" (Typing.Type.to_string ty)
            (Typing.Type.to_string sub_target_ty)];

        match unify_elem ~span ~subst ~preconds:[] sub_target_ty ty with
        | Ok _ -> true
        | _ -> false)
    (*

    List.exists relations ~f:(fun rel ->
        [%loga.debug
          "rel? %s <= %s"
            (Typing.Type.to_string rel.Typing.Subst.sub)
            (Typing.Type.to_string rel.Typing.Subst.super)];
        [%loga.debug
          "rel! %s <= %s"
            (Typing.Type.to_string sub)
            (Typing.Type.to_string super)];
        match unify_elem ~span ~subst ~preconds:[] sub rel.Typing.Subst.sub with
        | Ok _ -> true
        | Error _ -> false)*)
  in

  found

let unify2 ~span (subst : Typing.Subst.t) preds ~from ~to_ :
    (Typing.Subst.t, Diagnostics.Elem.t) Result.t =
  let open Result.Let_syntax in
  let%bind subst =
    unify_elem ~span ~subst ~preconds:preds from to_
    |> Result.map_error ~f:(fun detail ->
           let e = new Reasons.type_mismatch ~detail in
           Diagnostics.Elem.error ~span e)
  in
  Ok subst

let unify ~span (subst : Typing.Subst.t) ~from ~to_ =
  unify2 ~span subst [] ~from ~to_

(*
let relate ~span (subst : Typing.Subst.t) trait_var_id ~entry =
  (* TODO: fix *)
  let (super_trait_id, super_implicit) =
    match super with
    | Typing.Type.
        { ty = Args { recv; args = [ { apply_dst_ty = implicit; _ } ] }; _ } ->
        let var_id = Typing.Type.assume_var_id recv in
        (var_id, implicit)
    | _ -> failwith "[ICE]"
  in
  let (sub_trait_id, sub_implicit) =
    match sub with
    | Typing.Type.
        { ty = Args { recv; args = [ { apply_dst_ty = implicit; _ } ] }; _ } ->
        let var_id = Typing.Type.assume_var_id recv in
        (var_id, implicit)
    | _ -> failwith "[ICE]"
  in
  let var_id =
    match (sub_trait_id, super_trait_id) with
    | (a, b) when a = b -> a
    | _ -> failwith "[ICE]"
  in
  let subst =
    Typing.Subst.append_subtype subst var_id ~sub:sub_implicit
      ~super:super_implicit
  in
  Ok subst
 *)
