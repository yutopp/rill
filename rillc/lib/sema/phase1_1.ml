(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module Span = Common.Span
module Diagnostics = Common.Diagnostics
module Ast = Syntax.Ast
module TopAst = Phase1.TopAst

module IAst = struct
  type t = {
    kind : kind_t;
    env : Env.t; [@sexp.opaque]
    span : Span.t; [@sexp.opaque]
  }

  and kind_t = Module of t list | Func [@@deriving sexp_of]
end

type ctx_t = { ds : Diagnostics.t; mutable subst : Typing.Subst.t }

let context ~ds ~subst = { ds; subst }

let rec declare_toplevels ~ctx ast : (IAst.t, Diagnostics.Elem.t) Result.t =
  let open Result.Let_syntax in
  match ast with
  (* *)
  | TopAst.{ kind = Module nodes; span; env } ->
      let%bind nodes_rev =
        List.fold_result nodes ~init:[] ~f:(fun mapped node ->
            match declare_toplevels ~ctx node with
            | Ok node' -> Ok (node' :: mapped)
            | Error d ->
                Diagnostics.append ctx.ds d;
                (* skip inserting *)
                Ok mapped)
      in
      Ok IAst.{ kind = Module (List.rev nodes_rev); env; span }
  (* *)
  | TopAst.{ kind = Decl body; span; env }
  | TopAst.{ kind = Def body; span; env } ->
      declare ~ctx ~env body

and declare ~ctx ~env ast : (IAst.t, Diagnostics.Elem.t) Result.t =
  let open Result.Let_syntax in
  match ast with
  (* *)
  | Ast.{ kind = DeclExternFunc { name; params; ret_ty; _ }; span }
  | Ast.{ kind = DeclFunc { name; params; ret_ty; _ }; span }
  | Ast.{ kind = DefFunc { name; params; ret_ty; _ }; span } ->
      let%bind params_tys =
        List.fold_result params ~init:[] ~f:(fun ps param ->
            match param with
            | Ast.{ kind = ParamDecl { name; ty_spec }; span } ->
                let%bind () = Guards.guard_dup_value ~span (Some env) name in
                let%bind spec_ty = lookup_type ~ctx ~env ty_spec in
                let venv =
                  Env.create name ~parent:(Some env) ~ty_w:(Env.T spec_ty)
                in
                Env.insert_value env venv;
                Ok (spec_ty :: ps)
            | _ -> failwith "[ICE]")
        |> Result.map ~f:List.rev
      in
      let%bind ret_ty = lookup_type ~ctx ~env ret_ty in
      let func_ty = Typing.Type.{ ty = Func (params_tys, ret_ty); span } in

      let%bind subst = Typer.unify ~span ctx.subst func_ty (Env.type_of env) in
      ctx.subst <- subst;

      Ok IAst.{ kind = Func; env; span }
  (* *)
  | Ast.{ span; _ } ->
      let e =
        new Common.Reasons.internal_error
          ~message:"Not supported decl node (phase1_1)"
      in
      let elm = Diagnostics.Elem.error ~span e in
      Error elm

and lookup_type ~ctx ~env ast : (Typing.Type.t, Diagnostics.Elem.t) Result.t =
  let open Result.Let_syntax in
  match ast with
  | Ast.{ kind = ID name; span } ->
      let%bind env =
        Env.lookup_type env name
        |> Result.map_error ~f:(fun _trace ->
               let e = new Common.Reasons.id_not_found ~name in
               let elm = Diagnostics.Elem.error ~span e in
               elm)
      in
      Ok (Env.type_of env)
  (* *)
  | Ast.{ span; _ } -> failwith ""
