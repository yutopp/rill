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

module TopAst = struct
  type t = {
    kind : kind_t;
    env : Env.t; [@sexp.opaque]
    span : Span.t; [@sexp.opaque]
  }
  [@@deriving sexp_of]

  and kind_t = Module of t list | Decl of Ast.t | Def of Ast.t
end

type ctx_t = {
  parent : Env.t option;
  ds : Diagnostics.t;
  subst : Typing.Subst.t;
  builtin : Builtin.t;
}

let context ~parent ~ds ~subst ~builtin = { parent; ds; subst; builtin }

type result_t = (TopAst.t, Diagnostics.Elem.t) Result.t

(* TODO: create them elsewhare *)
let introduce_prelude penv builtin =
  let register name ty =
    let env = Env.create name ~parent:None ~ty_w:(Env.T ty) in
    Env.insert_type penv env
  in
  register "i32" builtin.Builtin.i32_;
  register "string" builtin.Builtin.string_;
  register "unit" builtin.Builtin.unit_;
  ()

let rec collect_toplevels ~ctx ast : (TopAst.t, Diagnostics.Elem.t) Result.t =
  let open Result.Let_syntax in
  match ast with
  (* *)
  | Ast.{ kind = Module nodes; span } ->
      (* TODO: fix *)
      let menv =
        Env.create "" ~parent:ctx.parent ~ty_w:(Env.T ctx.builtin.Builtin.unit_)
      in
      introduce_prelude menv ctx.builtin;
      let%bind nodes_rev =
        List.fold_result nodes ~init:[] ~f:(fun mapped node ->
            let ctx' = { ctx with parent = Some menv } in
            match collect_toplevels ~ctx:ctx' node with
            | Ok node' -> Ok (node' :: mapped)
            | Error d ->
                Diagnostics.append ctx.ds d;
                Ok mapped)
      in
      Ok TopAst.{ kind = Module (List.rev nodes_rev); env = menv; span }
  (* *)
  | ( Ast.{ kind = DeclFunc { name; params; ret_ty; _ }; span }
    | Ast.{ kind = DeclExternFunc { name; params; ret_ty; _ }; span } ) as decl
    ->
      let%bind () = Guards.guard_dup_value ~span ctx.parent name in

      let ty = preconstruct_func_ty ~ctx ~span params ret_ty in
      let fenv = Env.create name ~parent:ctx.parent ~ty_w:(Env.T ty) in
      Option.iter ctx.parent ~f:(fun penv -> Env.insert_value penv fenv);
      Ok TopAst.{ kind = Decl decl; env = fenv; span }
  (* *)
  | Ast.{ kind = DefFunc { name; params; ret_ty; _ }; span } as decl ->
      let%bind () = Guards.guard_dup_value ~span ctx.parent name in

      let ty = preconstruct_func_ty ~ctx ~span params ret_ty in
      let fenv = Env.create name ~parent:ctx.parent ~ty_w:(Env.T ty) in
      Option.iter ctx.parent ~f:(fun penv -> Env.insert_value penv fenv);
      Ok TopAst.{ kind = Def decl; env = fenv; span }
  (* *)
  | Ast.{ span; _ } ->
      let e =
        new Common.Reasons.internal_error ~message:"Not supported node (phase1)"
      in
      let elm = Diagnostics.Elem.error ~span e in
      Error elm

and preconstruct_func_ty ~ctx ~span params ret_ty : Typing.Type.t =
  (* TODO: support generic params *)
  let params_tys =
    List.map params ~f:(fun param ->
        let span = Ast.(param.span) in
        Typing.Subst.fresh_ty ~span ctx.subst)
  in
  let ret_ty =
    let span = Ast.(ret_ty.span) in
    Typing.Subst.fresh_ty ~span ctx.subst
  in
  Typing.Type.{ ty = Func (params_tys, ret_ty); span }
