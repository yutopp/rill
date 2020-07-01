(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module Span = Common.Span
module Ast = Syntax.Ast

module TopAst = struct
  type t = { kind : kind_t; span : Span.t }

  and kind_t =
    | Module of { nodes : t list }
    | WithEnv of { node : Ast.t; env : Env.t }
    | PassThrough of { node : Ast.t }
  [@@deriving show]
end

type ctx_t = {
  parent : Env.t;
  ds : Diagnostics.t;
  subst : Typing.Subst.t;
  builtin : Builtin.t;
}

let context ~m ~subst ~builtin =
  let Mod.{ menv; ds; _ } = m in
  { parent = menv; ds; subst; builtin }

type result_t = (TopAst.t, Diagnostics.Elem.t) Result.t

let assume_new inseted_status =
  match inseted_status with
  | Env.InsertedHiding -> failwith "[ICE] insertion with hiding"
  | _ -> ()

(* TODO: create them elsewhare *)
let to_type ty = Typing.Type.{ ty with ty = Type ty }

let of_type ty =
  match ty with
  | Typing.Type.{ ty = Type ty; _ } -> ty
  | _ -> failwith "[ICE] not type"

let introduce_prelude penv builtin =
  let register name inner_ty =
    let ty = to_type inner_ty in
    let env =
      Env.create name ~parent:None ~visibility:Env.Private ~ty ~kind:Env.Ty
        ~lookup_space:Env.LkGlobal
    in
    Env.insert penv env |> assume_new
  in
  register "bool" builtin.Builtin.bool_;
  register "i8" builtin.Builtin.i8_;
  register "i32" builtin.Builtin.i32_;
  register "i64" builtin.Builtin.i64_;
  register "u64" builtin.Builtin.u64_;
  register "isize" builtin.Builtin.isize_;
  register "usize" builtin.Builtin.usize_;
  register "string" builtin.Builtin.string_;
  register "unit" builtin.Builtin.unit_;
  ()

let rec collect_toplevels ~ctx ast : (TopAst.t, Diagnostics.Elem.t) Result.t =
  let open Result.Let_syntax in
  match ast with
  (* *)
  | Ast.{ kind = Module nodes; span } ->
      (* TODO: fix *)
      let menv = ctx.parent in
      introduce_prelude menv ctx.builtin;

      let%bind nodes =
        List.fold_result nodes ~init:[] ~f:(fun mapped node ->
            let ctx' = { ctx with parent = menv } in
            match collect_toplevels ~ctx:ctx' node with
            | Ok node' -> Ok (node' :: mapped)
            | Error d ->
                Diagnostics.append ctx.ds d;
                Ok mapped)
        |> Result.map ~f:List.rev
      in
      Ok TopAst.{ kind = Module { nodes }; span }
  (* *)
  | Ast.{ kind = Import _; span } as i ->
      Ok TopAst.{ kind = PassThrough { node = i }; span }
  (* *)
  | Ast.{ kind = DefTypeAlias { name; _ }; span } as alias ->
      let penv = ctx.parent in
      let%bind () = Guards.guard_dup_value ~span penv name in

      let ty =
        let inner = Typing.Subst.fresh_ty ~span ctx.subst in
        ctx.builtin.Builtin.type_ inner
      in
      let visibility = Env.Public in
      let tenv =
        Env.create name ~parent:(Some penv) ~visibility ~ty ~kind:Env.Ty
          ~lookup_space:Env.LkGlobal
      in
      Env.insert penv tenv |> assume_new;

      Ok TopAst.{ kind = WithEnv { node = alias; env = tenv }; span }
  (* *)
  | Ast.{ kind = DeclExternStaticVar { attr; name; ty_spec }; span } as decl ->
      let penv = ctx.parent in
      let%bind () = Guards.guard_dup_value ~span penv name in

      let binding_mut = Mut.mutability_of attr in
      let ty =
        let span = Ast.(ty_spec.span) in
        let ty = Typing.Subst.fresh_ty ~span ctx.subst in
        Typing.(Type.{ ty with binding_mut })
      in
      let visibility = Env.Public in
      let fenv =
        Env.create name ~parent:(Some penv) ~visibility ~ty ~kind:Env.Val
          ~lookup_space:Env.LkGlobal
      in
      Env.insert penv fenv |> assume_new;

      Ok TopAst.{ kind = WithEnv { node = decl; env = fenv }; span }
  (* *)
  | Ast.{ kind = DeclExternFunc { name; params; ret_ty; symbol_name }; span } as
    decl ->
      let penv = ctx.parent in
      let%bind () = Guards.guard_dup_value ~span penv name in

      let linkage = Functions.linkage_of decl in

      let ty = preconstruct_func_ty ~ctx ~span ~linkage params ret_ty in
      let visibility = Env.Public in
      let fenv =
        Env.create name ~parent:(Some penv) ~visibility ~ty ~kind:Env.Val
          ~lookup_space:Env.LkGlobal
      in
      Env.insert penv fenv |> assume_new;

      Ok TopAst.{ kind = WithEnv { node = decl; env = fenv }; span }
  (* *)
  | ( Ast.{ kind = DeclFunc { name; params; ret_ty; _ }; span }
    | Ast.{ kind = DefFunc { name; params; ret_ty; _ }; span } ) as decl ->
      let penv = ctx.parent in
      let%bind () = Guards.guard_dup_value ~span penv name in

      let linkage = Functions.linkage_of decl in

      let ty = preconstruct_func_ty ~ctx ~span ~linkage params ret_ty in
      let visibility = Env.Public in
      let fenv =
        Env.create name ~parent:(Some penv) ~visibility ~ty ~kind:Env.Val
          ~lookup_space:Env.LkGlobal
      in
      Env.insert penv fenv |> assume_new;

      Ok TopAst.{ kind = WithEnv { node = decl; env = fenv }; span }
  (* *)
  | Ast.{ kind = DefStruct { name }; span } as decl ->
      let penv = ctx.parent in
      let%bind () = Guards.guard_dup_value ~span penv name in

      let ty =
        let inner = Typing.Subst.fresh_ty ~span ctx.subst in
        ctx.builtin.Builtin.type_ inner
      in
      let visibility = Env.Public in
      let tenv =
        Env.create name ~parent:(Some penv) ~visibility ~ty ~kind:Env.Ty
          ~lookup_space:Env.LkGlobal
      in
      Env.insert penv tenv |> assume_new;

      Ok TopAst.{ kind = WithEnv { node = decl; env = tenv }; span }
  (* *)
  | Ast.{ span; _ } ->
      let e =
        new Diagnostics.Reasons.internal_error
          ~message:"Not supported node (phase1)"
      in
      let elm = Diagnostics.Elem.error ~span e in
      Error elm

and preconstruct_func_ty ~ctx ~span ~linkage params ret_ty : Typing.Type.t =
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
  let binding_mut = Typing.Type.MutImm in
  Typing.Type.
    {
      ty = Func { params = params_tys; ret = ret_ty; linkage };
      binding_mut;
      span;
    }
