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
    | Module of { stmts : t }
    | Stmts of { nodes : t list }
    | WithEnv of { node : Ast.t; env : Env.t }
    | WithEnvAndBody of { node : Ast.t; body : t; env : Env.t }
    | LazyDecl of { node : Ast.t; penv : Env.t }
    | WithEnvAndBody2 of {
        node : Ast.t;
        body : t;
        env : Env.t;
        impl_record : Impl.t;
      }
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

let introduce_builtin penv builtin =
  let register name inner_ty_gen =
    let ty = Typing.Type.to_type_ty (inner_ty_gen ~span:Span.undef) in
    let pty = Typing.Pred.of_type ty in
    let ty_sc = Typing.Scheme.of_ty pty in
    let env =
      Env.create ~is_builtin:true name ~parent:None ~visibility:Env.Private
        ~ty_sc ~kind:Env.Ty ~lookup_space:Env.LkGlobal
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
  | Ast.{ kind = Module stmts; span } ->
      (* TODO: fix *)
      let menv = ctx.parent in
      introduce_builtin menv ctx.builtin;

      let%bind stmts =
        let ctx' = { ctx with parent = menv } in
        collect_toplevels ~ctx:ctx' stmts
      in
      Ok TopAst.{ kind = Module { stmts }; span }
  (* *)
  | Ast.{ kind = Stmts nodes; span } ->
      let%bind nodes =
        List.fold_result nodes ~init:[] ~f:(fun mapped node ->
            match collect_toplevels ~ctx node with
            | Ok node' -> Ok (node' :: mapped)
            | Error d ->
                Diagnostics.append ctx.ds d;
                Ok mapped)
        |> Result.map ~f:List.rev
      in
      Ok TopAst.{ kind = Stmts { nodes }; span }
  (* *)
  | Ast.{ kind = Import _; span } as i ->
      Ok TopAst.{ kind = PassThrough { node = i }; span }
  (* *)
  | Ast.{ kind = DefTypeAlias { name; _ }; span } as alias ->
      let penv = ctx.parent in
      let%bind () = Guards.guard_dup_value ~span penv name in

      let ty =
        let inner = Typing.Subst.fresh_ty ~span ctx.subst in
        ctx.builtin.Builtin.type_ ~span inner
      in
      let pty = Typing.Pred.of_type ty in
      let ty_sc = Typing.Scheme.of_ty pty in
      let visibility = Env.Public (* TODO: fix *) in
      let tenv =
        Env.create name ~parent:(Some penv) ~visibility ~ty_sc ~kind:Env.Ty
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
      let pty = Typing.Pred.of_type ty in
      let ty_sc = Typing.Scheme.of_ty pty in
      let visibility = Env.Public (* TODO: fix *) in
      let fenv =
        Env.create name ~parent:(Some penv) ~visibility ~ty_sc ~kind:Env.Val
          ~lookup_space:Env.LkGlobal
      in
      Env.insert penv fenv |> assume_new;

      Ok TopAst.{ kind = WithEnv { node = decl; env = fenv }; span }
  (* *)
  | Ast.{ kind = DeclExternFunc { name; params; ret_ty; symbol_name; _ }; span }
    as decl ->
      let penv = ctx.parent in
      let%bind () = Guards.guard_dup_value ~span penv name in

      let ty_params = (* TODO *) [] in

      let linkage = Functions.linkage_of decl in

      let ty_sc =
        preconstruct_func_ty_sc ~ctx ~span ~linkage ~ty_params ~params ~ret_ty
      in
      let visibility = Env.Public (* TODO: fix *) in
      let fenv =
        Env.create name ~parent:(Some penv) ~visibility ~ty_sc ~kind:Env.Val
          ~lookup_space:Env.LkGlobal
      in
      Env.insert penv fenv |> assume_new;

      Ok TopAst.{ kind = WithEnv { node = decl; env = fenv }; span }
  (* *)
  | ( Ast.{ kind = DeclFunc { name; ty_params; params; ret_ty; _ }; span } as
    decl )
  | (Ast.{ kind = DefFunc { name; ty_params; params; ret_ty; _ }; span } as decl)
    ->
      let penv = ctx.parent in
      let%bind () = Guards.guard_dup_value ~span penv name in

      let linkage = Functions.linkage_of decl in

      let ty_sc =
        preconstruct_func_ty_sc ~ctx ~span ~linkage ~ty_params ~params ~ret_ty
      in
      let visibility = Env.Public (* TODO: fix *) in
      let fenv =
        Env.create name ~parent:(Some penv) ~visibility ~ty_sc ~kind:Env.Val
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
        ctx.builtin.Builtin.type_ ~span inner
      in
      let pty = Typing.Pred.of_type ty in
      let ty_sc = Typing.Scheme.of_ty pty in
      let visibility = Env.Public (* TODO: fix *) in
      let tenv =
        Env.create name ~parent:(Some penv) ~visibility ~ty_sc ~kind:Env.Ty
          ~lookup_space:Env.LkGlobal
      in
      Env.insert penv tenv |> assume_new;

      Ok TopAst.{ kind = WithEnv { node = decl; env = tenv }; span }
  (* *)
  | Ast.{ kind = DefTrait { name; decls }; span } as decl ->
      let penv = ctx.parent in
      let%bind () = Guards.guard_dup_value ~span penv name in

      (* T *)
      let ty =
        let inner = Typing.Subst.fresh_ty ~span ctx.subst in
        ctx.builtin.Builtin.type_ ~span inner
      in
      let pty = Typing.Pred.of_type ty in
      let ty_sc = Typing.Scheme.of_ty pty in
      let visibility = Env.Public (* TODO *) in
      let tenv =
        Env.create name ~parent:(Some penv) ~visibility ~ty_sc ~kind:Env.Trait
          ~lookup_space:Env.LkGlobal
      in
      (* a *)
      let implicits =
        let inner = Typing.Subst.fresh_forall_ty ~span ~label:"t" ctx.subst in
        [ inner ]
      in
      Env.append_implicits tenv implicits;

      (* Trait :: T!(_a) *)
      Env.insert penv tenv |> assume_new;

      let%bind decls =
        let ctx = { ctx with parent = tenv } in
        collect_toplevels ~ctx decls
      in

      (* Add 'Self' *)
      let self_env =
        let name = "Self" in
        let ty_sc =
          List.hd_exn implicits |> Typing.Type.to_type_ty |> Typing.Pred.of_type
          |> Typing.Scheme.of_ty
        in
        Env.create name ~parent:(Some tenv) ~visibility ~ty_sc ~kind:Env.Ty
          ~lookup_space:Env.LkLocal
      in
      Env.insert_type tenv self_env |> assume_new;

      Ok
        TopAst.
          {
            kind = WithEnvAndBody { node = decl; body = decls; env = tenv };
            span;
          }
  (* *)
  | Ast.{ kind = DefImplFor _; span } as decl ->
      let penv = ctx.parent in
      Ok TopAst.{ kind = LazyDecl { node = decl; penv }; span }
  (* *)
  | Ast.{ span; _ } ->
      let e =
        new Diagnostics.Reasons.internal_error
          ~message:"Not supported node (phase1)"
      in
      let elm = Diagnostics.Elem.error ~span e in
      Error elm

and preconstruct_func_ty_sc ~ctx ~span ~linkage ~ty_params ~params ~ret_ty :
    Typing.Scheme.t =
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
  let ty =
    Typing.Type.
      {
        ty = Func { params = params_tys; ret = ret_ty; linkage };
        binding_mut;
        span;
      }
  in
  let pty = Typing.Pred.of_type ty in

  let vars =
    List.map ty_params ~f:(fun ty_param ->
        match ty_param with
        | Ast.{ kind = TyParamDecl { name }; span } ->
            Typing.Subst.fresh_forall_ty ~span ~label:name ctx.subst
        | _ -> failwith "[ICE]")
  in
  Typing.Scheme.ForAll { implicits = []; vars; ty = pty }
