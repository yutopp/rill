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
module TopAst = Phase1.TopAst

(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

type ctx_t = {
  m : Mod.t;
  parent : Env.t option;
  ds : Diagnostics.t;
  mutable subst : Typing.Subst.t;
  builtin : Builtin.t;
}

let context ~m ~subst ~builtin =
  let Mod.{ ds; _ } = m in
  { m; parent = None; ds; subst; builtin }

let rec declare_impls ~ctx ast : (ctx_t * TopAst.t, Diagnostics.Elem.t) Result.t
    =
  let open Result.Let_syntax in
  match ast with
  (* *)
  | TopAst.{ kind = Module { stmts }; span } ->
      let Mod.{ menv; _ } = ctx.m in
      let ctx' = { ctx with parent = Some menv } in
      let%bind (ctx', stmts) = declare_impls ~ctx:ctx' stmts in
      Ok (ctx', TopAst.{ kind = Module { stmts }; span })
  (* *)
  | TopAst.{ kind = Stmts { nodes }; span } ->
      let%bind (ctx', nodes_rev) =
        List.fold_result nodes ~init:(ctx, []) ~f:(fun (ctx, mapped) node ->
            match declare_impls ~ctx node with
            | Ok (ctx', node') -> Ok (ctx', node' :: mapped)
            | Error d ->
                Diagnostics.append ctx.ds d;
                (* skip inserting *)
                Ok (ctx, mapped))
      in
      let nodes = nodes_rev |> List.rev in
      Ok (ctx', TopAst.{ kind = Stmts { nodes }; span })
  (* *)
  | TopAst.{ kind = LazyDecl { node; penv }; span } -> lazy_decl ~ctx ~penv node
  | _ -> Ok (ctx, ast)

and lazy_decl ~ctx ~penv ast =
  let open Result.Let_syntax in
  match ast with
  (* *)
  | Ast.{ kind = DefImplFor { trait; for_ty; decls }; span } as impl ->
      let for_ty_base = for_ty in
      let%bind (for_ty, for_predicates, for_implicits, for_vars, for_env) =
        Phase1_1.lookup_type' ~env:penv ~subst:ctx.subst ctx.builtin for_ty
      in
      let for_ty =
        let args =
          Phase1_1.id_type_vars ~implicits:for_implicits ~vars:for_vars
        in
        match args with
        | [] -> for_ty
        | args -> Typing.Type.{ for_ty with ty = Args { recv = for_ty; args } }
      in

      (*let env_ty_sc = Env.type_sc_of env in
        let (Typing.Scheme.ForAll
              { implicits = env_implicits; vars = env_vars; ty = env_ty }) =
          env_ty_sc
        in*)
      let%bind ( trait_ty,
                 trait_predicates,
                 trait_implicits,
                 trait_vars,
                 trait_env ) =
        Phase1_1.lookup_type' ~env:penv ~subst:ctx.subst ctx.builtin trait
      in
      let trait_id = Typing.Type.assume_trait_id trait_ty in
      let trait_ty =
        let args =
          Phase1_1.id_type_vars ~implicits:trait_implicits ~vars:trait_vars
        in
        match args with
        | [] -> failwith "[ICE] trait must have implicit params at least 1"
        | args ->
            Typing.Type.{ trait_ty with ty = Args { recv = trait_ty; args } }
      in

      [%loga.debug
        "impl Trait: %s / %d" (Typing.Type.to_string trait_ty) trait_id];
      [%loga.debug "impl For: %s" (Typing.Type.to_string for_ty)];

      let ty = Typing.Type.to_type_ty for_ty in
      let pty = Typing.Pred.of_type ty in
      let ty_sc = Typing.Scheme.of_ty pty in
      let visibility = Env.Public in

      (* DO NOT INSERTS the tenv TO the PARENT *)
      let tenv =
        (* TODO: check duplication *)
        let name = Ast.type_decl_name for_ty_base in
        Env.create name ~parent:(Some penv) ~visibility ~ty_sc ~kind:Env.Impl
          ~lookup_space:Env.LkGlobal
      in

      let%bind (subst, decls) =
        let%bind p1ast =
          let ctx =
            Phase1.context ~m:ctx.m ~subst:ctx.subst ~builtin:ctx.builtin
          in
          Phase1.collect_toplevels ~ctx:Phase1.{ ctx with parent = tenv } decls
        in

        let%bind ctx =
          let ctx =
            Phase1_1.context ~m:ctx.m ~subst:ctx.subst ~builtin:ctx.builtin
          in
          Phase1_1.declare_toplevels
            ~ctx:Phase1_1.{ ctx with parent = Some tenv }
            p1ast
        in

        Ok (Phase1_1.(ctx.subst), p1ast)
      in
      let ctx = { ctx with subst } in

      let trait_name = Name.to_nested_chain' trait_env ctx.subst in
      let for_name = Name.to_nested_chain' for_env ctx.subst in

      [%loga.debug
        "impl Trait env: %s"
          (Name.to_string ~to_s:Typing.Type.to_string trait_name)];
      [%loga.debug
        "impl For env: %s" (Name.to_string ~to_s:Typing.Type.to_string for_name)];

      let mapping =
        let nodes =
          match decls with
          | TopAst.{ kind = Stmts { nodes }; _ } -> nodes
          | _ -> failwith ""
        in
        let mapping =
          List.fold_left nodes ~init:[] ~f:(fun mapping node ->
              match node with
              | TopAst.{ kind = WithEnv { node; env; _ }; _ } ->
                  (* TODO: check that decls satisfy traits constraints *)
                  (* TODO: disallow generics *)
                  (* TODO: check all members are defined *)
                  let nest = Name.to_nested_chain' env ctx.subst in
                  let Common.Chain.Nest.{ last; _ } = nest in
                  [%loga.debug
                    "-> %s => %s"
                      (Common.Chain.Layer.to_string ~to_s:Typing.Type.to_string
                         last)
                      (Name.to_string ~to_s:Typing.Type.to_string nest)];
                  (last, nest) :: mapping
              | _ -> failwith "")
        in
        mapping
      in

      let impl_record = Impl.{ trait_name; for_ty; mapping } in

      let subst =
        let impl_entry = Typing.Subst.{ sub_target_ty = for_ty } in
        Typing.Subst.append_impl subst trait_id ~relation:impl_entry
      in
      let ctx = { ctx with subst } in

      Ok
        ( ctx,
          TopAst.
            {
              kind =
                WithEnvAndBody2
                  { node = impl; body = decls; env = tenv; impl_record };
              span;
            } )
  (* *)
  | Ast.{ span; _ } ->
      let e =
        new Diagnostics.Reasons.internal_error
          ~message:"Not supported decl node (phase1_2, pass_through)"
      in
      let elm = Diagnostics.Elem.error ~span e in
      Error elm

and lookup_type ~env ~ctx ast =
  let builtin = ctx.builtin in
  Phase1_1.lookup_type_fresh ~env ~subst:ctx.subst ~builtin ast
