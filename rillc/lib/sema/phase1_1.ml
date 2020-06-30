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

type ctx_t = {
  parent : Env.t option;
  ds : Diagnostics.t;
  mutable subst : Typing.Subst.t;
  builtin : Builtin.t;
}

let context ~ds ~subst ~builtin ~external_pkgs_env =
  { parent = None; ds; subst; builtin }

let rec declare_toplevels ~ctx ast : (unit, Diagnostics.Elem.t) Result.t =
  let open Result.Let_syntax in
  match ast with
  (* *)
  | TopAst.{ kind = Module { nodes; env }; span } ->
      let%bind (ctx', nodes_rev) =
        List.fold_result nodes ~init:(ctx, []) ~f:(fun (ctx, mapped) node ->
            let ctx' = { ctx with parent = Some env } in
            match declare_toplevels ~ctx:ctx' node with
            | Ok node' -> Ok (ctx', node' :: mapped)
            | Error d ->
                Diagnostics.append ctx.ds d;
                (* skip inserting *)
                Ok (ctx, mapped))
      in
      ctx.subst <- ctx'.subst;
      Ok ()
  (* *)
  | TopAst.{ kind = WithEnv { node; env }; span } -> with_env ~ctx ~env node
  | TopAst.{ kind = PassThrough { node }; span } -> pass_through ~ctx node

and with_env ~ctx ~env ast : (unit, Diagnostics.Elem.t) Result.t =
  let open Result.Let_syntax in
  match ast with
  (* *)
  | Ast.{ kind = DeclExternFunc { name; params; ret_ty; _ }; span }
  | Ast.{ kind = DeclFunc { name; params; ret_ty; _ }; span }
  | Ast.{ kind = DefFunc { name; params; ret_ty; _ }; span } ->
      let%bind params_tys =
        List.fold_result params ~init:[] ~f:(fun ps param ->
            match param with
            | Ast.{ kind = ParamDecl { attr; name; ty_spec }; span } ->
                let%bind () = Guards.guard_dup_value ~span env name in

                let%bind spec_ty =
                  let%bind ty = lookup_type ~env ctx.builtin ty_spec in
                  let binding_mut = Mut.mutability_of attr in
                  Ok Typing.Type.{ ty with binding_mut }
                in
                let visibility = Env.Public in
                let venv =
                  Env.create name ~parent:(Some env) ~visibility ~ty:spec_ty
                    ~kind:Env.Val ~lookup_space:Env.LkLocal
                in
                Env.insert env venv |> Phase1.assume_new;
                Ok (spec_ty :: ps)
            | _ -> failwith "[ICE]")
        |> Result.map ~f:List.rev
      in
      let%bind ret_ty =
        let%bind ty = lookup_type ~env ctx.builtin ret_ty in
        let binding_mut = Typing.Type.MutImm in
        Ok Typing.Type.{ ty with binding_mut }
      in

      let func_ty =
        let linkage = Functions.linkage_of ast in
        let binding_mut = Typing.Type.MutImm in
        Typing.Type.
          {
            ty = Func { params = params_tys; ret = ret_ty; linkage };
            binding_mut;
            span;
          }
      in

      let%bind subst = Typer.unify ~span ctx.subst func_ty (Env.type_of env) in
      ctx.subst <- subst;

      Ok ()
  (* *)
  | Ast.{ kind = DeclExternStaticVar { attr; name; ty_spec }; span } ->
      let%bind ty =
        let%bind ty = lookup_type ~env ctx.builtin ty_spec in
        let binding_mut = Mut.mutability_of attr in
        Ok Typing.Type.{ ty with binding_mut }
      in

      let%bind subst = Typer.unify ~span ctx.subst ty (Env.type_of env) in
      ctx.subst <- subst;

      Ok ()
  (* *)
  | Ast.{ kind = DefTypeAlias { alias_ty; _ }; span } ->
      let%bind alias =
        lookup_type ~env ctx.builtin alias_ty |> Result.map ~f:Phase1.to_type
      in

      let%bind subst = Typer.unify ~span ctx.subst alias (Env.type_of env) in
      ctx.subst <- subst;

      Ok ()
  (* *)
  | Ast.{ kind = DefStruct { name }; span } ->
      let struct_ty =
        let tag = Typing.Subst.fresh_struct_tag ctx.subst in
        let binding_mut = Typing.Type.MutMut in
        let inner = Typing.Type.{ ty = Struct { tag }; binding_mut; span } in
        ctx.builtin.Builtin.type_ inner
      in

      let%bind subst =
        Typer.unify ~span ctx.subst struct_ty (Env.type_of env)
      in
      ctx.subst <- subst;

      Ok ()
  (* *)
  | Ast.{ span; _ } ->
      let e =
        new Diagnostics.Reasons.internal_error
          ~message:"Not supported decl node (phase1_1, with_env)"
      in
      let elm = Diagnostics.Elem.error ~span e in
      Error elm

and pass_through ~ctx ast =
  let open Result.Let_syntax in
  match ast with
  (* *)
  | Ast.{ kind = Import { pkg; mods }; span } ->
      let parent_env =
        match ctx.parent with Some env -> env | None -> failwith "[ICE]"
      in
      let%bind mod_env = find_mod ?lookup:(Some true) ~env:parent_env pkg in
      let rec f mods env loadable_envs =
        match mods with
        | [] -> Ok [ env ]
        | [ last_id ] ->
            [%loga.debug "leaf: %s" (Ast.show last_id)];
            let%bind envs = find_mods_with_wildcard ~env last_id in
            Ok (List.join [ envs; loadable_envs ])
        | cont :: rest ->
            [%loga.debug "node: %s" (Ast.show cont)];
            let%bind env = find_mod ~env cont in
            f rest env loadable_envs
      in
      let%bind envs = f mods mod_env [] in
      let pub_envs =
        List.filter envs ~f:(fun env ->
            Poly.equal env.Env.visibility Env.Public)
      in

      Option.iter ctx.parent ~f:(fun penv ->
          List.iter pub_envs ~f:(fun env ->
              let name = env.Env.name in
              let visibility = Env.Private (* TODO: fix *) in
              (* A type of the imported term must be decided until this time *)
              let src_root_mod = Env.root_mod_of ~scoped:true env in
              let subst = Mod.subst_of src_root_mod in
              let ty = Typing.Subst.subst_type subst (Env.type_of env) in
              (* TODO: assert *)
              let aenv =
                Env.create name ~parent:(Some penv) ~visibility ~ty
                  ~kind:(Env.Alias env) ~lookup_space:env.Env.lookup_space
              in
              (*[%loga.debug "loadable env: %s" (Env.show env)];*)
              Env.insert penv aenv |> Phase1.assume_new));

      Ok ()
  (* *)
  | Ast.{ span; _ } ->
      let e =
        new Diagnostics.Reasons.internal_error
          ~message:"Not supported decl node (phase1_1, pass_through)"
      in
      let elm = Diagnostics.Elem.error ~span e in
      Error elm

and find_mod ?lookup ~env ast =
  let open Result.Let_syntax in
  match ast with
  (* *)
  | Ast.{ kind = ID name; span } ->
      let%bind env =
        match lookup with
        (* *)
        | Some _ ->
            let%bind (env, _depth) =
              Env.lookup_meta env name
              |> Result.map_error ~f:(fun trace ->
                     let candidates =
                       (* TODO *)
                       trace
                       |> List.map ~f:(fun e ->
                              let Env.{ name; _ } = e in
                              name)
                     in
                     let e =
                       new Diagnostics.Reasons.id_not_found ~name ~candidates
                     in
                     let elm = Diagnostics.Elem.error ~span e in
                     elm)
            in
            Ok env
        (* *)
        | None ->
            Env.find_meta env name |> Result.of_option ~error:()
            |> Result.map_error ~f:(fun () ->
                   let candidates = Env.meta_keys env in
                   let e =
                     new Diagnostics.Reasons.id_not_found ~name ~candidates
                   in
                   let elm = Diagnostics.Elem.error ~span e in
                   elm)
      in
      Ok env
  (* *)
  | _ ->
      let s = Ast.show ast in
      failwith (Printf.sprintf "unexpected token (find_mod): %s" s)

and find_mods_with_wildcard ~env ast =
  match ast with
  (* *)
  | Ast.{ kind = IDWildcard; span } -> Ok (Env.collect_all env)
  (* *)
  | _ -> find_mod ~env ast |> Result.map ~f:(fun e -> [ e ])

and lookup_type ~env builtin ast : (Typing.Type.t, Diagnostics.Elem.t) Result.t
    =
  let open Result.Let_syntax in
  match ast with
  (* *)
  | Ast.{ kind = ID name; span } ->
      let%bind (env, _depth) =
        Env.lookup_type env name
        |> Result.map_error ~f:(fun _trace ->
               let candidates = [] (* TODO *) in
               let e = new Diagnostics.Reasons.id_not_found ~name ~candidates in
               let elm = Diagnostics.Elem.error ~span e in
               elm)
      in
      let ty =
        match Env.type_of env with
        | Typing.Type.{ ty = Type ty; _ } -> ty
        | _ -> failwith (Printf.sprintf "[ICE] = %s" name)
      in
      Ok Typing.Type.{ ty with span }
  (* *)
  | Ast.{ kind = TypeExprArray { elem; len }; span } ->
      let%bind elem_ty = lookup_type ~env builtin elem in
      (* TODO: fix 4 *)
      let ty = builtin.Builtin.array_ elem_ty 4 in
      Ok Typing.Type.{ ty with span }
  (* *)
  | Ast.{ kind = TypeExprPointer { attr; elem }; span } ->
      let%bind elem_ty = lookup_type ~env builtin elem in
      let mut = Mut.mutability_of attr in
      let ty = builtin.Builtin.pointer_ mut elem_ty in
      Ok Typing.Type.{ ty with span }
  (* *)
  | Ast.{ span; _ } -> failwith "unexpected token (lookup_type)"

and lookup_path ~env ast : (Env.t, Diagnostics.Elem.t) Result.t =
  let open Result.Let_syntax in
  match ast with
  (* *)
  | Ast.{ kind = Path { root; elems }; span; _ } ->
      let rec find_elems env elems =
        match elems with
        | [] -> Ok env
        | x :: xs ->
            let%bind nenv = lookup_path ~env x in
            find_elems nenv xs
      in
      let%bind root_env = lookup_path ~env root in
      find_elems root_env elems
  (* *)
  | Ast.{ kind = ID name; span } ->
      let%bind (envs, _depth) =
        Env.lookup_multi env name
        |> Result.map_error ~f:(fun _trace ->
               let candidates = [] (* TODO *) in
               let e = new Diagnostics.Reasons.id_not_found ~name ~candidates in
               let elm = Diagnostics.Elem.error ~span e in
               elm)
      in
      let%bind env = match envs with [ e ] -> Ok e | _ -> failwith "" in
      Ok env
  (* *)
  | Ast.{ span; _ } -> failwith "unexpected token (lookup_path)"

let to_nested_chain env =
  let module Chain = Common.Chain in
  let merge_chain cs env =
    let kind =
      match env.Env.kind with
      | Env.M _ -> Some Chain.Nest.Module
      | Env.Ty -> Some Chain.Nest.Type
      | Env.Val -> Some Chain.Nest.Var
      | _ -> None
    in
    match kind with
    | Some kind ->
        let n = Chain.Nest.{ name = env.Env.name; kind } in
        Chain.Nest.join_rev cs n
    | None -> cs
  in

  let rec f env cs =
    match env.Env.kind with
    | Env.Alias aenv -> f aenv cs
    | _ -> (
        let cs = merge_chain cs env in
        match env.Env.parent with None -> cs | Some penv -> f penv cs )
  in
  f env (Chain.Nest.create ())

let to_chains env =
  let module Chain = Common.Chain in
  match env.Env.lookup_space with
  | Env.LkLocal -> Chain.Local env.Env.name
  | Env.LkGlobal -> Chain.Global (to_nested_chain env)
