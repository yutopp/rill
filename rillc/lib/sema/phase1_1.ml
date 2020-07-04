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
  m : Mod.t;
  parent : Env.t option;
  ds : Diagnostics.t;
  mutable subst : Typing.Subst.t;
  builtin : Builtin.t;
}

let context ~m ~subst ~builtin =
  let Mod.{ ds; _ } = m in
  { m; parent = None; ds; subst; builtin }

let rec declare_toplevels ~ctx ast : (unit, Diagnostics.Elem.t) Result.t =
  let open Result.Let_syntax in
  match ast with
  (* *)
  | TopAst.{ kind = Module { nodes }; span } ->
      let Mod.{ menv; _ } = ctx.m in
      let%bind (ctx', nodes_rev) =
        List.fold_result nodes ~init:(ctx, []) ~f:(fun (ctx, mapped) node ->
            let ctx' = { ctx with parent = Some menv } in
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
  | Ast.{ kind = DeclExternFunc { name; ty_params; params; ret_ty; _ }; span }
  | Ast.{ kind = DefFunc { name; ty_params; params; ret_ty; _ }; span } ->
      let env_ty_sc = Env.type_sc_of env in
      let (Typing.Scheme.ForAll (ty_vars, ty)) = env_ty_sc in

      (* generics params *)
      let%bind () =
        List.zip_exn ty_params ty_vars
        |> List.fold_result ~init:() ~f:(fun _ (ty_param, ty_var) ->
               match ty_param with
               | Ast.{ kind = TyParamDecl { name }; span } ->
                   let%bind () = Guards.guard_dup_value ~span env name in
                   let binding_mut = Typing.Type.MutImm in
                   let inner_ty = ty_var in
                   let ty = Typing.Type.to_type_ty inner_ty in

                   let ty_sc = Typing.Scheme.of_ty ty in
                   let visibility = Env.Private in
                   let tenv =
                     Env.create name ~parent:(Some env) ~visibility ~ty_sc
                       ~kind:Env.Ty ~lookup_space:Env.LkLocal
                   in
                   Env.insert env tenv |> Phase1.assume_new;
                   Ok ()
               | _ -> failwith "[ICE]")
      in

      (* params, ret *)
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
                let spec_ty_sc = Typing.Scheme.of_ty spec_ty in
                let visibility = Env.Public in
                let venv =
                  Env.create name ~parent:(Some env) ~visibility
                    ~ty_sc:spec_ty_sc ~kind:Env.Val ~lookup_space:Env.LkLocal
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

      let env_ty = Typing.Scheme.raw_ty env_ty_sc in
      let%bind subst = Typer.unify ~span ctx.subst ~from:env_ty ~to_:func_ty in
      ctx.subst <- subst;

      Ok ()
  (* *)
  | Ast.{ kind = DeclExternStaticVar { attr; name; ty_spec }; span } ->
      let binding_mut = Mut.mutability_of attr in
      let%bind ty =
        let%bind ty = lookup_type ~env ctx.builtin ty_spec in
        Ok Typing.Type.{ ty with binding_mut }
      in

      let env_ty_sc = Env.type_sc_of env in
      let env_ty = Typing.Scheme.assume_has_no_generics env_ty_sc in
      let%bind subst = Typer.unify ~span ctx.subst ~from:ty ~to_:env_ty in
      ctx.subst <- subst;

      Ok ()
  (* *)
  | Ast.{ kind = DefTypeAlias { alias_ty; _ }; span } ->
      let%bind alias =
        lookup_type ~env ctx.builtin alias_ty
        |> Result.map ~f:Typing.Type.to_type_ty
      in

      let env_ty_sc = Env.type_sc_of env in
      let env_ty = Typing.Scheme.raw_ty env_ty_sc in
      let%bind subst = Typer.unify ~span ctx.subst ~from:alias ~to_:env_ty in
      ctx.subst <- subst;

      Ok ()
  (* *)
  | Ast.{ kind = DefStruct _; span } ->
      let struct_ty =
        let name = to_nested_chain env [] in
        let binding_mut = Typing.Type.MutMut in
        let inner = Typing.Type.{ ty = Struct { name }; binding_mut; span } in
        ctx.builtin.Builtin.type_ inner
      in

      let env_ty_sc = Env.type_sc_of env in
      let env_ty = Typing.Scheme.raw_ty env_ty_sc in
      let%bind subst =
        Typer.unify ~span ctx.subst ~from:struct_ty ~to_:env_ty
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
      let rec f mods env loadable_envs =
        match mods with
        | [] ->
            register_deps ~ctx env;
            Ok [ env ]
        | [ last_id ] ->
            [%loga.debug "leaf: %s" (Ast.show last_id)];
            register_deps ~ctx env;
            let%bind envs = find_mods_with_wildcard ~env last_id in
            Ok (List.join [ envs; loadable_envs ])
        | cont :: rest ->
            [%loga.debug "node: %s" (Ast.show cont)];
            let%bind env = find_mod ~env cont in
            f rest env loadable_envs
      in
      let%bind mod_env = find_mod ?lookup:(Some true) ~env:parent_env pkg in
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
              let ty_sc = Env.type_sc_of env in
              (* TODO: assert *)
              let aenv =
                Env.create name ~parent:(Some penv) ~visibility ~ty_sc
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

and register_deps ~ctx env =
  let Mod.{ menv; _ } = ctx.m in
  Env.register_deps_mod menv env

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
      let ty_sc = Env.type_sc_of env in
      (* TODO: generate a type which have fresh vars *)
      let ty =
        match Typing.Scheme.assume_has_no_generics ty_sc with
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

and rename_ty_sc ~span ~subst ty_sc =
  let open Result.Let_syntax in
  let (Typing.Scheme.ForAll (vars, ty)) = ty_sc in
  match vars with
  | [] -> Ok (ty_sc, subst, [])
  | _ ->
      let new_vars =
        List.map vars ~f:(fun var ->
            match Typing.Subst.is_bound subst var with
            | true -> (var, false)
            | false ->
                let Typing.Type.{ span; _ } = var in
                (Typing.Subst.fresh_ty ~span subst, true))
      in

      let vars_rels = List.zip_exn new_vars vars in

      [%loga.debug "Rename: From %s" (Typing.Type.to_string ty)];

      (* unify vars -> new_vars *)
      let%bind subst =
        List.fold_result vars_rels ~init:subst ~f:(fun subst ((nv, _), v) ->
            Typer.unify_var ~span subst ~from:v ~to_:nv)
      in
      let ty = Typing.Subst.subst_type subst ty in

      [%loga.debug "Rename: To %s" (Typing.Type.to_string ty)];

      (* mapping from new_vars -> vars *)
      let inv_subst =
        List.map vars_rels ~f:(fun ((nv, is_fresh), v) ->
            match is_fresh with true -> (v, Some nv) | false -> (v, None))
      in

      let new_vars = new_vars |> List.map ~f:fst in
      let ty_sc = Typing.Scheme.ForAll (new_vars, ty) in
      Ok (ty_sc, subst, inv_subst)

and lookup_path ~env ~subst ast :
    ( Env.t
      * (Typing.Type.t * Typing.Type.t option) list
      * Typing.Type.t
      * Typing.Subst.t,
      Diagnostics.Elem.t )
    Result.t =
  let open Result.Let_syntax in
  match ast with
  (* *)
  | Ast.{ kind = Path { root; elems }; span; _ } ->
      let rec find_elems env_aux elems =
        match elems with
        | [] -> Ok env_aux
        | x :: xs ->
            let (env, vars, _, subst) = env_aux in
            let%bind (nenv, nvars, nty, nsubst) = lookup_path ~env ~subst x in
            let nenv_aux = (nenv, nvars @ vars, nty, nsubst) in
            find_elems nenv_aux xs
      in
      let%bind env_aux = lookup_path ~env ~subst root in
      find_elems env_aux elems
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
      let%bind env = match envs with [ e ] -> Ok e | _ -> failwith "TODO" in

      let ty_sc = Env.type_sc_of env in
      let%bind (Typing.Scheme.ForAll (new_vars, new_ty), subst, inv_rel) =
        rename_ty_sc ~span ~subst ty_sc
      in

      Ok (env, inv_rel, new_ty, subst)
  (* *)
  | Ast.{ span; _ } -> failwith "unexpected token (lookup_path)"

and make_inv_subst vars =
  let subst = Typing.Subst.create () in
  List.fold_left vars ~init:subst ~f:(fun subst (v, nv) ->
      match nv with
      | Some nv ->
          let span = Common.Span.undef in
          Typer.unify_var ~span subst ~from:v ~to_:nv
          |> Result.map_error ~f:(fun _ -> "[ICE] must not failed")
          |> Result.ok_or_failwith
      | None -> subst)

and to_leyer env inv_subst =
  let module Chain = Common.Chain in
  let kind =
    match env.Env.kind with
    | Env.M -> Some Chain.Layer.Module
    | Env.Ty -> Some Chain.Layer.Type
    | Env.Val -> Some Chain.Layer.Var
    | _ -> None
  in

  (* TODO: fix performance *)
  let (Typing.Scheme.ForAll (params, _)) = Env.type_sc_of env in
  let generics_vars =
    List.map params ~f:(fun param -> Typing.Subst.subst_type inv_subst param)
  in

  Option.map kind ~f:(fun k ->
      let l = Chain.Layer.{ name = env.Env.name; kind = k; generics_vars } in
      l)

and to_nested_chain env vars =
  let module Chain = Common.Chain in
  let inv_subst = make_inv_subst vars in
  let merge_chain cs env =
    let l_opt = to_leyer env inv_subst in
    match l_opt with Some l -> Chain.Nest.join_rev cs l | None -> cs
  in

  let rec f env cs =
    match env.Env.kind with
    | Env.Alias aenv -> f aenv cs
    | _ -> (
        let cs = merge_chain cs env in
        match env.Env.parent with None -> cs | Some penv -> f penv cs )
  in
  f env (Chain.Nest.create ())

and to_chains' env vars =
  let module Chain = Common.Chain in
  match env.Env.lookup_space with
  | Env.LkLocal ->
      let inv_subst = make_inv_subst vars in
      let l_opt = to_leyer env inv_subst in
      let l = Option.value_exn ~message:"[ICE]" l_opt in
      Chain.Local l
  | Env.LkGlobal -> Chain.Global (to_nested_chain env vars)

and to_chains env = to_chains' env []
