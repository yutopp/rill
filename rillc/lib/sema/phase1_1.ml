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
  subst : Typing.Subst.t;
  builtin : Builtin.t;
}

let context ~m ~subst ~builtin =
  let Mod.{ ds; _ } = m in
  { m; parent = None; ds; subst; builtin }

let rec declare_toplevels ~ctx ast : (ctx_t, Diagnostics.Elem.t) Result.t =
  let open Result.Let_syntax in
  match ast with
  (* *)
  | TopAst.{ kind = Module { stmts }; span } ->
      let Mod.{ menv; _ } = ctx.m in
      let ctx' = { ctx with parent = Some menv } in
      let%bind ctx' = declare_toplevels ~ctx:ctx' stmts in
      Ok ctx'
  (* *)
  | TopAst.{ kind = Stmts { nodes }; span } ->
      let%bind ctx =
        List.fold_result nodes ~init:ctx ~f:(fun ctx node ->
            match declare_toplevels ~ctx node with
            | Ok ctx' -> Ok ctx'
            | Error d ->
                Diagnostics.append ctx.ds d;
                (* skip inserting *)
                Ok ctx)
      in
      Ok ctx
  (* *)
  | TopAst.{ kind = WithEnv { node; env }; span } -> with_env ~ctx ~env node
  | TopAst.{ kind = WithEnvAndBody { node; body; env }; _ }
  | TopAst.{ kind = WithEnvAndBody2 { node; body; env; _ }; _ } ->
      with_env_and_body ~ctx ~env node body
  | TopAst.{ kind = PassThrough { node }; span } -> pass_through ~ctx node
  | TopAst.{ kind = LazyDecl _; _ } -> Ok ctx

and with_env ~ctx ~env ast : (ctx_t, Diagnostics.Elem.t) Result.t =
  let open Result.Let_syntax in
  match ast with
  (* *)
  | Ast.{ kind = DeclExternFunc { name; ty_params; params; ret_ty; _ }; span }
  | Ast.{ kind = DeclFunc { name; ty_params; params; ret_ty; _ }; span }
  | Ast.{ kind = DefFunc { name; ty_params; params; ret_ty; _ }; span } ->
      let env_ty_sc = Env.type_sc_of env in
      let (Typing.Scheme.ForAll { vars = ty_vars; ty; _ }) = env_ty_sc in
      Typing.Subst.assume_not_bound ctx.subst ty;

      let parent_args =
        match ctx.parent with
        | Some env ->
            let (Typing.Scheme.ForAll { implicits; vars; ty }) =
              Env.type_sc_of env
            in
            rename_type_vars ~subst:ctx.subst ~implicits ~vars
        | _ -> []
      in

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
                   let pty = Typing.Pred.of_type ty in
                   let ty_sc = Typing.Scheme.of_ty pty in
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
      let%bind (params_specs_rev, has_self, _) =
        List.fold_result params ~init:([], false, 0)
          ~f:(fun (ps, has_self, i) param ->
            match param with
            (* normal *)
            | Ast.{ kind = ParamDecl { attr; name; ty_spec }; span } ->
                let%bind () = Guards.guard_dup_value ~span env name in

                let%bind spec =
                  decl_param_var ~env ~subst:ctx.subst ~builtin:ctx.builtin
                    ~attr ~name ~ty_spec
                in
                Ok (spec :: ps, has_self, i + 1)
            (* self *)
            | Ast.{ kind = ParamSelfDecl { attr; ty_spec_opt }; span }
              when i = 0 ->
                let name = "self" in
                let ty_spec =
                  match ty_spec_opt with
                  | None -> Ast.self_of ~span
                  | Some _ -> failwith "[ICE] not implemented"
                in
                let%bind spec =
                  decl_param_var ~env ~subst:ctx.subst ~builtin:ctx.builtin
                    ~attr ~name ~ty_spec
                in
                let has_self = has_self || true in
                Ok (spec :: ps, has_self, i + 1)
            (* other *)
            | _ -> failwith "[ICE]")
      in
      let (params_tys, predicates, implicits, vars) =
        params_specs_rev
        |> List.fold_left ~init:([], [], [], [])
             ~f:(fun (params_tys, predicates, implicits, vars) spec ->
               let (p, ps, is, vs) = spec in
               (p :: params_tys, ps @ predicates, is @ implicits, vs @ vars))
      in

      let%bind ret_spec =
        let%bind (ty, predicates, implicits, vars, _) =
          lookup_type' ~env ~subst:ctx.subst ctx.builtin ret_ty
        in
        let binding_mut = Typing.Type.MutImm in
        Ok (Typing.Type.{ ty with binding_mut }, predicates, implicits, vars)
      in
      let (ret_ty, predicates, implicits, vars) =
        let (p, ps, is, vs) = ret_spec in
        (p, ps @ predicates, is @ implicits, vs @ vars)
      in

      (* TODO: check vars *)
      (* *)
      Env.append_implicits env implicits;
      Env.append_predicates env predicates;
      Env.set_has_self env has_self;

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

      [%loga.debug
        "func(bef) %s (has_self=%b) :: %s" name has_self
          ( Env.type_sc_of env
          |> Typing.Subst.subst_scheme ctx.subst
          |> Typing.Scheme.to_string )];

      let env_ty = Typing.Scheme.raw_ty env_ty_sc in
      let%bind subst = Typer.unify ~span ctx.subst ~from:env_ty ~to_:func_ty in
      let ctx = { ctx with subst } in

      [%loga.debug
        "func %s (has_self=%b) :: %s" name has_self
          ( Env.type_sc_of env
          |> Typing.Subst.subst_scheme subst
          |> Typing.Scheme.to_string )];

      Ok ctx
  (* *)
  | Ast.{ kind = DeclExternStaticVar { attr; name; ty_spec }; span } ->
      let binding_mut = Mut.mutability_of attr in
      let%bind ty =
        let%bind (ty, _, _, _, _) =
          lookup_type' ~env ~subst:ctx.subst ctx.builtin ty_spec
        in
        Ok Typing.Type.{ ty with binding_mut }
      in

      let env_ty_sc = Env.type_sc_of env in
      let env_ty = Typing.Scheme.raw_ty env_ty_sc in
      let%bind subst = Typer.unify ~span ctx.subst ~from:ty ~to_:env_ty in
      let ctx = { ctx with subst } in

      Ok ctx
  (* *)
  | Ast.{ kind = DefTypeAlias { alias_ty; _ }; span } ->
      let%bind (alias, _, _, _, _) =
        lookup_type' ~env ~subst:ctx.subst ctx.builtin alias_ty
      in
      let alias = Typing.Type.to_type_ty alias in

      let env_ty_sc = Env.type_sc_of env in
      let env_ty = Typing.Scheme.raw_ty env_ty_sc in
      let%bind subst = Typer.unify ~span ctx.subst ~from:alias ~to_:env_ty in
      let ctx = { ctx with subst } in

      Ok ctx
  (* *)
  | Ast.{ kind = DefStruct _; span } ->
      let struct_ty =
        let name = Name.to_nested_chain env in
        let binding_mut = Typing.Type.MutMut in
        let inner = Typing.Type.{ ty = Struct { name }; binding_mut; span } in
        ctx.builtin.Builtin.type_ ~span inner
      in

      let env_ty_sc = Env.type_sc_of env in
      let env_ty = Typing.Scheme.raw_ty env_ty_sc in
      let%bind subst =
        Typer.unify ~span ctx.subst ~from:struct_ty ~to_:env_ty
      in
      let ctx = { ctx with subst } in

      Ok ctx
  (* *)
  | Ast.{ span; _ } ->
      let e =
        new Diagnostics.Reasons.internal_error
          ~message:"Not supported decl node (phase1_1, with_env)"
      in
      let elm = Diagnostics.Elem.error ~span e in
      Error elm

and with_env_and_body ~ctx ~env node body =
  let open Result.Let_syntax in
  match node with
  | Ast.{ kind = DefTrait _; span } ->
      let env_ty_sc = Env.type_sc_of env in
      let env_ty = Typing.Scheme.raw_ty env_ty_sc in

      let trait_ty =
        let name = Name.to_nested_chain env in
        let binding_mut = Typing.Type.MutMut in
        let inner =
          let initial_marker =
            Typing.Type.(env_ty |> of_type_ty |> assume_var_id)
          in
          Typing.Type.{ ty = Trait { name; initial_marker }; binding_mut; span }
        in
        ctx.builtin.Builtin.type_ ~span inner
      in

      (* Nodes *)
      let%bind ctx' =
        let ctx = { ctx with parent = Some env } in
        declare_toplevels ~ctx body
      in
      let subst = ctx'.subst in

      [%loga.debug
        "Trait from(%s) -> to(%s)"
          (Typing.Type.to_string trait_ty)
          (Typing.Type.to_string env_ty)];
      let%bind subst = Typer.unify ~span subst ~from:trait_ty ~to_:env_ty in
      let ctx = { ctx with subst } in

      Ok ctx
  (* *)
  | Ast.{ span; _ } ->
      let e =
        new Diagnostics.Reasons.internal_error
          ~message:"Not supported decl node (phase1_1, with_env_and_body)"
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

      Ok ctx
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

and lookup_type' ~env ~subst builtin ast =
  let open Result.Let_syntax in
  match ast with
  (* *)
  | Ast.{ kind = IDSelf; span } ->
      let%bind env =
        Env.lookup_self_type env
        |> Result.map_error ~f:(fun _trace ->
               let candidates = [] (* TODO *) in
               let e =
                 new Diagnostics.Reasons.id_not_found ~name:"self" ~candidates
               in
               let elm = Diagnostics.Elem.error ~span e in
               elm)
      in
      let (Typing.Scheme.ForAll { implicits; vars; ty }) = Env.type_sc_of env in
      let (ty, implicits, predicates) =
        match ty with
        | Typing.Pred.Pred { ty = Typing.Type.{ ty = Type tty; span; _ }; _ } ->
            let tup =
              match (implicits, vars) with
              (* trait *)
              | ([ a ], []) ->
                  let trait_ty = tty in
                  let predicate =
                    Typing.Pred.{ cond_trait = trait_ty; cond_var = a }
                  in
                  (a, implicits, [ predicate ])
              (* trait *)
              | ([ a ], vars) -> failwith "[ICE] not implemented"
              (* impl *)
              | ([], vars) -> (tty, [], [])
              | _ ->
                  failwith
                    (Printf.sprintf "[ICE] invalid implicits: %s"
                       (Typing.Pred.to_string ty))
            in
            tup
        | _ ->
            failwith
              (Printf.sprintf "[ICE] self is not type: %s"
                 (Typing.Pred.to_string ty))
      in

      Ok (Typing.Type.{ ty with span }, predicates, implicits, vars, env)
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
      let (Typing.Scheme.ForAll { implicits; vars; ty }) = Env.type_sc_of env in
      let ty =
        match ty with
        | Typing.Pred.Pred { ty = Typing.Type.{ ty = Type sty; span; _ }; _ } ->
            sty
        | _ ->
            failwith
              (Printf.sprintf "[ICE] not type: %s" (Typing.Pred.to_string ty))
      in
      Ok (Typing.Type.{ ty with span }, [], implicits, vars, env)
  (* *)
  | Ast.{ kind = TypeExprArray { elem; len }; span } ->
      let%bind (elem_ty, predicates, implicits, vars, env) =
        lookup_type' ~env ~subst builtin elem
      in
      (* TODO: fix 4 *)
      let ty = builtin.Builtin.array_ ~span elem_ty 4 in
      Ok (ty, [], implicits, vars, env)
  (* *)
  | Ast.{ kind = TypeExprPointer { attr; elem }; span } ->
      let%bind (elem_ty, predicates, implicits, vars, env) =
        lookup_type' ~env ~subst builtin elem
      in
      let mut = Mut.mutability_of attr in
      let ty = builtin.Builtin.pointer_ ~span mut elem_ty in
      Ok (ty, [], implicits, vars, env)
  (* *)
  | Ast.{ span; _ } -> failwith "unexpected token (lookup_type)"

and lookup_type_fresh ~env ~subst ~builtin ast =
  let open Result.Let_syntax in
  let%bind (ty, predicates, implicits, vars, _) =
    lookup_type' ~env ~subst builtin ast
  in
  let args = rename_type_vars ~subst ~implicits ~vars in
  match args with
  | [] -> Ok ty
  | _ -> Ok Typing.Type.{ ty with ty = Args { recv = ty; args } }

and decl_param_var ~env ~subst ~builtin ~attr ~name ~ty_spec =
  let open Result.Let_syntax in
  let%bind (spec_ty, predicates, implicits, vars) =
    let%bind (ty, predicates, implicits, vars, _) =
      lookup_type' ~env ~subst builtin ty_spec
    in
    let binding_mut = Mut.mutability_of attr in
    Ok (Typing.Type.{ ty with binding_mut }, predicates, implicits, vars)
  in
  let spec_pty = Typing.Pred.of_type spec_ty in
  let spec_ty_sc = Typing.Scheme.of_ty spec_pty in
  let visibility = Env.Public in
  let venv =
    Env.create name ~parent:(Some env) ~visibility ~ty_sc:spec_ty_sc
      ~kind:Env.Val ~lookup_space:Env.LkLocal
  in
  Env.insert env venv |> Phase1.assume_new;

  [%loga.debug
    "decl_param(%s) %s :: %s" env.Env.name name (Typing.Type.to_string spec_ty)];

  Ok (spec_ty, predicates, implicits, vars)

and prepare_fresh_vars_for_unbound ~subst vars =
  List.filter_map vars ~f:(fun var ->
      let (var_id, bound) = Typing.Subst.is_bound subst var in
      match bound with
      | true -> None
      | false ->
          let Typing.Type.{ span; _ } = var in
          let new_ty = Typing.Subst.fresh_ty ~span subst in
          Some Typing.Type.{ apply_src_ty = var; apply_dst_ty = new_ty })

and rename_type_vars ~subst ~implicits ~vars =
  let new_implicits = prepare_fresh_vars_for_unbound ~subst implicits in
  let new_vars = prepare_fresh_vars_for_unbound ~subst vars in
  new_implicits @ new_vars

and renamed_ty_sc ~subst ty_sc =
  let (Typing.Scheme.ForAll { implicits; vars; ty }) = ty_sc in
  let args = rename_type_vars ~subst ~implicits ~vars in
  match args with
  | [] -> (ty_sc, subst)
  | _ ->
      let subst =
        List.fold_left args ~init:subst ~f:(fun subst apply ->
            let Typing.Type.{ apply_src_ty = src; apply_dst_ty = dst } =
              apply
            in
            Typing.Subst.update_type subst src dst)
      in
      (Typing.Subst.subst_scheme subst ty_sc, subst)

and dup_vars vars =
  List.map vars ~f:(fun var ->
      Typing.Type.{ apply_src_ty = var; apply_dst_ty = var })

and id_type_vars ~implicits ~vars =
  let id_implicits = dup_vars implicits in
  let id_vars = dup_vars vars in

  id_implicits @ id_vars

and assign_vars params args =
  let zipped = List.zip params args in
  match zipped with
  | List.Or_unequal_lengths.Ok rels ->
      let apply_list =
        rels
        |> List.map ~f:(fun (s, d) ->
               Typing.Type.{ apply_src_ty = s; apply_dst_ty = d })
      in
      Ok apply_list
  | List.Or_unequal_lengths.Unequal_lengths -> failwith "[TODO]"

and assign_type_vars ~implicits ~implicit_args ~vars ~args =
  let open Result.Let_syntax in
  let%bind id_implicits = assign_vars implicits implicit_args in
  let%bind id_vars = assign_vars vars args in

  Ok (id_implicits @ id_vars)

and to_chains' env subst =
  let module Chain = Common.Chain in
  match env.Env.lookup_space with
  | Env.LkLocal ->
      let l_opt = Name.to_leyer env subst in
      let l = Option.value_exn ~message:"[ICE]" l_opt in
      Chain.Local l
  | Env.LkGlobal -> Chain.Global (Name.to_nested_chain' env subst)

and to_chains env =
  let subst = Typing.Subst.create () in
  to_chains' env subst
