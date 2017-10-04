(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries
open Type_sets
open Sema_context
open Sema_env
open Sema_error

let rec solve_forward_refs ?(meta_variables=[])
                           ?(opt_attr=None)
                           ?(execution_level=Meta_level.Runtime)
                           node parent_env ctx =
  match node with
  | Ast.StatementList (nodes) ->
     begin
       try
         let tagged_nodes =
           nodes
           |> List.map (fun n -> solve_forward_refs n parent_env ctx)
         in
         TAst.StatementList tagged_nodes
       with
       | Normal_error err ->
          (* TODO: fix *)
          failwith "[ICE] normal error"
       | Fatal_error err as exn ->
          Sema_error.store_error_message err ctx;
          raise exn
     end

  | Ast.ExprStmt ast ->
     TAst.ExprStmt (TAst.PrevPassNode ast)

  | Ast.VoidExprStmt ast ->
     TAst.VoidExprStmt (TAst.PrevPassNode ast)

  | Ast.ReturnStmt (ast) ->
     TAst.ReturnStmt ((Option.map (fun a -> TAst.PrevPassNode a) ast))

  | Ast.ImportStmt (pkg_names, mod_name, is_public, loc) ->
     begin
       let mod_env = prepare_module ~loc:loc pkg_names mod_name ctx in
       let privacy = if is_public then Env.ModPublic else Env.ModPrivate in
       Env.import_module ~privacy:privacy parent_env mod_env;

       (* remove import statements *)
       TAst.EmptyStmt
     end

  | Ast.FunctionDefStmt (id_name, lt_specs, params, ml, opt_ret_type, instance_cond, body, None, loc) ->
     begin
       match (ml, execution_level) with
       | (Meta_level.OnlyMeta, Meta_level.Runtime) ->
          let base_kind = Env.Kind.Function Meta_level.OnlyMeta in
          let (base_env, is_env_created) =
            Env.MultiSetOp.find_or_add parent_env id_name base_kind
          in

          let param_inits =
            match params with
            | Ast.ParamsList param_inits -> param_inits
            | _ -> failwith "[ICE]"
          in
          let template_param_inits =
            param_inits
            |> List.mapi (fun n (attr, opt_id, value_init) ->
                          let id =
                            Option.default_delayed
                              (fun () -> Id_string.Pure (Printf.sprintf "_%d" n))
                              opt_id
                          in
                          (id, Some value_init)
                        )
          in

          let inner_node =
            Ast.FunctionDefStmt (id_name,
                                 lt_specs,
                                 Ast.ParamsList [],
                                 ml,
                                 opt_ret_type,
                                 instance_cond,
                                 body,
                                 None,
                                 loc)
          in
          let template_params = Ast.TemplateParamsList template_param_inits in
          let template_env_r = {
            Env.tl_name = id_name;
            Env.tl_params = TAst.NotInstantiatedNode (template_params, None);
            Env.tl_inner_node = TAst.NotInstantiatedNode (inner_node, opt_attr);
          } in
          let base_env_r = Env.MultiSetOp.get_record base_env in
          base_env_r.Env.ms_templates <- template_env_r :: base_env_r.Env.ms_templates;

          (* regards as no statement *)
          TAst.EmptyStmt
       | _ ->
          let fenv = declare_pre_function id_name ml meta_variables loc parent_env ctx in

          let node = TAst.FunctionDefStmt (
                         id_name,
                         lt_specs,
                         TAst.PrevPassNode params,
                         ml,
                         opt_ret_type |> Option.map (fun x -> TAst.PrevPassNode x),
                         instance_cond |> Option.map (fun x -> TAst.PrevPassNode x),
                         TAst.PrevPassNode body,
                         opt_attr,
                         (loc, Some fenv)
                       ) in
          Env.update_rel_ast fenv node;
          node
     end

  | Ast.MemberFunctionDefStmt (id_name, lt_specs, params, quals, opt_ret_type, body, None, loc) ->
     begin
       let ml = Meta_level.Runtime in
       let fenv = declare_pre_function id_name ml meta_variables loc parent_env ctx in
       Env.ClassOp.push_member_function parent_env fenv;

       let node = TAst.MemberFunctionDefStmt (
                      id_name,
                      lt_specs,
                      TAst.PrevPassNode params,
                      quals,
                      Option.map (fun x -> TAst.PrevPassNode x) opt_ret_type,
                      TAst.PrevPassNode body,
                      opt_attr,
                      (loc, Some fenv)
                    ) in
       Env.update_rel_ast fenv node;
       node
     end

  | Ast.ExternFunctionDefStmt (id_name, lt_specs, params, ml, ret_type, instance_cond, extern_fname, None, loc) ->
     begin
       match (ml, execution_level) with
       | (Meta_level.OnlyMeta, Meta_level.Runtime) ->
          let base_kind = Env.Kind.Function Meta_level.OnlyMeta in
          let (base_env, is_env_created) =
            Env.MultiSetOp.find_or_add parent_env id_name base_kind
          in

          let param_inits =
            match params with
            | Ast.ParamsList param_inits -> param_inits
            | _ -> failwith "[ICE]"
          in
          let template_param_inits =
            param_inits
            |> List.mapi (fun n (attr, opt_id, value_init) ->
                          let id =
                            Option.default_delayed
                              (fun () -> Id_string.Pure (Printf.sprintf "_%d" n))
                              opt_id
                          in
                          (id, Some value_init)
                        )
          in

          let inner_node =
            Ast.ExternFunctionDefStmt (id_name,
                                       lt_specs,
                                       Ast.ParamsList [],
                                       ml,
                                       ret_type,
                                       instance_cond,
                                       extern_fname,
                                       None,
                                       loc
                                      )
          in
          let template_params = Ast.TemplateParamsList template_param_inits in
          let template_env_r = {
            Env.tl_name = id_name;
            Env.tl_params = TAst.NotInstantiatedNode (template_params, None);
            Env.tl_inner_node = TAst.NotInstantiatedNode (inner_node, opt_attr);
          } in
          let base_env_r = Env.MultiSetOp.get_record base_env in
          base_env_r.Env.ms_templates <- template_env_r :: base_env_r.Env.ms_templates;

          (* regards as no statement *)
          TAst.EmptyStmt
       | _ ->
          let fenv = declare_pre_function id_name ml meta_variables loc parent_env ctx in
          let node = TAst.ExternFunctionDefStmt (
                         id_name,
                         lt_specs,
                         TAst.PrevPassNode params,
                         ml,
                         TAst.PrevPassNode ret_type,
                         instance_cond |> Option.map (fun x -> TAst.PrevPassNode x),
                         extern_fname,
                         opt_attr,
                         (loc, Some fenv)
                       ) in
          Env.update_rel_ast fenv node;
          node
     end

  | Ast.ClassDefStmt (id_name, lt_spec, body, None, loc) ->
     begin
       let cenv = declare_pre_class id_name meta_variables loc parent_env ctx in
       let cenv_r = Env.ClassOp.get_record cenv in

       (* class members are forward referencable *)
       let nbody = solve_forward_refs ~opt_attr:opt_attr body cenv ctx in

       (* members are appended to head,
        * thus reverse them in order to make them declared order *)
       cenv_r.Env.cls_member_vars <- List.rev cenv_r.Env.cls_member_vars;
       cenv_r.Env.cls_member_funcs <- List.rev cenv_r.Env.cls_member_funcs;

       let node = TAst.ClassDefStmt (
                      id_name,
                      lt_spec,
                      nbody,
                      opt_attr,
                      (loc, Some cenv)
                    ) in
       Env.update_rel_ast cenv node;
       node
     end

  | Ast.ExternClassDefStmt (id_name, lifetime_spec, extern_cname, opt_body, None, loc) ->
     begin
       let cenv = declare_pre_class id_name meta_variables loc parent_env ctx in

       let node = TAst.ExternClassDefStmt (
                      id_name,
                      lifetime_spec,
                      extern_cname,
                      opt_body |> Option.map (fun a -> TAst.PrevPassNode a),
                      opt_attr,
                      (loc, Some cenv)
                    ) in
       Env.update_rel_ast cenv node;
       node
     end

  | Ast.MemberVariableDefStmt (v, loc) ->
     begin
       let (_, var_id_name, _) =
         match v with
         | Ast.VarInit vi -> vi
         | _ -> failwith "unexpected node"
       in

       let var_r = Env.VariableOp.empty_record var_id_name in
       let venv =
         Env.create_context_env parent_env var_id_name
                                (Env.Variable (var_r))
                                loc
       in
       Env.add_inner_env parent_env var_id_name venv |> error_if_env_is_dupped loc;
       Env.ClassOp.push_member_variable parent_env venv;

       let node = TAst.MemberVariableDefStmt (TAst.PrevPassNode v, (loc, Some venv)) in
       Env.update_rel_ast venv node;    (* this node will be updated later... *)
       node
     end

  | Ast.VariableDefStmt (ml, v, loc) ->
     TAst.VariableDefStmt (ml, TAst.PrevPassNode v, (loc, None))

  | Ast.TemplateStmt (id_name, template_params, inner_node) ->
     begin
       let loc = None in
       let base_kind = match inner_node with
         | Ast.FunctionDefStmt _
         | Ast.ExternFunctionDefStmt _
         | Ast.MemberFunctionDefStmt _ ->
            (* TODO: fix metalevel *)
            Env.Kind.Function Meta_level.Meta
         | Ast.ClassDefStmt _
         | Ast.ExternClassDefStmt _ ->
            Env.Kind.Class
         | _ ->
            failwith "[ICE] template of this statement is not supported."
       in
       let (base_env, is_env_created) =
         Env.MultiSetOp.find_or_add parent_env id_name base_kind
       in
       let template_env_r = {
         Env.tl_name = id_name;
         Env.tl_params = TAst.NotInstantiatedNode (template_params, None);
         Env.tl_inner_node = TAst.NotInstantiatedNode (inner_node, opt_attr);
       } in
       let base_env_r = Env.MultiSetOp.get_record base_env in
       base_env_r.Env.ms_templates <- template_env_r :: base_env_r.Env.ms_templates;

       let () =
         let register_member_info () =
           match Env.kind_of_env parent_env with
           | Env.Kind.Class ->
              begin
                match base_kind with
                (* will be member function *)
                | Env.Kind.Function _ ->
                   let fenv =
                     Env.create_context_env base_env id_name
                                            (Env.Template template_env_r)
                                            loc
                   in
                   Env.ClassOp.push_member_function parent_env fenv
                | _ ->
                   ()
              end
           | _ -> ()
         in
         if is_env_created then
           register_member_info ()
       in

       (* regards as no statement *)
       TAst.EmptyStmt
     end

  | Ast.EmptyStmt -> TAst.EmptyStmt

  | Ast.AttrWrapperStmt (attr_tbl, ast) ->
     begin
       let t_ast =
         solve_forward_refs ~meta_variables:meta_variables
                            ~opt_attr:(Some attr_tbl)
                            ast parent_env ctx
       in
       (* reduce AttrWrapperStmt *)
       t_ast
     end

  | _ ->
     begin
       Ast.print node;
       failwith "solve_forward_refs: unsupported node"
     end

and prepare_module_from_filepath ?(def_mod_info=None) filepath ctx =
  let root_env = ctx.sc_root_env in
  let simple_module_name = filepath |> Filename.basename |> Filename.chop_extension in
  let mod_ast =
    Syntax.make_ast_from_file ~default_pkg_names:[]
                              ~default_mod_name:simple_module_name
                              filepath
  in

  let load_module_ast body pkg_names mod_name base_dir attr loc =
    let check_mod_name expect_mod_head =
      let (expect_pkg_names, expect_mod_name) = expect_mod_head in
      if not (pkg_names = expect_pkg_names && mod_name = expect_mod_name) then
        let err =
          Error_msg.ModuleNameDifferent (expect_mod_head, (pkg_names, mod_name))
        in
        fatal_error err
    in
    Option.may check_mod_name def_mod_info;

    (* TODO: fix g_name *)
    let g_id_name = Id_string.Pure (String.concat "." (pkg_names @ [mod_name])) in
    let env =
      Env.create_context_env root_env g_id_name
                             (Env.Module (Env.empty_lookup_table (),
                                          {
                                            Env.mod_name = mod_name;
                                            Env.mod_pkg_names = pkg_names;
                                          })
                             )
                             loc
    in
    Env.add_inner_env root_env g_id_name env |> error_if_env_is_dupped loc;

    (* register module*)
    let mod_id = Module_info.Bag.register ctx.sc_module_bag pkg_names mod_name env in
    ignore mod_id;

    let no_builtin = Attribute.find_bool_val (Some attr) "no_builtin" ctx in
    if not no_builtin then
      begin
        (* import incomplete builtin module *)
        let builtin_mod_e = prepare_builtin_module ctx in
        Env.import_module ~privacy:Env.ModPrivate env builtin_mod_e
      end;

    (* solve forward references *)
    let res_node = solve_forward_refs body env ctx in

    (* make and save tmp TAst to env *)
    let node = TAst.Module (res_node, pkg_names, mod_name, base_dir, (loc, Some env)) in
    Env.update_rel_ast env node;

    env
  in

  let rec load_ast ast attr =
    match ast with
    | Ast.Module (inner, pkg_names, mod_name, base_dir, loc) ->
       load_module_ast inner pkg_names mod_name base_dir attr loc
    | Ast.AttrWrapperStmt (attr, ast) ->
       load_ast ast attr
    | _ -> failwith "[ICE] ast is not module"
  in
  load_ast mod_ast (Hashtbl.create 0)


and prepare_module ~loc pkg_names mod_name ctx =
  let mod_env =
    Module_info.Bag.search_module ctx.sc_module_bag pkg_names mod_name
  in
  match mod_env with
  | Some r ->
     (* return cached module *)
     r
  | None ->
     let dig_pkgs (dirs, hist) dir_name =
       let dir_exists dir =
         let dir_name = Filename.concat dir dir_name in
         Sys.file_exists dir_name
       in
       let next_dir =
         try
           let base_dir_name = List.find dir_exists dirs in
           [Filename.concat base_dir_name dir_name]
         with
         | Not_found -> []
       in
       (next_dir, (dir_name, dirs) :: hist)
     in
     let search_dirs = ctx.sc_module_search_dirs in
     let target_dirs = List.fold_left dig_pkgs (search_dirs, []) pkg_names in
     let target_dir = match target_dirs with
       | ([dir], _) ->
          dir
       | ([], hist) ->
          let full_module_name = (String.concat "." pkg_names) ^ "." ^ mod_name in
          fatal_error (Error_msg.ModuleNotFound (full_module_name, hist, loc))
       | _ -> failwith "[ICE]"
     in
     Debug.printf "import from = %s\n" target_dir;
     let filepath = Filename.concat target_dir mod_name ^ ".rill" in

     prepare_module_from_filepath ~def_mod_info:(Some (pkg_names, mod_name))
                                  filepath ctx

and declare_meta_var env ctx (id_name, uni_id) =
  let loc = None in
  let meta_er = Env.MetaVariable uni_id in
  let e = Env.create_context_env env id_name meta_er loc in
  Env.add_inner_env env id_name e |> error_if_env_is_dupped loc;

  Unification.get_as_value ctx.sc_unification_ctx uni_id


and declare_pre_function id_name ml meta_variables loc parent_env ctx =
  let in_template = List.length meta_variables > 0 in

  (* accept multiple definition for overload *)
  let (base_env, _) =
    Env.MultiSetOp.find_or_add parent_env id_name (Env.Kind.Function ml)
  in

  let fenv_r = Env.FunctionOp.empty_record id_name in
  let fenv =
    Env.create_context_env ~is_instantiated:in_template
                           parent_env id_name
                           (Env.Function (
                                Env.empty_lookup_table (),
                                fenv_r))
                           loc
  in

  (* declare meta variables if exist *)
  let template_vals = List.map (declare_meta_var fenv ctx) meta_variables in
  fenv_r.Env.fn_template_vals <- template_vals;

  let _ = if in_template then
            Env.MultiSetOp.add_template_instances base_env fenv
          else
            Env.MultiSetOp.add_normal_instances base_env fenv
  in
  fenv


and declare_pre_class id_name meta_variables loc parent_env ctx =
  let in_template = List.length meta_variables > 0 in

  (* accept multiple definition for specialization *)
  let (base_env, _) = Env.MultiSetOp.find_or_add parent_env id_name Env.Kind.Class in

  let cenv_r = Env.ClassOp.empty_record id_name in
  let cenv =
    Env.create_context_env ~is_instantiated:in_template
                           parent_env id_name
                           (Env.Class (
                                Env.empty_lookup_table (),
                                cenv_r))
                           loc
  in

  (* declare meta variables if exist *)
  let template_vals = List.map (declare_meta_var cenv ctx) meta_variables in
  cenv_r.Env.cls_template_vals <- template_vals;

  let _ = if in_template then
            Env.MultiSetOp.add_template_instances base_env cenv
          else
            Env.MultiSetOp.add_normal_instances base_env cenv
  in
  cenv

and prepare_builtin_module ctx =
  try
    prepare_module ~loc:Loc.dummy ["core"] "builtin" ctx
  with
  | Fatal_error err as exn ->
     Sema_error.store_error_message err ctx;
     raise exn
