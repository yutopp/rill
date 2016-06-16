(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries
open Type_sets
open Sema_definitions
open Sema_context

let rec solve_forward_refs ?(meta_variables=[])
                           ?(opt_attr=None)
                           node parent_env ctx =
  match node with
  | Ast.StatementList (nodes) ->
     let tagged_nodes =
       nodes
       |> List.map (fun n -> solve_forward_refs n parent_env ctx)
     in
     TAst.StatementList tagged_nodes

  | Ast.ExprStmt ast ->
     TAst.ExprStmt (TAst.PrevPassNode ast)

  | Ast.ReturnStmt (ast) ->
     TAst.ReturnStmt ((Option.map (fun a -> TAst.PrevPassNode a) ast))

  | Ast.ImportStmt (pkg_names, mod_name, _) ->
     begin
       let mod_env = load_module pkg_names mod_name ctx in

       Env.import_module parent_env mod_env;

       (* remove import statements *)
       TAst.EmptyStmt
     end

  | Ast.FunctionDefStmt (id_name, params, opt_ret_type, _, body, None, _) ->
     begin
       let fenv = declare_pre_function id_name meta_variables parent_env ctx in

       let node = TAst.FunctionDefStmt (
                      id_name,
                      TAst.PrevPassNode params,
                      Option.map (fun x -> TAst.PrevPassNode x) opt_ret_type,
                      None,
                      TAst.PrevPassNode body,
                      opt_attr,
                      Some fenv
                    ) in
       Env.update_rel_ast fenv node;
       node
     end

  | Ast.MemberFunctionDefStmt (id_name, params, opt_ret_type, body, None, _) ->
     begin
       let fenv = declare_pre_function id_name meta_variables parent_env ctx in
       Env.ClassOp.push_member_function parent_env fenv;

       let node = TAst.MemberFunctionDefStmt (
                      id_name,
                      TAst.PrevPassNode params,
                      Option.map (fun x -> TAst.PrevPassNode x) opt_ret_type,
                      TAst.PrevPassNode body,
                      opt_attr,
                      Some fenv
                    ) in
       Env.update_rel_ast fenv node;
       node
     end

  | Ast.ExternFunctionDefStmt (id_name, params, ml, ret_type, extern_fname, None, _) ->
     begin
       let fenv = declare_pre_function id_name meta_variables parent_env ctx in

       let node = TAst.ExternFunctionDefStmt (
                      id_name,
                      TAst.PrevPassNode params,
                      ml,
                      TAst.PrevPassNode ret_type,
                      extern_fname,
                      opt_attr,
                      Some fenv
                    ) in
       Env.update_rel_ast fenv node;
       node
     end

  | Ast.ClassDefStmt (id_name, body, None, _) ->
     begin
       let cenv = declare_pre_class id_name meta_variables parent_env ctx in
       let cenv_r = Env.ClassOp.get_record cenv in

       (* class members are forward referencable *)
       let nbody = solve_forward_refs ~opt_attr:opt_attr body cenv ctx in

       (* members are appended to head,
        * thus reverse them in order to make them declared order *)
       cenv_r.Env.cls_member_vars <- List.rev cenv_r.Env.cls_member_vars;
       cenv_r.Env.cls_member_funcs <- List.rev cenv_r.Env.cls_member_funcs;

       let node = TAst.ClassDefStmt (
                      id_name,
                      nbody,
                      opt_attr,
                      Some cenv
                    ) in
       Env.update_rel_ast cenv node;
       node
     end

  | Ast.ExternClassDefStmt (id_name, extern_cname, None, _) ->
     begin
       let cenv = declare_pre_class id_name meta_variables parent_env ctx in

       let node = TAst.ExternClassDefStmt (
                      id_name,
                      extern_cname,
                      opt_attr,
                      Some cenv
                    ) in
       Env.update_rel_ast cenv node;
       node
     end

  | Ast.MemberVariableDefStmt (v, _) ->
     begin
       let (_, var_name, _) =
         match v with
         | Ast.VarInit vi -> vi
         | _ -> failwith "unexpected node"
       in

       let var_r = Env.VariableOp.empty_record var_name in
       let venv = Env.create_context_env parent_env (
                                           Env.Variable (var_r)
                                         )
       in
       Env.add_inner_env parent_env var_name venv;
       Env.ClassOp.push_member_variable parent_env venv;

       let node = TAst.MemberVariableDefStmt (TAst.PrevPassNode v, Some venv) in
       Env.update_rel_ast venv node;    (* this node will be updated later... *)
       node
     end

  | Ast.VariableDefStmt (ml, v, _) ->
     TAst.VariableDefStmt (ml, TAst.PrevPassNode v, None)

  | Ast.TemplateStmt (id_name, template_params, inner_node) ->
     begin
       let base_kind = match inner_node with
         | Ast.FunctionDefStmt _
         | Ast.ExternFunctionDefStmt _
         | Ast.MemberFunctionDefStmt _ -> Env.Kind.Function
         | Ast.ClassDefStmt _
         | Ast.ExternClassDefStmt _ -> Env.Kind.Class
         | _ ->
            begin
              failwith "[ICE] template of this statement is not supported."
            end
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

       let _ =
         let register_member_info () =
           match Env.kind_of_env parent_env with
           | Env.Kind.Class ->
              begin
                match base_kind with
                (* will be member function *)
                | Env.Kind.Function ->
                   let tenv =
                     Env.create_context_env base_env
                                            (Env.Template template_env_r)
                   in
                   Env.ClassOp.push_member_function parent_env tenv
                | _ ->
                   ()
              end
           | _ -> ()
         in
         if is_env_created then register_member_info ()
       in

       (* regards as no statement *)
       TAst.EmptyStmt
     end

  | Ast.EmptyStmt -> TAst.EmptyStmt

  | Ast.AttrWrapperStmt (attr_tbl, ast) ->
     begin
       let conv k v = Option.map (fun v -> TAst.PrevPassNode v) v in
       let t_attr_tbl = attr_tbl |> Hashtbl.map conv in

       let t_ast =
         solve_forward_refs ~meta_variables:meta_variables
                            ~opt_attr:(Some t_attr_tbl)
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


and load_module_by_filepath ?(def_mod_info=None) filepath ctx =
  let root_env = ctx.sc_root_env in
  let (raw_pkg_names, raw_mod_name) =
    Option.default ([], filepath |> Filename.basename |> Filename.chop_extension)
                   def_mod_info
  in
  let mod_ast =
    Syntax.make_ast_from_file ~default_pkg_names:raw_pkg_names
                              ~default_mod_name:raw_mod_name
                              filepath
  in

  match mod_ast with
  | Ast.Module (inner, pkg_names, mod_name, base_dir, _) ->
     begin
       let check_mod_name (raw_pkg_names, raw_mod_name) =
         if not (pkg_names = raw_pkg_names && mod_name = raw_mod_name) then
           raise (Fatal_error "package/module names are different")
       in
       Option.may check_mod_name def_mod_info;

       let env = Env.create_context_env root_env (
                                          Env.Module (Env.empty_lookup_table (),
                                                      {
                                                        Env.mod_name = mod_name;
                                                        Env.mod_pkg_names = pkg_names;
                                                      })
                                        ) in
       (* TODO: fix g_name *)
       let g_name = String.concat "." (pkg_names @ [mod_name]) in
       Env.add_inner_env root_env g_name env;

       (* register module*)
       let mod_id =
         Module_info.Bag.register ctx.sc_module_bag
                                  pkg_names mod_name env
       in
       ignore mod_id;

       (* import builtins for all modules, if the builtin modules loaded *)
       Option.may (fun bm -> Env.import_module env bm) ctx.sc_builtin_m_env;

       (* solve forward references *)
       let res_node = solve_forward_refs inner env ctx in
       let node = TAst.Module (res_node, pkg_names, mod_name, base_dir, Some env) in
       Env.update_rel_ast env node;

       env
     end
  | _ -> failwith "[ICE]"


and load_module pkg_names mod_name ctx =
  let mod_env = Module_info.Bag.search_module ctx.sc_module_bag
                                              pkg_names mod_name in
  match mod_env with
    | Some r -> r (* return cached module *)
    | None ->
       begin
         let pp dirs dir_name =
           let dir_exists dir =
             let dir_name = Filename.concat dir dir_name in
             Sys.file_exists dir_name
           in
           try [Filename.concat (List.find dir_exists dirs) dir_name] with
           | Not_found -> []
         in
         let target_dirs = List.fold_left pp ctx.sc_module_search_dirs pkg_names in
         let target_dir = match target_dirs with
           | [dir] -> dir
           | [] -> raise (Fatal_error ("[ERR] package not found : " ^ (String.concat "." pkg_names) ^ "." ^ mod_name))
           | _ -> failwith "[ICE]"
         in
         Debug.printf "import from = %s\n" target_dir;
         let filepath = Filename.concat target_dir mod_name ^ ".rill" in

         load_module_by_filepath ~def_mod_info:(Some (pkg_names, mod_name))
                                 filepath ctx
       end


and declare_meta_var env ctx (name, uni_id) =
  let meta_e = Env.MetaVariable uni_id in
  let e = Env.create_context_env env meta_e in
  Env.add_inner_env env name e;

  Unification.get_as_value ctx.sc_unification_ctx uni_id


and declare_pre_function id_name meta_variables parent_env ctx =
  (* accept multiple definition for overload *)
  let (base_env, _) = Env.MultiSetOp.find_or_add parent_env id_name Env.Kind.Function in

  let fenv_r = Env.FunctionOp.empty_record id_name in
  let fenv = Env.create_context_env parent_env (
                                      Env.Function (
                                          Env.empty_lookup_table (),
                                          fenv_r)
                                    ) in
  (* declare meta variables if exist *)
  let template_vals = List.map (declare_meta_var fenv ctx) meta_variables in
  fenv_r.Env.fn_template_vals <- template_vals;

  let in_template = List.length meta_variables > 0 in
  let _ = if in_template then
            Env.MultiSetOp.add_template_instances base_env fenv
          else
            Env.MultiSetOp.add_normal_instances base_env fenv
  in
  fenv


and declare_pre_class id_name meta_variables parent_env ctx =
  (* accept multiple definition for specialization *)
  let (base_env, _) = Env.MultiSetOp.find_or_add parent_env id_name Env.Kind.Class in

  let cenv_r = Env.ClassOp.empty_record id_name in
  let cenv = Env.create_context_env parent_env (
                                      Env.Class (
                                          Env.empty_lookup_table (),
                                          cenv_r)
                                    ) in
  (* declare meta variables if exist *)
  let template_vals = List.map (declare_meta_var cenv ctx) meta_variables in
  cenv_r.Env.cls_template_vals <- template_vals;

  let in_template = List.length meta_variables > 0 in
  let _ = if in_template then
            Env.MultiSetOp.add_template_instances base_env cenv
          else
            Env.MultiSetOp.add_normal_instances base_env cenv
  in
  cenv
