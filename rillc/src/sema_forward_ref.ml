(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries
open Type_sets
open Sema_predef

let rec solve_forward_refs ?(meta_variables=[])
                           ?(opt_attr=None)
                           node parent_env (ctx:TAst.ast Env.env_t ctx_t) =
  let in_template = List.length meta_variables > 0 in
  let declare_meta_var env (name, uni_id) =
    let meta_e = Env.MetaVariable uni_id in
    let e = Env.create_env env meta_e in
    Env.add_inner_env env name e;
    uni_id
  in
  match node with
  | Ast.StatementList (nodes) ->
     let tagged_nodes = nodes |> List.map (fun n -> solve_forward_refs n parent_env ctx) in
     TAst.StatementList tagged_nodes

  | Ast.ExprStmt ast -> TAst.ExprStmt (TAst.PrevPassNode ast)

  | Ast.ImportStmt (pkg_names, mod_name, _) ->
     begin
       let mod_env = load_module pkg_names mod_name ctx in

       let lt = Env.get_lookup_table parent_env in
       lt.Env.imported_mods <- (mod_env, Env.ModPrivate) :: lt.Env.imported_mods;

       (* remove import statements *)
       TAst.EmptyStmt
     end

  | Ast.FunctionDefStmt (name, params, opt_ret_type, body, None, _) ->
     begin
       let name_s = Nodes.string_of_id_string name in
       (* accept multiple definition for overload *)
       let base_env = Env.MultiSetOp.find_or_add parent_env name_s Env.Kind.Function in
       let fenv_r = {
         Env.fn_name = name;
         Env.fn_mangled = None;
         Env.fn_templare_var_ids = [];
         Env.fn_param_types = [];
         Env.fn_return_type = Type.undef_ty;
         Env.fn_detail = Env.FnUndef;
       } in
       let fenv = Env.create_env parent_env (
                                   Env.Function (
                                       Env.empty_lookup_table (),
                                       fenv_r)
                                 ) in
       (* declare meta variables if exist *)
       let uni_ids = List.map (declare_meta_var fenv) meta_variables in
       fenv_r.Env.fn_templare_var_ids <- uni_ids;

       let _ = if in_template then
                 Env.MultiSetOp.add_template_instances base_env fenv
               else
                 Env.MultiSetOp.add_normal_instances base_env fenv
       in

       let node = TAst.FunctionDefStmt (
                      name,
                      TAst.PrevPassNode params,
                      Option.map (fun x -> TAst.PrevPassNode x) opt_ret_type,
                      TAst.PrevPassNode body,
                      opt_attr,
                      Some fenv
                    ) in
       Env.update_rel_ast fenv node;
       node
     end

  | Ast.ExternFunctionDefStmt (name, params, ret_type, extern_fname, None, _) ->
     begin
       let name_s = Nodes.string_of_id_string name in
       (* accept multiple definition for overload *)
       let base_env = Env.MultiSetOp.find_or_add parent_env name_s Env.Kind.Function in
       let fenv_r = {
         Env.fn_name = name;
         Env.fn_mangled = None;
         Env.fn_templare_var_ids = [];
         Env.fn_param_types = [];
         Env.fn_return_type = Type.undef_ty;
         Env.fn_detail = Env.FnUndef;
       } in
       let fenv = Env.create_env parent_env (
                                   Env.Function (
                                       Env.empty_lookup_table ~init:0 (),
                                       fenv_r)
                                 ) in
       (* declare meta variables if exist *)
       let uni_ids = List.map (declare_meta_var fenv) meta_variables in
       fenv_r.Env.fn_templare_var_ids <- uni_ids;

       let _ = if in_template then
                 Env.MultiSetOp.add_template_instances base_env fenv
               else
                 Env.MultiSetOp.add_normal_instances base_env fenv
       in

       let node = TAst.ExternFunctionDefStmt (
                      name,
                      TAst.PrevPassNode params,
                      TAst.PrevPassNode ret_type,
                      extern_fname,
                      opt_attr,
                      Some fenv
                    ) in
       Env.update_rel_ast fenv node;
       node
     end

  | Ast.ExternClassDefStmt (name, extern_cname, _) ->
     begin
       let name_s = Nodes.string_of_id_string name in
       (* accept multiple definition for specialization *)
       let base_env = Env.MultiSetOp.find_or_add parent_env name_s Env.Kind.Class in
       let cenv = Env.create_env parent_env (
                                   Env.Class (
                                       Env.empty_lookup_table ~init:0 (),
                                       {
                                         Env.cls_name = name;
                                         Env.cls_mangled = Some "int32";    (* XXX *)
                                         Env.cls_detail = Env.ClsUndef;
                                       })
                                 ) in

       let _ = if in_template then
                 Env.MultiSetOp.add_template_instances base_env cenv
               else
                 Env.MultiSetOp.add_normal_instances base_env cenv
       in

       let node = TAst.ExternClassDefStmt (
                      name,
                      extern_cname,
                      Some cenv
                    ) in
       Env.update_rel_ast cenv node;
       node
     end

  | Ast.VariableDefStmt (rv, v, _) ->
     TAst.VariableDefStmt (rv, TAst.PrevPassNode v, None)

  | Ast.TemplateStmt (name, template_params, inner_node) ->
     begin
       let name_s = Nodes.string_of_id_string name in
       let base_kind = match inner_node with
         | Ast.FunctionDefStmt _
         | Ast.ExternFunctionDefStmt _ ->
            Env.Kind.Function
         | Ast.ExternClassDefStmt _ ->
            Env.Kind.Class
         | _ ->
            begin
              failwith "[ICE] template of this statement is not supported."
            end
       in
       let base_env =
         Env.MultiSetOp.find_or_add parent_env name_s base_kind
       in
       let template_env_r = {
         Env.tl_name = name;
         Env.tl_params = TAst.PrevPassNode template_params;
         Env.tl_inner_node = TAst.PrevPassNode inner_node;
       } in
       let base_env_r = Env.MultiSetOp.get_record base_env in
       base_env_r.Env.ms_templates <- template_env_r :: base_env_r.Env.ms_templates;

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
  let mod_ast = Syntax.make_ast_from_file ~default_pkg_names:raw_pkg_names
                                          ~default_mod_name:raw_mod_name
                                          filepath
  in

  match mod_ast with
  | Ast.Module (inner, pkg_names, mod_name, base_dir, _) ->
     begin
       let check_mod_name (raw_pkg_names, raw_mod_name) =
         if not (pkg_names = raw_pkg_names && mod_name = raw_mod_name) then
           failwith "[ERR] package/module names are different";
       in
       Option.may check_mod_name def_mod_info;

       let env = Env.create_env root_env (
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
    | Some r -> r (* return cache module *)
    | None ->
       begin
         let pp dirs dir_name =
           let exist dir =
             let dir_name = Filename.concat dir dir_name in
             Sys.file_exists dir_name
           in
           try [Filename.concat (List.find exist dirs) dir_name] with
           | Not_found -> []
         in
         let target_dirs = List.fold_left pp ctx.sc_module_search_dirs pkg_names in
         let target_dir = match target_dirs with
           | [dir] -> dir
           | [] -> failwith "[ERR] package not found"
           | _ -> failwith "[ICE]"
         in
         Printf.printf "import from = %s\n" target_dir;
         let filepath = Filename.concat target_dir mod_name ^ ".rill" in

         load_module_by_filepath ~def_mod_info:(Some (pkg_names, mod_name))
                                 filepath ctx
       end
