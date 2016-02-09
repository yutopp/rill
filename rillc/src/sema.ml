open Batteries
open Type_sets

module TAst = Tagged_ast

type type_info = TAst.ast Env.type_info_t

type 'env ctx_t = {
  sc_root_env       : 'env;

  sc_module_bag         : 'env Module_info.Bag.t;
  sc_module_search_dirs : string list;

  (* ctfe engine *)
  sc_ctfe_engine    : Ctfe_engine.t;

  (* type sets *)
  sc_tsets          : 'env type_sets_t;

  (* for template *)
  sc_unification_ctx    : type_info Unification.t;
}


let check_env env =
  Env.update_status env Env.Checking

let complete_env env node =
  Env.update_status env Env.Complete;
  Env.update_rel_ast env node

let check_is_args_valid ty =
  (* TODO: implement *)
  ()


module FuncMatchLevel =
  struct
    type t =
      | ExactMatch
      | QualConv
      | ImplicitConv
      | NoMatch

    let to_int = function
      | ExactMatch      -> 0
      | QualConv        -> 1
      | ImplicitConv    -> 2
      | NoMatch         -> 3

    let of_int = function
      | 0 -> ExactMatch
      | 1 -> QualConv
      | 2 -> ImplicitConv
      | 3 -> NoMatch
      | _ -> failwith "invalid"

    let bottom a b =
      of_int (max (to_int a) (to_int b))

    (* ascending order, ExactMatch -> ... -> NoMatch *)
    let compare a b =
      compare (to_int a) (to_int b)

    (* if 'a' is matched than 'b', returns true *)
    let is_better a b =
      (to_int a) < (to_int b)

    let is_same a b =
      (to_int a) = (to_int b)

    let to_string = function
      | ExactMatch      -> "ExactMatch"
      | QualConv        -> "QualConv"
      | ImplicitConv    -> "ImplicitConv"
      | NoMatch         -> "NoMatch"
  end


let make_default_env () =
  Env.make_root_env ()

let make_default_context root_env module_search_dirs =
  let type_gen = Type.Generator.default () in
  let uni_map = Unification.empty () in
  let ctfe_engine = Ctfe_engine.initialize type_gen uni_map in

  let tsets = {
    ts_type_gen = type_gen;

    ts_type_type = Type.undef_ty;
    ts_void_type = Type.undef_ty;
    ts_int_type = Type.undef_ty;
  } in
  let ctx = {
    sc_root_env = root_env;

    sc_module_search_dirs = module_search_dirs;
    sc_module_bag = Module_info.Bag.empty ();

    sc_ctfe_engine = ctfe_engine;
    sc_tsets = tsets;
    sc_unification_ctx = uni_map;
  } in

  let create_builtin_class name inner_name =
    let env = Env.create_env root_env (
                               Env.Class (Env.empty_lookup_table ~init:0 (),
                                          {
                                            Env.cls_name = name;
                                            Env.cls_detail = Env.ClsUndef;
                                          })
                             ) in
    let node = TAst.BuiltinClass (inner_name, Some env) in
    complete_env env node;
    env
  in
  let register_builtin_type name inner_name setter =
    let id_name = Nodes.Pure name in
    let cenv = create_builtin_class id_name inner_name in
    Env.add_inner_env root_env name cenv;
    let (ty, _) = Type.Generator.generate_type ctx.sc_tsets.ts_type_gen
                                               (Type.UniqueTy {
                                                    Type.ty_cenv = cenv;
                                                  })
    in
    setter ty
  in

  register_builtin_type "type" "__type_type"
                        (fun ty -> ctx.sc_tsets.ts_type_type <- ty);

  register_builtin_type "void" "__type_void"
                        (fun ty -> ctx.sc_tsets.ts_void_type <- ty);
  register_builtin_type "int" "__type_int"
                        (fun ty -> ctx.sc_tsets.ts_int_type <- ty);
  ctx


let make_default_state system_libs_dirs user_srcs_dirs =
  let module_search_dirs = system_libs_dirs @ user_srcs_dirs in

  let env = make_default_env () in
  let ctx = make_default_context env module_search_dirs in
  (env, ctx)


let rec solve_forward_refs ?(meta_variables=[])
                           ?(opt_attr=None)
                           node parent_env (ctx:TAst.ast Env.env_t ctx_t) =
  let in_template = List.length meta_variables > 0 in
  let declare_meta_var env (name, env_r) =
    let e = Env.create_env env env_r in
    Env.add_inner_env env name e
  in
  match node with
  | Ast.StatementList (nodes) ->
     let tagged_nodes = nodes |> List.map (fun n -> solve_forward_refs n parent_env ctx) in
     TAst.StatementList tagged_nodes

  | Ast.ExprStmt ast -> TAst.ExprStmt (TAst.PrevPassNode ast)

  | Ast.ImportStmt (pkg_names, mod_name, _) ->
     begin
       let mod_env = load_module pkg_names mod_name ctx in
       ignore mod_env;

       TAst.EmptyStmt
     end

  | Ast.FunctionDefStmt (name, params, opt_ret_type, body, None, _) ->
     begin
       let name_s = Nodes.string_of_id_string name in
       (* accept multiple definition for overload *)
       let base_env = Env.MultiSetOp.find_or_add parent_env name_s Env.Kind.Function in
       let fenv = Env.create_env parent_env (
                                   Env.Function (
                                       Env.empty_lookup_table (),
                                       {
                                         Env.fn_name = name;
                                         Env.fn_param_types = [];
                                         Env.fn_return_type = Type.undef_ty;
                                         Env.fn_detail = Env.FnUndef;
                                       })
                                 ) in
       (* declare meta variables if exist *)
       List.iter (fun x -> declare_meta_var fenv x) meta_variables;

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
       let fenv = Env.create_env parent_env (
                                   Env.Function (
                                       Env.empty_lookup_table ~init:0 (),
                                       {
                                         Env.fn_name = name;
                                         Env.fn_param_types = [];
                                         Env.fn_return_type = Type.undef_ty;
                                         Env.fn_detail = Env.FnUndef;
                                       })
                                 ) in
       (* declare meta variables if exist *)
       List.iter (fun x -> declare_meta_var fenv x) meta_variables;

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
       let base_env = match inner_node with
         | Ast.FunctionDefStmt _
         | Ast.ExternClassDefStmt _ ->
            begin
              Env.MultiSetOp.find_or_add parent_env name_s Env.Kind.Function
            end
         | _ ->
            begin
              failwith "[ICE] template of this statement is not supported."
            end
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
  let _ = match mod_env with
    | Some r -> failwith ""
    | None -> ()
  in

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


let rec construct_env node parent_env ctx opt_chain_attr =
  match node with
  | TAst.Module (inner, pkg_names, mod_name, base_dir, Some env) ->
     begin
       construct_env inner env ctx opt_chain_attr
     end

  | TAst.StatementList (nodes) ->
     let f n = construct_env n parent_env ctx opt_chain_attr in
     let tagged_nodes = nodes |> List.map f in
     TAst.StatementList tagged_nodes

  | TAst.FunctionDefStmt (
        name, params_node, opt_ret_type, body, opt_attr, Some env
      ) ->
     if Env.is_checked env then Env.get_rel_ast env
     else begin
       (* TODO: check duplicate *)
       let name_s = Nodes.string_of_id_string name in
       Printf.printf "function %s - unchecked\n" name_s;
       check_env env;

       (* check parameters *)
       let (params, param_types, param_venvs) =
         prepare_params env node params_node ctx opt_attr in

       (* infer return type *)
       (* TODO: implement *)
       let return_type = ctx.sc_tsets.ts_void_type in

       (* body *)
       let nbody = analyze_inner body env ctx opt_attr in

       Printf.printf "function %s - complete\n" name_s;
       let node = TAst.FunctionDefStmt (
                      name,
                      TAst.ParamsList params,
                      opt_ret_type,
                      nbody,
                      opt_attr,
                      Some env
                    ) in

       (* update record *)
       let detail_r = Env.FnRecordNormal {
                          Env.fn_n_param_envs = param_venvs;
                        } in
       let r = Env.FunctionOp.get_record env in
       r.Env.fn_param_types <- param_types;
       r.Env.fn_return_type <- return_type;
       r.Env.fn_detail <- detail_r;

       complete_env env node;
       node
     end

  | TAst.ExternFunctionDefStmt (
        name, params_node, ret_type, extern_fname, opt_attr, Some env) ->
     if Env.is_checked env then Env.get_rel_ast env
     else begin
       (* TODO: check duplicate *)
       let name_s = Nodes.string_of_id_string name in
       Printf.printf "extern function %s - unchecked\n" name_s;
       check_env env;

       (* check parameters *)
       let (params, param_types, _) =
         prepare_params env node params_node ctx opt_attr in

       (* determine return type *)
       (* TODO: implement *)
       let return_type = ctx.sc_tsets.ts_void_type in

       (* TODO: fix *)
       let is_builtin = match opt_attr with
           Some tbl -> Hashtbl.mem tbl "builtin"
         | None -> false
       in

       (* body *)
       Printf.printf "extern function %s - complete (builtin=%b)\n" name_s is_builtin;
       let node = TAst.ExternFunctionDefStmt (
                      name,
                      TAst.ParamsList params,
                      ret_type,
                      extern_fname,
                      opt_attr,
                      Some env
                    ) in

       (* update record *)
       let detail_r = Env.FnRecordExtern {
                          Env.fn_e_name = extern_fname;
                          Env.fn_e_is_builtin = is_builtin;
                        } in
       let r = Env.FunctionOp.get_record env in
       r.Env.fn_param_types <- param_types;
       r.Env.fn_return_type <- return_type;
       r.Env.fn_detail <- detail_r;

       complete_env env node;
       node
     end

  | TAst.ExternClassDefStmt (
        name, extern_cname, Some env) ->
     if Env.is_checked env then Env.get_rel_ast env
     else begin
       (* TODO: check duplicate *)
       let name_s = Nodes.string_of_id_string name in
       Printf.printf "extern class %s - unchecked\n" name_s;
       check_env env;

       (* currently, do not remake a node like other nodes *)
       Printf.printf "extern class %s - complete\n" name_s;

       (* update record *)
       let detail_r = Env.ClsRecordExtern {
                          Env.cls_e_name = extern_cname;
                        } in
       let r = Env.ClassOp.get_record env in
       r.Env.cls_detail <- detail_r;

       complete_env env node;
       node
     end

  | TAst.VariableDefStmt (rv, v, opt_env) ->
     begin
       match opt_env with
       | Some env -> failwith "unexpected"
       | None ->
          begin
            let (var_name, init_term) =
              match extract_prev_pass_node v with
              | Ast.VarInit vi -> vi
              | _ -> failwith "unexpected node"
            in
            check_id_is_defined_uniquely parent_env var_name;

            let venv = Env.create_env parent_env (
                                        Env.Variable (
                                          {
                                            Env.var_name = var_name;
                                            Env.var_type = Type.undef_ty;
                                            Env.var_detail = Env.VarUndef;
                                          })
                                      )
            in
            Env.add_inner_env parent_env var_name venv;

            let (opt_type, opt_init_value) = init_term in
            let opt_init_value_res = match opt_init_value with
              | Some init_value -> Some (analyze_expr init_value parent_env
                                                      ctx opt_chain_attr)
              | None -> None
            in

            (* type check for the variable *)
            let (type_node, value_node, var_ty) = match opt_type with
              (* variable type is specified *)
              | Some var_type_node ->
                 begin
                   let (var_ty, type_expr) =
                     resolve_type_with_node var_type_node parent_env ctx opt_chain_attr
                   in
                   let res_expr_node = match opt_init_value_res with
                     | Some (expr_node, expr_ty) ->
                        begin
                          let (m_level, m_filter) =
                            convert_type expr_ty var_ty parent_env ctx opt_chain_attr
                          in
                          if m_level = FuncMatchLevel.NoMatch then
                            failwith "[ERR] cannot conver type";
                          (* TODO: call filter, call copy/mode ctor *)
                          expr_node
                        end
                     | None ->
                        begin
                          (* TODO: implement call defaut constructor *)
                          failwith "not implemented //"
                        end
                   in
                   (Some type_expr, res_expr_node, var_ty)
                 end

              (* var_type is infered from initial_value *)
              | None ->
                 begin
                   let (expr_node, expr_ty) = match opt_init_value_res with
                     | Some v -> v
                     | None -> failwith "[ERROR] initial value is required";
                   in
                   (* TODO: check void value *)

                   (None, expr_node, expr_ty)
                 end
            in

            let node = TAst.VariableDefStmt (
                           rv,
                           TAst.VarInit (var_name, (type_node, Some value_node)),
                           Some venv
                         ) in

            let detail_r = Env.VarRecordNormal () in
            let r = Env.VariableOp.get_record venv in
            r.Env.var_type <- var_ty;
            r.Env.var_detail <- detail_r;

            complete_env venv node;
            node
          end
     end

  | TAst.ExprStmt (TAst.PrevPassNode e) ->
     let (node, ty) = analyze_expr e parent_env ctx opt_chain_attr in
     TAst.ExprStmt node

  | TAst.EmptyStmt -> node

  | TAst.BuiltinClass _ -> node

  | _ ->
     begin
       TAst.print node;
       failwith "construct_env: unsupported node or nodes have no valid env"
     end

and analyze_expr node parent_env ctx attr : ('node * type_info)=
  match node with
  | Ast.BinaryOpExpr (lhs, op, rhs) ->
     begin
       let args = [lhs; rhs] in
       let eargs = evaluate_invocation_args args parent_env ctx attr in
       let (args_nodes, args_types) = List.split eargs in

       let opt_fs_and_args =
         find_suitable_operator ~universal_search:true
                                op args_types node parent_env ctx attr in
       match opt_fs_and_args with
       | Some (f_env, conv_filters) ->
          begin
            (* Replace BinOpCall to generic FuncCall *)
            let node = TAst.GenericCall (
                           Nodes.string_of_id_string op,
                           args_nodes,
                           Some f_env) in
            (*TODO: extract return type from f_env*)
            let f_ty = get_builtin_int_type ctx in
            (node, f_ty)
          end
       | None ->
          (* TODO: error message *)
          failwith "binary operator is not found"
     end

  | Ast.UnaryOpExpr (op, expr) ->
     begin
       failwith "u op"
     end

  | Ast.CallExpr (reciever, args) ->
     begin
       let (rec_node, rec_type_info) = analyze_expr reciever parent_env ctx attr in

       let eargs = evaluate_invocation_args args parent_env ctx attr in
       let (args_nodes, args_types) = List.split eargs in
       List.iter check_is_args_valid args_types;

       match Type.type_sort rec_type_info with
       | Type.FunctionSetTy menv ->
          begin
            (* notmal function call*)
            let (f_env, conv_filters) =
              solve_function_overload args_types None menv parent_env ctx attr in

            let { Env.fn_name = fname; _ } = Env.FunctionOp.get_record f_env in
            let node = TAst.GenericCall (
                           Nodes.string_of_id_string fname,
                           args_nodes,
                           Some f_env) in
            (*TODO: extract return type from f_env*)
            let f_ty = get_builtin_int_type ctx in
            (node, f_ty)
          end
       | _ -> failwith "not implemented//" (* TODO: call ctor OR operator() *)
     end

  | Ast.Id (name, _) as id_node ->
     begin
       (* WIP *)
       let (ty, trg_env) = match solve_identifier id_node parent_env ctx attr with
           Some v -> v
         | None -> failwith "id not found"  (* TODO: change to exception *)
       in
       let node = TAst.Id (name, trg_env) in

       (node, ty)
     end

  | Ast.Int32Lit (i) ->
     begin
       (* WIP *)
       let ty = get_builtin_int_type ctx in
       let node = TAst.Int32Lit i in

       (node, ty)
     end

  | Ast.ArrayLit (elems) ->
     begin
       let (n_nodes, n_tys) = elems
                          |> List.map (fun e -> analyze_expr e parent_env ctx attr)
                          |> List.split
       in
       let n_array = TAst.ArrayLit (n_nodes) in
       let n_array_ty = List.hd n_tys in
       (n_array, n_array_ty)
     end

  | _ ->
     begin
       Ast.print node;
       failwith "analyze_expr: unsupported node"
     end


and analyze_inner node parent_env ctx opt_chain_attr =
  let pre_node = extract_prev_pass_node node in
  analyze ~opt_attr:opt_chain_attr pre_node parent_env ctx

and extract_prev_pass_node node =
  match node with
  | TAst.PrevPassNode n -> n
  | _ -> failwith "not prev node"


and prepare_params env func_decl_node params_node ctx attr =
  match params_node with
  | TAst.PrevPassNode (Ast.ParamsList ps) ->
     begin
       declare_function_params env func_decl_node ps ctx attr
     end
  | _ -> failwith "check_params / unexpected"

(*
* if parameter has name, create variable env
* DO NOT forget to call Env.add_inner_env
*)
and make_parameter_env f_env opt_param_name param_ty ctx =
  let make_env name =
    let detail_r = Env.VarRecordNormal () in
    let venv = Env.create_env f_env (
                                Env.Variable (
                                    {
                                      Env.var_name = name;
                                      Env.var_type = param_ty;
                                      Env.var_detail = detail_r;
                                    })
                              )
    in
    Env.update_status venv Env.Complete;
    venv
  in
  opt_param_name |> Option.map make_env

and declare_function_params f_env func_decl_node params ctx attr =
  let analyze_param param =
    let (name, init_part) = param in
    let (param_ty, default_value) = match init_part with
    (* Ex. :int = 10 *)
    | (Some type_expr, Some defalut_val) ->
       begin
         failwith "declare_function_params : not implemented / default value of param"
       end

    (* Ex. :int *)
    | (Some type_expr, None) ->
       begin
         let ty = resolve_type type_expr f_env ctx attr in
         (ty, None)
       end

    (* Ex. = 10 *)
    | (None, Some defalut_val) ->
       begin
         (* type is inferenced from defalut_val *)
         failwith "declare_function_params : not implemented / infer type from value"
       end

    | _ ->
       (* TODO: change to exception *)
       failwith "type or default value is required"
    in

    let ninit_part = (None, default_value) in   (* type node is no longer necessary *)
    let nparam = (name, ninit_part) in
    (nparam, param_ty)
  in

  let (nparams, param_types) =
    params |> List.map analyze_param |> List.split
  in
  match func_decl_node with
  | TAst.FunctionDefStmt _ ->
     begin
       let make_env param ty =
         let (opt_name, _) = param in
         make_parameter_env f_env opt_name ty ctx
       in
       let declare_env param opt_env =
         let declare name =
           let env = Option.get opt_env in
           Env.add_inner_env f_env name env
         in
         let (opt_name, _) = param in
         Option.may declare opt_name
       in
       (* first, make environments *)
       let param_envs = List.map2 make_env params param_types in
       (* add var envs to fenv (declare) *)
       List.iter2 declare_env params param_envs;

       (* TODO: support functions that have recievers. ex, member function (this/self) *)
       (nparams, param_types, param_envs)
     end

  | TAst.ExternFunctionDefStmt _ ->
     begin
       (*params*)
       (nparams, param_types, [])
     end

  | _ -> failwith "declare_function_params: not supported"


and check_id_is_defined_uniquely env id =
  let res = Env.find_on_env env id in
  match res with
    Some _ -> failwith "same ids are defined"
  | None -> ()


and solve_identifier ?(do_rec_search=true) id_node env ctx attr =
  match id_node with
  | Ast.Id (name, _) ->
     solve_simple_identifier ~do_rec_search:do_rec_search name env ctx attr
  | _ -> failwith "unsupported ID type"

and solve_simple_identifier
      ?(do_rec_search=true) name search_base_env ctx attr =
  let name_s = Nodes.string_of_id_string name in
  Printf.printf "-> finding identitifer = %s : rec = %b\n" name_s do_rec_search;
  let oenv = if do_rec_search then
               Env.lookup search_base_env name_s
             else
               Env.find_on_env search_base_env name_s
  in
  (*Env.print env;*)

  (* Class is a value of type, thus returns "type" of type, and corresponding env.
   * Ex, "int" -> { type: type, value: int }
   *)
  let single_type_id_node name cenv attr =
    try_to_complete_env cenv ctx attr;

    (ctx.sc_tsets.ts_type_type, Some cenv)
  in

  let solve env : (type_info * 'env option)=
    let { Env.er = env_r; _ } = env in
    match env_r with
    | Env.MultiSet (record) ->
       begin
         match record.Env.ms_kind with
         | Env.Kind.Class ->
            begin
              (* classes will not be overloaded. However, template classes have
               * some instances (specialized). Thus, type may be unclear...
               *)
              if List.length record.Env.ms_templates <> 0 then
                let (ty, _) =
                  Type.Generator.generate_type ctx.sc_tsets.ts_type_gen
                                               (Type.ClassSetTy env)
                in
                (ty, Some env)
              else
                let num_of_candiates =
                  List.length record.Env.ms_normal_instances in
                match num_of_candiates with
                | 1 ->
                   let single_cenv = List.hd record.Env.ms_normal_instances in
                   single_type_id_node name single_cenv attr
                | n when n >= 1 ->
                   let (ty, _) =
                     Type.Generator.generate_type ctx.sc_tsets.ts_type_gen
                                                  (Type.ClassSetTy env)
                   in
                   (ty, Some env)
                | _ -> failwith "[ICE] unexpected : class / multi-set"
            end
         | Env.Kind.Function ->
            begin
              (* functions will be overloaded *)
              let (ty, _) =
                Type.Generator.generate_type ctx.sc_tsets.ts_type_gen
                                             (Type.FunctionSetTy env)
              in
              (ty, Some env)
            end
         | _ -> failwith "unexpected env : multi-set kind"
       end

    (* only builtin classes may be matched *)
    | Env.Class (_) -> single_type_id_node name env attr

    | Env.Variable (vr) ->
       begin
         let {
           Env.var_type = var_ty;
           _
         } = vr in
         (* TODO: check class variable *)

         (var_ty, Some env)
       end

    (* returns **type** of MetaVariable, NOT value *)
    | Env.MetaVariable (uni_id) ->
       begin
         let (term_uni_id, c) =
           Unification.search_type_until_terminal ctx.sc_unification_ctx uni_id
         in
         match c with
         | (Unification.Val ty) ->
            (ty, Some env)
         | (Unification.Undef) ->
            let (ty, _) =
              Type.Generator.generate_type ctx.sc_tsets.ts_type_gen
                                           (Type.NotDetermined (
                                                uni_id, ctx.sc_unification_ctx))
            in
            (ty, Some env)
         | _ -> failwith "[ICE] meta ver"
       end

    | _ -> failwith "solve_simple_identifier: unexpected env"
  in

  oenv |> Option.map (fun e -> solve e)


and try_to_complete_env env ctx attr =
  if Env.is_incomplete env then
    match env.Env.rel_node with
    | Some (node) ->
       begin
         let parent_env = Option.get env.Env.parent_env in
         ignore @@ construct_env node parent_env ctx attr;
         if not (Env.is_complete env) then
           failwith "? recursice definition is appeared"; (* TODO: exception *)
         ()
       end
    | None -> failwith ""
  else
    ()


and convert_type src_ty dist_ty ext_env ctx attr =
  if Type.is_same src_ty dist_ty then
    (FuncMatchLevel.ExactMatch, None)
  else begin
    failwith "convert_type / not implemented"
  end


and resolve_type expr env ctx attr =
  let (ty, _) = resolve_type_with_node expr env ctx attr in
  ty

and resolve_type_with_node expr env ctx attr =
  let (ctfe_val, nexpr) = eval_expr_as_ctfe expr env ctx attr in
  let ty = match ctfe_val with
    | Ctfe_value.Type ty -> ty
  in
  (ty, nexpr)
(*  | _ ->
     begin
       (* TODO: change to exception *)
       failwith "This expression must be type"
     end
 *)

and eval_expr_as_ctfe expr env ctx attr =
  Printf.printf "-> eval_expr_as_ctfe : begin ; \n";
  let (nexpr, type_of_expr) = analyze_expr expr env ctx attr in
  if not (Type.is_unique_ty type_of_expr) then
    failwith "[ICE] : non unique type";

  (*TAst.print nexpr;*)
  let ctfe_val =
    Ctfe_engine.execute ctx.sc_ctfe_engine nexpr type_of_expr ctx.sc_tsets in

  Printf.printf "<- eval_expr_as_ctfe : end\n";
  (ctfe_val, nexpr)


and evaluate_invocation_args args env ctx attr =
  args |> List.map (fun n -> evaluate_invocation_arg n env ctx attr)

and evaluate_invocation_arg expr env ctx attr =
  (* TODO: check CTFE-able node *)
  analyze_expr expr env ctx attr


and find_suitable_operator ?(universal_search=false)
                           op_name_id_s arg_types expr env ctx attr =
  let op_name = Ast.Id (op_name_id_s, ()) in
  let opt_callee_function_info =
    select_member_element ~universal_search:universal_search
                          (List.hd arg_types) op_name env ctx attr
  in

  let check_type_and_solve_overload (callee_f_ty, callee_f_env) =
    match Type.type_sort callee_f_ty with
    | Type.FunctionSetTy menv ->
       solve_function_overload arg_types None menv env ctx attr
    | _ -> failwith "[ICE]: operator must be defined as function"
  in
  match opt_callee_function_info with
    Some v -> Some (check_type_and_solve_overload v)
  | None -> None


(* returns Env of function *)
and solve_function_overload arg_types opt_tamplate_args mset_env ext_env ctx attr =
  let mset_record = match mset_env.Env.er with
    | Env.MultiSet r -> r
    | _ -> Env.print mset_env; failwith "[ICE] solve_function_overload : Only Multiset is accepted"
  in
  if mset_record.Env.ms_kind <> Env.Kind.Function then
    failwith "[ICE] solve_function_overload : sort of menv must be function.";

  match opt_tamplate_args with
  | Some _->
     failwith "not implemented"

  | None ->
     begin
       (* TODO: fix *)
       let (normal_f_level, normal_fs_and_args) =
         find_suitable_functions mset_record.Env.ms_normal_instances arg_types
                                 ext_env ctx attr
       in
       Printf.printf "!! normal function candidates = %s / %d\n"
                     (FuncMatchLevel.to_string normal_f_level)
                     (List.length normal_fs_and_args);

       let (f_level, fs_and_args) =
         if normal_f_level <> FuncMatchLevel.ExactMatch then
           begin
             instantiate_function_templates mset_env arg_types ext_env ctx attr;

             let (instanced_f_level, instanced_fs_and_args) =
               find_suitable_functions mset_record.Env.ms_template_instances arg_types
                                       ext_env ctx attr
             in
             Printf.printf "!! instanced function candidates = %s / %d\n"
                           (FuncMatchLevel.to_string instanced_f_level)
                           (List.length instanced_fs_and_args);
             if FuncMatchLevel.is_better instanced_f_level normal_f_level then
               (instanced_f_level, instanced_fs_and_args)
             else
               (normal_f_level, normal_fs_and_args)
           end
         else (normal_f_level, normal_fs_and_args)
       in

       if f_level = FuncMatchLevel.NoMatch then
         failwith "[ERR] no match";  (* TODO: change to exception *)

       assert (List.length fs_and_args <> 0);
       if (List.length fs_and_args) > 1 then
         failwith "[ERR] anbigous";  (* TODO: change to exception *)

       List.hd fs_and_args
     end


and find_suitable_functions f_candidates arg_types ext_env ctx attr =
  Printf.printf "number of candidates = %d\n" (List.length f_candidates);

  let calc_match_level f_env =
    try_to_complete_env f_env ctx attr;
    let f_record = Env.FunctionOp.get_record f_env in

    (* check number of args *)
    (* TODO: support dynamic variadic args *)
    if (List.length f_record.Env.fn_param_types <> List.length arg_types) then
      failwith "~~~~";

    let (match_levels, conv_funcs) =
      List.map2 (fun s d -> convert_type s d ext_env ctx attr)
                arg_types
                f_record.Env.fn_param_types
      |> List.split
    in

    let total_f_level =
      List.fold_left FuncMatchLevel.bottom FuncMatchLevel.ExactMatch match_levels in

    (total_f_level, f_env, conv_funcs)
  in

  let collect (cur_order, fs_and_args) candidate =
    let (total_f_level, f_env, conv_funcs) = calc_match_level candidate in
    if FuncMatchLevel.is_better total_f_level cur_order then
      (* if more better function is found, remake candidates and raise level *)
      (total_f_level, [(f_env, conv_funcs)])
    else if FuncMatchLevel.is_same total_f_level cur_order then
      (* if this function has same match level, add to candicates *)
      (cur_order, (f_env, conv_funcs) :: fs_and_args)
    else
      (* ignore(do NOT add) function which has lower match level *)
      (cur_order, fs_and_args)
  in
  let (level, fs_and_args) =
    List.fold_left collect (FuncMatchLevel.NoMatch, []) f_candidates in

  if level = FuncMatchLevel.NoMatch then
    (level, [])
  else
    (level, fs_and_args)


and instantiate_function_templates menv arg_types ext_env ctx attr =
  let mset_record = Env.MultiSetOp.get_record menv in
  let instantiate t_env_record =
    Printf.printf "&& start instantiation\n";
    let template_params =
      match t_env_record.Env.tl_params with
      | TAst.PrevPassNode (Ast.TemplateParamsList params) -> params
      | _ -> failwith "[ICE] unexpected template params"
    in

    (* In this context, value of MetaVar is treated as TYPE *)

    let (meta_var_names, meta_var_inits) =
      List.split template_params
    in

    (* temporary environment for evalutate meta variables *)
    let temp_env =
      Env.create_env ext_env (Env.Temporary (Env.empty_lookup_table ())) in

    (* generate meta variables which have no value and no type *)
    let generate_meta_var name =
      let uni_id =
        Unification.generate_uni_id ctx.sc_unification_ctx in
      let e = Env.create_env temp_env (Env.MetaVariable uni_id) in
      (uni_id, e)
    in
    let (uni_ids, meta_envs) =
      List.map generate_meta_var meta_var_names
      |> List.split
    in
    (* declare *)
    List.iter2 (fun n e -> Env.add_inner_env temp_env n e)
               meta_var_names meta_envs;

    (* let *)
    let get_meta_var_ty_env meta_var_name =
      let s_meta_var_name = Nodes.Pure meta_var_name in
      let opt_meta_var =
        solve_simple_identifier ~do_rec_search:false
                                s_meta_var_name temp_env ctx attr
      in
      let ty_env = match opt_meta_var with
        | Some v -> v
        | None ->
           (*TODO change to exception *)
           failwith "template parameter is not found"
      in
      ty_env
    in
    let meta_vars_ty_env = List.map get_meta_var_ty_env meta_var_names in

    (* set types of meta var *)
    let set_meta_var_type (ty, env) opt_init =
      match opt_init with
      (* :U *)
      | Some (Some ty, None) -> failwith "not supported"
      (* = V *)
      | Some (None, Some value) -> failwith "not supported"
      (* :U = V *)
      | Some (Some ty, Some value) -> failwith "not supported"
      | Some (None, None) -> failwith "[ICE] unexpected"

      | None ->
         begin
           match Type.type_sort ty with
           | Type.NotDetermined (uni_t_id, uni_map) ->
              begin
                (* unify *)
                unify_type ty ctx.sc_tsets.ts_type_type;
              end
           | _ -> failwith "not implemented 01"
         end
    in
    List.iter2 set_meta_var_type meta_vars_ty_env meta_var_inits;


    let set_default_value (name, _)=
      let s_name = Nodes.Pure name in
      let opt_meta_var =
        solve_simple_identifier ~do_rec_search:false
                                s_name temp_env ctx attr
      in
      let (ty, uni_id) = match opt_meta_var with
        | Some (ty, Some {Env.er = Env.MetaVariable (uni_id); _}) -> (ty, uni_id)
        | _ -> failwith ""
      in
      if Type.has_same_class ty ctx.sc_tsets.ts_type_type then
        begin
          let (ud_ty, _) =
            Type.Generator.generate_type ctx.sc_tsets.ts_type_gen
                                         (Type.NotDetermined (
                                              uni_id,
                                              ctx.sc_unification_ctx))
          in
          let type_val = Ctfe_value.Type ud_ty in
          Unification.update_value ctx.sc_unification_ctx uni_id type_val
        end
      else
        failwith "not implemented / inst"
    in
    template_params |> List.iter set_default_value;

    List.iter (fun c -> print_meta_var c ctx) uni_ids;

    Printf.printf "\nREACHED / set_default_value\n";

    (* TODO: implement match values by template args *)

    (* match valuse by arg types *)
    let inner_node = t_env_record.Env.tl_inner_node in
    let inner_node = match inner_node with
      | TAst.PrevPassNode n -> n
      | _ -> failwith ""
    in
    let parameters = match inner_node with
      | Ast.FunctionDefStmt (_, Ast.ParamsList params, _, _, _, _) -> params
      | Ast.ExternFunctionDefStmt (_, Ast.ParamsList params, _, _, _, _) -> params
      | _ -> failwith ""
    in
    let get_param_type (_, init) =
      match init with
      | (Some ty_node, _) -> resolve_type ty_node temp_env ctx attr
      | _ -> failwith "not implemented / param nodes"
    in
    let param_types = List.map get_param_type parameters in

    List.iter print_type param_types;
    Printf.printf "\nREACHED / param_types\n";

    let params_ = List.map (fun x -> Ctfe_value.Type x) param_types in
    let args_ = List.map (fun x -> Ctfe_value.Type x) arg_types in
    List.iter2 unify_arg_type params_ args_;

    Printf.printf "\nREACHED / unify_arg_type\n";


    let normalize_meta_var uni_id =
      let normalize_uni_value uni_id =
        let (last_uni_id, c) =
          Unification.search_value_until_terminal ctx.sc_unification_ctx uni_id
        in
        match c with
        | Unification.Val v ->
           Unification.update_value ctx.sc_unification_ctx uni_id v
        | _ -> failwith "[ERR] not resolved"
      in
      let normalize_uni_type uni_id =
        let (last_uni_id, c) =
          Unification.search_type_until_terminal ctx.sc_unification_ctx uni_id
        in
        match c with
        | Unification.Val v ->
           Unification.update_type ctx.sc_unification_ctx uni_id v
        | _ -> failwith "[ERR] not resolved"
      in

      normalize_uni_value uni_id;
      normalize_uni_type uni_id
    in
    List.iter normalize_meta_var uni_ids;

    let make_mv_set name uni_id =
      let mv = Env.MetaVariable uni_id in
      (name, mv)
    in
    let mvs = List.map2 make_mv_set meta_var_names uni_ids in

    (**)
    let env_parent = Option.get menv.Env.parent_env in
    let _ = analyze ~meta_variables:mvs inner_node env_parent ctx in

    ()
  in
  List.iter instantiate mset_record.Env.ms_templates


and unify_type lhs rhs =
  match (lhs, rhs) with
  | ({Type.ti_sort = Type.NotDetermined (lhs_uni_t_id, lhs_uni_map)},
     {Type.ti_sort = Type.NotDetermined (rhs_uni_t_id, rhs_uni_map)}) ->
     begin
       assert (lhs_uni_map == rhs_uni_map); (* same instance *)
       let uni_map = lhs_uni_map in
       Unification.link_type uni_map lhs_uni_t_id rhs_uni_t_id
     end
  | (({Type.ti_sort = (Type.UniqueTy _)} as ty),
     {Type.ti_sort = Type.NotDetermined (uni_t_id, uni_map)})
  | ({Type.ti_sort = Type.NotDetermined (uni_t_id, uni_map)},
     ({Type.ti_sort = (Type.UniqueTy _)} as ty))
    ->
     begin
       Unification.update_type uni_map uni_t_id ty
     end
  | _ -> failwith "[ICE] unify_type"


and unify_type_value lhs rhs =
  match (lhs, rhs) with
  | ({Type.ti_sort = Type.NotDetermined (lhs_uni_t_id, lhs_uni_map)},
     {Type.ti_sort = Type.NotDetermined (rhs_uni_t_id, rhs_uni_map)}) ->
     begin
       assert (lhs_uni_map == rhs_uni_map); (* same instance *)
       Printf.printf "!! unify_type_value(T/T) / %d = %d\n" lhs_uni_t_id rhs_uni_t_id;
       let uni_map = lhs_uni_map in
       Unification.link_value uni_map lhs_uni_t_id rhs_uni_t_id
     end
  | (({Type.ti_sort = (Type.UniqueTy _)} as ty),
     {Type.ti_sort = Type.NotDetermined (uni_t_id, uni_map)})
  | ({Type.ti_sort = Type.NotDetermined (uni_t_id, uni_map)}
    , ({Type.ti_sort = (Type.UniqueTy _)} as ty))
    ->
     begin
       Printf.printf "!! unify_type_value(T|V) / %d -> value\n" uni_t_id;
       Unification.update_value uni_map uni_t_id (Ctfe_value.Type ty)
     end
  | (lhs, rhs) ->
     begin
       print_type lhs;
       print_type rhs;
       failwith "[ICE] unify_value_type"
     end


and unify_arg_type lhs rhs =
  match (lhs, rhs) with
  | (Ctfe_value.Type lhs_ty, Ctfe_value.Type rhs_ty) ->
     begin
       unify_type_value lhs_ty rhs_ty
     end


and print_type ty =
  match Type.type_sort ty with
  | Type.UniqueTy tr ->
     begin
       let {
         Type.ty_cenv = cenv;
         _
       } = tr in
       let cls_r = Env.ClassOp.get_record cenv in
       let name = Nodes.string_of_id_string cls_r.Env.cls_name in
       Printf.printf "@==> %s\n" name
     end
  | Type.FunctionSetTy _ -> Printf.printf "function set\n"
  | Type.ClassSetTy _ -> Printf.printf "class set\n"
  | Type.Undef -> Printf.printf "@undef@\n"
  | Type.NotDetermined (uni_id, uni_map) ->
     begin
       Printf.printf "@not determined [%d]\n" uni_id
     end


and print_ctfe_value value =
  match value with
  | Ctfe_value.Type ty -> print_type ty


and print_meta_var uni_id ctx =
  let (_, ty_c) =
    Unification.search_type_until_terminal ctx.sc_unification_ctx uni_id
  in
  let (_, val_c) =
    Unification.search_value_until_terminal ctx.sc_unification_ctx uni_id
  in
  Printf.printf "===\n";
  let _ = match ty_c with
    | Unification.Val ty -> print_type ty
    | _ -> Printf.printf "link or undef\n"
  in
  match val_c with
  | Unification.Val value -> print_ctfe_value value
  | _ -> Printf.printf "link or undef\n";


and prepare_template_params params_node ctx =
  match params_node with
  | TAst.PrevPassNode (Ast.TemplateParamsList params) ->
     begin
       let normalize param =
         let (meta_var_name, opt_init) = param in
         match opt_init with
         (* :U *)
         | Some (Some ty, None) -> failwith "not supported"
         (* = V *)
         | Some (None, Some value) -> failwith "not supported"
         (* :U = V *)
         | Some (Some ty, Some value) -> failwith "not supported"
         | Some (None, None) -> failwith "[ICE] unexpected"

         | None -> (meta_var_name, (ctx.sc_tsets.ts_type_type, None))
       in
       List.map normalize params
     end
  | _ -> failwith "[ICE] unexpected template params"


(* this function solves types of the following
* "some_class.some_method"
* When universal_search option is true, a following code will be also solved (for UFCS).
* "some_method"
*)
and select_member_element ?(universal_search=false) recv_ty t_id env ctx attr =
  let recv_ty_r = Type.as_unique recv_ty in
  let r_cenv = recv_ty_r.Type.ty_cenv in
  let opt_ty_ctx = solve_identifier ~do_rec_search:false t_id r_cenv ctx attr in

  match opt_ty_ctx with
  | Some (ty_info) ->
     begin
       (* member env named id is found in recv_ty_r! *)
       failwith "select_member_element: not implemented / select"
     end
  | None ->
     begin
       (* not found *)
       (* first, find the member function like "opDispatch" in recv_ty_r *)
       (* TODO: implement *)

       (* second, do universal_search *)
       if universal_search then begin
         (* TODO: exclude class envs *)
         let opt_t_id_info = solve_identifier t_id env ctx attr in
         let solve t_id_info =
           t_id_info
         in
         Option.map solve opt_t_id_info
       end else
         None
     end

and get_builtin_int_type ctx =
  ctx.sc_tsets.ts_int_type
    (*
  let ty = ctx.sc_tsets.ts_int_type in
  match Type.type_sort ctx.sc_tsets.ts_int_type with
  | Type.Undef ->
     begin
       let res =
         solve_simple_identifier ~do_rec_search:false (Nodes.Pure "int") ctx.sc_root_env ctx None
       in
       match res with
       | Some (ty, Some c_env) when ty = ctx.sc_tsets.ts_type_type ->
          begin
            try_to_complete_env c_env ctx None;
            failwith "Yo"
          end
       | _ -> failwith "[ICE]"
     end
  | _ -> ty
     *)


and analyze ?(meta_variables=[]) ?(opt_attr=None) node env ctx =
  let snode = solve_forward_refs ~meta_variables:meta_variables node env ctx in
  construct_env snode env ctx opt_attr


let analyze_module mod_env ctx =
  let mod_node = Option.get mod_env.Env.rel_node in
  construct_env mod_node ctx.sc_root_env ctx None
