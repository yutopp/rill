open Batteries

module TAst = Tagged_ast
module CtfeEngine = Ctfe.Make(Llvm_codegen)

type 'env type_info = 'env Type.info_t

type 'env shared_info = {
  si_type_gen      : 'env Type.Generator.t;

  (* buildin primitive types *)
  mutable si_type_type      : 'env type_info;
  mutable si_void_type      : 'env type_info;
  mutable si_int_type       : 'env type_info;

  (* ctfe engine *)
  si_ctfe_engine            : CtfeEngine.t;
}


let is_type ty ctx =
  Type.has_same_class ty ctx.si_type_type


let check_env env =
  Env.update_status env Env.Checking

let complete_env env node =
  Env.update_status env Env.Complete;
  Env.update_rel_ast env node

let check_is_args_valid ty =
  (* TODO: implement *)
  ()


let make_default_env () =
  Env.make_root_env ()

let make_default_context root_env ctfe_engine =
  let ctx = {
    si_type_gen = Type.Generator.default ();

    si_type_type = Type.Generator.dummy_ty;
    si_void_type = Type.Generator.dummy_ty;
    si_int_type = Type.Generator.dummy_ty;

    si_ctfe_engine = ctfe_engine;
  } in

  let create_builtin_class name inner_name =
    let env = Env.create_env root_env (Env.Class (Env.empty_lookup_table (),
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
    let ty = Type.Generator.generate_type_with_cache ctx.si_type_gen cenv in
    setter ty
  in

  register_builtin_type "type" "__type_type" (fun ty -> ctx.si_type_type <- ty);

  register_builtin_type "void" "__type_void" (fun ty -> ctx.si_void_type <- ty);
  register_builtin_type "int" "__type_int" (fun ty -> ctx.si_int_type <- ty);
  ctx


let make_default_state () =
  let env = make_default_env () in
  let ctfe_engine = CtfeEngine.empty () in
  let ctx = make_default_context env ctfe_engine in
  (env, ctx)


let rec solve_forward_refs node parent_env =
  match node with
  | Ast.Module (inner, _) ->
     begin
       let name = "module" in   (* "module" is a temporary name *)
       let env = Env.create_env parent_env (
                                  Env.Module (Env.empty_lookup_table (),
                                              {
                                                Env.mod_name = name;
                                              })
                                ) in
       Env.add_inner_env parent_env name env;

       let res_node = solve_forward_refs inner env in
       let node = TAst.Module (res_node, Some env) in
       Env.update_rel_ast env node;
       node
     end

  | Ast.StatementList (nodes) ->
     let tagged_nodes = nodes |> List.map (fun n -> solve_forward_refs n parent_env) in
     TAst.StatementList tagged_nodes

  | Ast.FunctionDefStmt (name, params, opt_ret_type, body, _) ->
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
                                         Env.fn_return_type = Type.Undef;
                                         Env.fn_detail = Env.FnUndef;
                                       })
                                 ) in
       Env.MultiSetOp.add_candidates base_env fenv;

       let node = TAst.FunctionDefStmt (
                      name,
                      TAst.PrevPassNode params,
                      Option.map (fun x -> TAst.PrevPassNode x) opt_ret_type,
                      TAst.PrevPassNode body,
                      Some fenv
                    ) in
       Env.update_rel_ast fenv node;
       node
     end

  | Ast.ExternFunctionDefStmt (name, params, ret_type, extern_fname, _) ->
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
                                         Env.fn_return_type = Type.Undef;
                                         Env.fn_detail = Env.FnUndef;
                                       })
                                 ) in
       Env.MultiSetOp.add_candidates base_env fenv;

       let node = TAst.ExternFunctionDefStmt (
                      name,
                      TAst.PrevPassNode params,
                      TAst.PrevPassNode ret_type,
                      extern_fname,
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

       Env.MultiSetOp.add_candidates base_env cenv;

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

  | Ast.ExprStmt ast -> TAst.ExprStmt (TAst.PrevPassNode ast)

  | Ast.EmptyStmt -> TAst.EmptyStmt

  | Ast.AttrWrapperStmt (attr_tbl, ast) ->
     begin
       let tast = solve_forward_refs ast parent_env in
       let tattr_tbl = attr_tbl
                       |> Hashtbl.map @@ fun k v ->
                                         Option.map (fun v -> TAst.PrevPassNode v) v
       in
       TAst.AttrWrapperStmt (tattr_tbl, tast)
     end

  | _ ->
     begin
       Ast.print node;
       failwith "solve_forward_refs: unsupported node"
     end


let rec construct_env node parent_env ctx attr =
  match node with
  | TAst.Module (inner, Some env) ->
     begin
       construct_env inner env ctx attr
     end

  | TAst.StatementList (nodes) ->
     let tagged_nodes = nodes
                        |> List.map (fun n -> construct_env n parent_env ctx attr) in
     TAst.StatementList tagged_nodes

  | TAst.FunctionDefStmt (name, params_node, opt_ret_type, body, Some env) ->
     if Env.is_checked env then Env.get_rel_ast env
     else begin
       (* TODO: check duplicate *)
       let name_s = Nodes.string_of_id_string name in
       Printf.printf "function %s - unchecked\n" name_s;
       check_env env;

       (* check parameters *)
       let (params, param_types, param_venvs) =
         prepare_params env node params_node ctx attr in

       (* infer return type *)
       (* TODO: implement *)
       let return_type = ctx.si_void_type in

       (* body *)
       let nbody = analyze_inner body env ctx in

       Printf.printf "function %s - complete\n" name_s;
       let node = TAst.FunctionDefStmt (
                      name,
                      TAst.ParamsList params,
                      opt_ret_type,
                      nbody,
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
        name, params_node, ret_type, extern_fname, Some env) ->
     if Env.is_checked env then Env.get_rel_ast env
     else begin
       (* TODO: check duplicate *)
       let name_s = Nodes.string_of_id_string name in
       Printf.printf "extern function %s - unchecked\n" name_s;
       check_env env;

       (* check parameters *)
       let (params, param_types, _) =
         prepare_params env node params_node ctx attr in

       (* determine return type *)
       (* TODO: implement *)
       let return_type = ctx.si_void_type in

       (* TODO: fix *)
       let is_builtin = match attr with
           Some tbl -> Hashtbl.mem tbl "builtin"
         | None -> false
       in

       (* body *)
       Printf.printf "extern function %s - complete\n" name_s;
       let node = TAst.ExternFunctionDefStmt (
                      name,
                      TAst.ParamsList params,
                      ret_type,
                      extern_fname,
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
                                            Env.var_type = Type.Undef;
                                            Env.var_detail = Env.VarUndef;
                                          })
                                      )
            in
            Env.add_inner_env parent_env var_name venv;

            let (opt_type, opt_init_value) = init_term in
            let opt_init_value_res = match opt_init_value with
              | Some init_value -> Some (analyze_expr init_value parent_env ctx attr)
              | None -> None
            in

            (* type check for the variable *)
            let (type_node, value_node, var_ty) = match opt_type with
              (* variable type is specified *)
              | Some var_type ->
                 begin
                   failwith "not implemented"
                 end

              (* var_type is infered from initial_value *)
              | None ->
                 begin
                   let (node, var_ty) = match opt_init_value_res with
                     | Some v -> v
                     | None -> failwith "[ERROR] initial value is required";
                   in
                   (* TODO: check void value *)

                   (None, node, var_ty)
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
     let (node, ty) = analyze_expr e parent_env ctx attr in
     TAst.ExprStmt node

  | TAst.EmptyStmt -> node

  | TAst.AttrWrapperStmt (attr_tbl, ast) ->
     begin
       (* TODO: merge prev attributes *)
       construct_env ast parent_env ctx (Some attr_tbl)
     end

  | TAst.BuiltinClass _ -> node

  | _ ->
     begin
       TAst.print node;
       failwith "construct_env: unsupported node or nodes have no valid env"
     end

and analyze_expr node parent_env ctx attr =
  match node with
  | Ast.BinaryOpExpr (lhs, op, rhs) ->
     begin
       let args = [lhs; rhs] in
       let eargs = evaluate_invocation_args args parent_env ctx attr in
       let (args_nodes, args_type_details) = List.split eargs in
       List.iter check_is_args_valid args_type_details;
       let args_ty_recs = args_type_details |> List.map Type.as_unique in

       let opt_f_env =
         find_suitable_operator ~universal_search:true
                                op args_ty_recs node parent_env ctx attr in
       match opt_f_env with
       | Some f_env ->
          begin
            (* Replace BinOpCall to generic FuncCall *)
            let node = TAst.GenericCall (
                           Nodes.string_of_id_string op,
                           args_nodes,
                           Some f_env) in
            (*TODO: extract return type from f_env*)
            let f_ty = ctx.si_int_type in
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
       let (args_nodes, args_type_details) = List.split eargs in
       List.iter check_is_args_valid args_type_details;
       let args_type_records = args_type_details |> List.map Type.as_unique in
       ignore args_type_records;

       match rec_type_info with
       | Type.FunctionSetTy menv ->
          begin
            (* notmal function call*)
            let f_env =
              solve_function_overload args_type_records menv parent_env ctx attr in

            let { Env.fn_name = fname; _ } = Env.FunctionOp.get_record f_env in
            let node = TAst.GenericCall (
                           Nodes.string_of_id_string fname,
                           args_nodes,
                           Some f_env) in
            (*TODO: extract return type from f_env*)
            let f_ty = ctx.si_int_type in
            (node, f_ty)
          end
       | _ -> failwith "not implemented" (* TODO: call ctor OR operator() *)
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
       let ty = ctx.si_int_type in
       let node = TAst.Int32Lit i in

       (node, ty)
     end

  | _ ->
     begin
       Ast.print node;
       failwith "analyze_expr: unsupported node"
     end


and analyze_inner node parent_env ctx =
  let pre_node = extract_prev_pass_node node in
  analyze pre_node parent_env ctx

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

  let (nparams, param_types) = params |> List.map analyze_param |> List.split
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
       let param_envs = List.map2 make_env params param_types in
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

and solve_simple_identifier ?(do_rec_search=true) name env ctx attr =
  let name_s = Nodes.string_of_id_string name in
  Printf.printf "-> finding identitifer = %s : rec = %b\n" name_s do_rec_search;
  let oenv = if do_rec_search then
               Env.lookup env name_s
             else
               Env.find_on_env env name_s
  in
  (*Env.print env;*)

  (* Class is a value of type, thus returns "type" of type, and corresponding env.
   * Ex, "int" -> { type: type, value: int }
   *)
  let single_type_id_node name cenv attr =
    try_to_complete_env cenv ctx attr;

    (ctx.si_type_type, Some cenv)
  in

  let solve target_env =
    let { Env.er = env_r; _ } = target_env in
    match env_r with
    | Env.MultiSet (record) ->
       begin
         let num_of_candiates = List.length record.Env.ms_candidates in
         match record.Env.ms_kind with
         | Env.Kind.Class ->
            begin
              (* classes will not be overloaded. However, template classes have
               * some instances (specialized). Thus, type may be unclear...
               *)
              match num_of_candiates with
              | 1 -> single_type_id_node name (List.hd record.Env.ms_candidates) attr
              | n when n >= 1 -> failwith "not implemented : class / multi-set"
              | _ -> failwith "[ICE] unexpected : class / multi-set"
            end
         | Env.Kind.Function ->
            begin
              (* functions will be overloaded *)
              let ty = Type.FunctionSetTy target_env in
              (ty, Some target_env)
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

         (var_ty, Some target_env)
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
         ()
       end
    | None -> failwith ""
  else
    ()

and resolve_type expr env ctx attr =
  let ty = eval_expr_as_ctfe expr env ctx attr in
  ty

and eval_expr_as_ctfe expr env ctx attr =
  Printf.printf "-> eval_expr_as_ctfe : begin ; \n";
  let (nexpr, type_of_expr) = analyze_expr expr env ctx attr in
  if not (Type.is_unique_ty type_of_expr) then
    failwith "[ICE] : non unique type";



  Printf.printf "<- eval_expr_as_ctfe : end\n";
  (* WIP WIP WIP WIP WIP WIP *)
  ctx.si_int_type


and evaluate_invocation_args args env ctx attr =
  args |> List.map (fun n -> evaluate_invocation_arg n env ctx attr)

and evaluate_invocation_arg expr env ctx attr =
  (* TODO: check CTFE-able node *)
  analyze_expr expr env ctx attr


and find_suitable_operator ?(universal_search=false)
                           op_name_id_s arg_type_records expr env ctx attr =
  let op_name = Ast.Id (op_name_id_s, ()) in
  let opt_callee_function_info =
    select_member_element ~universal_search:universal_search
                          (List.hd arg_type_records) op_name env ctx attr
  in

  let check_type_and_solve_overload (callee_f_ty, callee_f_env) =
    match callee_f_ty with
    | Type.FunctionSetTy menv ->
       solve_function_overload arg_type_records menv env ctx attr
    | _ -> failwith "[ICE]: operator must be defined as function"
  in
  match opt_callee_function_info with
    Some v -> Some (check_type_and_solve_overload v)
  | None -> None


(* returns Env of function *)
and solve_function_overload arg_types mset_env ext_env ctx attr =
  let mset_record = match mset_env.Env.er with
    | Env.MultiSet r -> r
    | _ -> Env.print mset_env; failwith "[ICE] solve_function_overload : Only Multiset is accepted"
  in
  if mset_record.Env.ms_kind != Env.Kind.Function then
    failwith "";
  (* TODO: fix *)
  List.hd mset_record.Env.ms_candidates


(* this function solves types of the following
* "some_class.some_method"
* When universal_search option is true, a following code will be also solved (for UFCS).
* "some_method"
*)
and select_member_element ?(universal_search=false) recv_ty_r t_id env ctx attr =
  let r_cenv = recv_ty_r.Type.ty_cenv in
  let opt_t_id_info = solve_identifier ~do_rec_search:false t_id r_cenv ctx attr in

  match opt_t_id_info with
  | Some (t_id_info) ->
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


and analyze node env ctx =
  let snode = solve_forward_refs node env in
  construct_env snode env ctx None
