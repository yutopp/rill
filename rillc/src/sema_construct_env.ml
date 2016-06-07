(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries
open Stdint
open Type_sets
open Value_category
open Sema_definitions
open Sema_forward_ref
open Sema_utils

type storage_operation =
  | SoExitScope
  | SoParamPassing
  | SoArrayElement of int


let rec print_error ?(loc=None) err =
  match err with
  | Error.DifferentArgNum (params_num, args_num) ->
     Printf.printf "%s:\nError: requires %d but given %d\n" (Nodes.Loc.to_string loc) params_num args_num
  | Error.ConvErr m ->
     begin
       Printf.printf "%s:\nError:\n" (Nodes.Loc.to_string loc);
       let p k (msg, arg_loc) =
         (* TODO: prinf arg_loc *)
         Printf.printf "%s: %dth arg %s\n" (Nodes.Loc.to_string arg_loc) k msg
       in
       Error.PosMap.iter p m
     end
  | Error.NoMatch (errs, loc) ->
     Printf.printf "%s:\nError: nomatch\n" (Nodes.Loc.to_string loc);
     List.iter (fun err -> print_error ~loc:loc err) errs

  | Error.Msg msg ->
     Printf.printf "\n------------------\nError:\n %s\n\n-------------------\n" msg


(*
* this function will raise exceptions. NError or Fatal_error.
*)
let rec construct_env node parent_env ctx opt_chain_attr =
  let void_t = get_void_aux ctx in
  match node with
  | TAst.Module (inner, pkg_names, mod_name, base_dir, Some env) ->
     begin
       construct_env inner env ctx opt_chain_attr
     end

  | TAst.StatementList (nodes) ->
     let (nodes, last_ty) =
       let ce n =
         try construct_env n parent_env ctx opt_chain_attr
         with
         | NError err ->
            begin
              Printf.printf "\n===============================\n";
              print_error err;
              store_error_message "" ctx;
              Printf.printf "\n===============================\n";
              (TAst.Error, void_t)
            end
       in
       let rec p nodes = match nodes with
         (* if there are no statements, it is void_type *)
         | [] -> [], void_t
         (* if there is only one statement, evaluate it and type is of that *)
         | [x] -> let n, s = ce x in [n], s
         | x :: xs ->
            let n, _ = ce x in
            let ns, last_ty = p xs in
            (n :: ns), last_ty
       in
       p nodes
     in

     (TAst.StatementList nodes, last_ty)

  | TAst.ExprStmt (TAst.PrevPassNode e) ->
     let (node, aux) = analyze_expr ~making_placeholder:false
                                    e parent_env ctx opt_chain_attr in
     (TAst.ExprStmt node, aux)

  | TAst.ReturnStmt (opt_e) ->
     begin
       let (opt_expr, expr_aux) = match opt_e with
         | Some (TAst.PrevPassNode e) ->
            begin
              let (expr, aux) =
                analyze_expr ~making_placeholder:false
                             e parent_env ctx opt_chain_attr
              in
              (Some expr, aux)
          end

         | None ->
            begin
              let ty = get_builtin_void_type default_ty_attr ctx in
              let val_cat = Value_category.VCatPrValue in
              let lt = Env.get_scope_lifetime parent_env in
              let ml = Meta_level.Meta in (* TODO: fix *)
              (None, (ty, val_cat, lt, ml, Nodes.Loc.dummy))
            end

         | _ -> failwith "[ICE]"
       in

       let ctx_env = Option.get parent_env.Env.context_env in
       let ctx_env_r = Env.FunctionOp.get_record ctx_env in

       let ret_ty = match ctx_env_r.Env.fn_is_auto_return_type with
         | true ->
            (* return type will be inferenced *)
            begin
              let (expr_ty, _, _, _, _) = expr_aux in

              let cur_ret_ty = ctx_env_r.Env.fn_return_type in
              match Type.type_sort cur_ret_ty with
              | Type_info.Undef ->
                 begin
                   (* TODO: check type attrbutes *)
                   let nexpr_ty = match make_type_default_form expr_ty ctx with
                     | Some t -> t
                     | None -> failwith "[ERR] cannot convert"
                   in
                   ctx_env_r.Env.fn_return_type <- nexpr_ty;
                   nexpr_ty
                 end

              | Type_info.UniqueTy _ ->
                 begin
                   if Type.has_same_class cur_ret_ty expr_ty then
                     expr_ty
                   else
                     failwith "[ERR]: return type must be same"
                 end

              | _ -> failwith "[ICE]"
          end

         (* return type is already defined *)
         | false -> ctx_env_r.Env.fn_return_type
       in

       (**)
       parent_env.Env.closed <- true;

       (**)
       let make_ret_expr expr =
         let (node, _) =
           adjust_expr_for_type ~exit_scope:true
                                ret_ty expr expr_aux parent_env ctx opt_chain_attr
         in
         node
       in
       let opt_ret_expr = Option.map make_ret_expr opt_expr in

       let node = TAst.ReturnStmt (opt_ret_expr) in
       (node, void_t)
     end

  | TAst.FunctionDefStmt (
        name, params_node, opt_ret_type, opt_cond, body, opt_attr, Some env
      ) ->
     if Env.is_checked env then Env.get_rel_ast env, void_t
     else begin
       (* TODO: check duplicate *)
       let name_s = Nodes.string_of_id_string name in
       Printf.printf "function %s - unchecked\n" name_s;

       (* check parameters *)
       let (params, param_types, param_venvs) =
         prepare_params env [] params_node ctx opt_attr in

       (* check body and check return type *)
       let (ret_type, is_auto) =
         determine_function_return_type opt_ret_type env ctx opt_attr
       in
       check_function_env env param_types Meta_level.Meta ret_type is_auto;

       (* analyze body *)
       let nbody = analyze_inner body env ctx opt_attr in

       let fr = Env.FunctionOp.get_record env in
       post_check_function_return_type fr ctx;

       Printf.printf "function %s - complete\n" name_s;
       let node = TAst.GenericFuncDef (Some nbody, Some env) in

       (* update record *)
       let detail_r = {
         Env.fn_n_param_envs = param_venvs;
       } in

       complete_function_env env node name
                             (Env.FnRecordNormal (Env.FnDefProvidedByUser,
                                                  Env.FnKindFree,
                                                  detail_r))
                             ctx;
       node, void_t
     end

  | TAst.MemberFunctionDefStmt (
        name, params_node, opt_ret_type, body, opt_attr, Some env
      ) ->
     if Env.is_checked env then (Env.get_rel_ast env, void_t)
     else begin
       (* TODO: check duplicate *)
       let name_s = Nodes.string_of_id_string name in
       Printf.printf "member function %s - unchecked\n" name_s;

       (* class env *)
       let parent_env = Option.get env.Env.parent_env in
       let ctx_env = Option.get parent_env.Env.context_env in

       match name with
       | Nodes.Pure s when s = ctor_name ->
          begin
            (* this function is constructor. TODO: check default/copy/move/other
             * currently, treat as normal ctor *)
            let ty = make_class_type ctx_env Type_attr.Val Type_attr.Const ctx in

            (* check parameters *)
            let (params, param_types, param_venvs) =
              prepare_params env [] params_node ctx opt_attr
            in
            let _ = match opt_ret_type with
              | Some _ -> failwith "[ERR] constructor cannot have return type"
              | None -> ()
            in

            (* interface of normal constructor: params -> TYPE *)
            check_function_env env param_types Meta_level.Meta ty false;

            (* prepare "this" special var *)(* TODO: consider member qual *)
            let this_ty = make_class_type ctx_env
                                          Type_attr.Ref Type_attr.Mutable
                                          ctx in
            let (name, this_venv) = make_parameter_venv env "this" this_ty ctx in
            Env.add_inner_env env name this_venv;

            (* analyze body *)
            let nbody = analyze_inner body env ctx opt_attr in

            Printf.printf "function %s - complete\n" name_s;
            let node = TAst.GenericFuncDef (Some nbody, Some env) in

            let detail_r = {
              Env.fn_n_param_envs = param_venvs;
            } in
            let detail =
              Env.FnRecordNormal (Env.FnDefProvidedByUser,
                                  Env.FnKindConstructor (Some this_venv),
                                  detail_r)
            in
            complete_function_env env node ctor_id_name detail ctx;

            (node, void_t)
          end

       | _ ->
          begin
            (* prepare "this" *)(* TODO: consider member qual *)
            let this_param =
              let attr = {
                Type_attr.ta_ref_val = Type_attr.Ref;
                Type_attr.ta_mut = Type_attr.Mutable;
              } in
              let this_ty = make_class_type ctx_env
                                            Type_attr.Ref Type_attr.Mutable
                                            ctx in
              ((attr, Some "this", (None, None)), this_ty)
            in

            (* check parameters *)
            let (params, param_types, param_venvs) =
              prepare_params env [this_param] params_node ctx opt_attr
            in

            (* check body and check return type *)
            let (ret_type, is_auto) =
              determine_function_return_type opt_ret_type env ctx opt_attr
            in
            check_function_env env param_types Meta_level.Meta ret_type is_auto;

            (* analyze body *)
            let nbody = analyze_inner body env ctx opt_attr in

            let fr = Env.FunctionOp.get_record env in
            post_check_function_return_type fr ctx;

            Printf.printf "function %s - complete\n" name_s;
            let node = TAst.GenericFuncDef (Some nbody, Some env) in

            (* update record *)
            let detail_r = {
              Env.fn_n_param_envs = param_venvs;
            } in

            complete_function_env env node name
                                  (Env.FnRecordNormal (Env.FnDefProvidedByUser,
                                                       Env.FnKindMember,
                                                       detail_r))
                                  ctx;
            node, void_t
          end
       end

  | TAst.ExternFunctionDefStmt (
        name, params_node, ml, ret_type, extern_fname, opt_attr, Some env
      ) ->
     if Env.is_checked env then Env.get_rel_ast env, void_t
     else begin
       (* TODO: check duplicate *)
       let name_s = Nodes.string_of_id_string name in
       Printf.printf "extern function %s - unchecked\n" name_s;

       (* check parameters *)
       let (params, param_types, _) =
         prepare_params env [] params_node ctx opt_attr in

       (* check body and check return type *)
       let (ret_type, is_auto) =
         determine_function_return_type (Some ret_type) env ctx opt_attr
       in
       assert(is_auto = false);
       check_function_env env param_types ml ret_type is_auto;

       (* TODO: fix *)
       let is_builtin = match opt_attr with
           Some tbl -> Hashtbl.mem tbl "builtin"
         | None -> false
       in

       (* body *)
       Printf.printf "extern function %s - complete (builtin=%b)\n" name_s is_builtin;
       let node = TAst.GenericFuncDef (None, Some env) in

       (* update record *)
       let record = if is_builtin then
                      Env.FnRecordBuiltin (Env.FnDefProvidedByUser,
                                           Env.FnKindFree,
                                           extern_fname)
                    else
                      Env.FnRecordExternal (Env.FnDefProvidedByUser,
                                            Env.FnKindFree,
                                            extern_fname)
       in
       complete_function_env env node name record ctx;

       node, void_t
     end

  | TAst.ClassDefStmt (
        name, body, opt_attr, Some env) ->
     if Env.is_checked env then Env.get_rel_ast env, void_t
     else begin
       (* TODO: check duplicate *)
       let name_s = Nodes.string_of_id_string name in
       Printf.printf "class %s - unchecked\n" name_s;

       (* resolve member variables first *)
       let cenv_r = Env.ClassOp.get_record env in
       let _ =
         let f venv =
           let node = Env.get_rel_ast venv in
           ignore (construct_env node env ctx opt_attr)
         in
         List.iter f cenv_r.Env.cls_member_vars
       in
       (* check! *)
       check_class_env env ctx;

       (**)
       let member_layouts =
         let f venv =
           let venv_r = Env.VariableOp.get_record venv in
           let var_ty = venv_r.Env.var_type in
           let var_cenv = Type.as_unique var_ty in
           let var_cenv_r = Env.ClassOp.get_record var_cenv in
           let vsize = match var_cenv_r.Env.cls_size with
             | Some v -> v
             | None -> failwith "[ERR] member size is not determined yet"
           in
           let valign = match var_cenv_r.Env.cls_align with
             | Some v -> v
             | None -> failwith "[ERR] member align is not determined yet"
           in
           (vsize, valign)
         in
         List.map f cenv_r.Env.cls_member_vars
       in

       let (class_size, class_align) =
         let f (csize, calign) (vsize, valign) =
           let open Uint32 in
           (* TODO: implement correctly *)
           (csize + vsize, calign)
         in
         List.fold_left f (Uint32.zero, Uint32.zero) member_layouts
       in

       (**)
       let module SpecialMemberStates = struct
         type state_t = {
           is_callable     : bool;
           is_trivial      : bool;
         }

         let init_state =
           {
             is_callable = true;
             is_trivial = true;
           }

         let scan_special_func_state state t =
           match state with
           | Env.FnDefDefaulted b ->
              {
                is_callable = true && t.is_callable;
                is_trivial = b && t.is_trivial;
              }
           | Env.FnDefProvidedByUser ->
              {
                is_callable = true && t.is_callable;
                is_trivial = false && t.is_trivial;
              }
           | Env.FnDefDeleted ->
              {
                is_callable = false && t.is_callable;
                is_trivial = false && t.is_trivial;
              }

         let state_to_trait state =
           match state with
           | { is_callable = true; is_trivial = b } -> Env.FnDefDefaulted b
           | { is_callable = false; is_trivial = false } -> Env.FnDefDeleted
           | _ -> failwith "[ICE]: state_to_trait"

         let string_of_state s =
           Printf.sprintf "is_callable = %b; is_trivial = %b" s.is_callable s.is_trivial

         type t =
             {
               default_ctor_state       : state_t;
               copy_ctor_state          : state_t;
               has_user_defined_ctor    : bool;
             }

         let init_states =
           {
             default_ctor_state = init_state;
             copy_ctor_state = init_state;
             has_user_defined_ctor = false;
           }

         let scan_special_func_states traits states =
           {
             states with
             default_ctor_state =
               scan_special_func_state traits.Env.cls_traits_default_ctor_state
                                       states.default_ctor_state;
             copy_ctor_state =
               scan_special_func_state traits.Env.cls_traits_copy_ctor_state
                                       states.copy_ctor_state;
           }

         let update_traits_by_states traits states =
           {
             traits with
             Env.cls_traits_default_ctor_state =
               state_to_trait states.default_ctor_state;
             Env.cls_traits_copy_ctor_state =
               state_to_trait states.copy_ctor_state;
           }

         let string_of_states ss =
           let s = Printf.sprintf "default_ctor_state = %s\n" (string_of_state ss.default_ctor_state) in
           let s = s ^ Printf.sprintf "copy_ctor_state = %s\n" (string_of_state ss.copy_ctor_state) in
           s

       end in
       let member_vars_sf_states =
         let f s venv =
           let venv_r = Env.VariableOp.get_record venv in
           let var_ty = venv_r.Env.var_type in
           let var_cenv = Type.as_unique var_ty in
           let var_cenv_r = Env.ClassOp.get_record var_cenv in
           let var_cls_traits = var_cenv_r.Env.cls_traits in

           SpecialMemberStates.scan_special_func_states var_cls_traits s
         in
         List.fold_left f SpecialMemberStates.init_states cenv_r.Env.cls_member_vars
       in
       Printf.printf "= CLASS: %s\n%s\n" name_s (SpecialMemberStates.string_of_states member_vars_sf_states);


       (* body *)
       let (nbody, _) = construct_env body env ctx opt_attr in

       (* TODO: check class characteristics *)
       let _ =
         let open SpecialMemberStates in
         let f c_states env =
           match Env.get_env_record env with
           (* if there are template constructors at least 1, class has user-defined constructor *)
           | Env.Template r ->
              begin
                match r.Env.tl_name with
                (* template has ctor name *)
                | Nodes.Pure n when n = ctor_name ->
                   { c_states with
                     has_user_defined_ctor = true
                   }
                | _ ->
                   c_states
              end

           (* normal functions *)
           | Env.Function (_, r) ->
              begin
                match r.Env.fn_name with
                | Nodes.Pure n when n = ctor_name ->
                   begin
                     let required_params = exclude_optional_params r.Env.fn_param_kinds in
                     match List.length required_params with
                     (* default constructor *)
                     | 0 ->
                        begin
                          {
                            c_states with
                            default_ctor_state =
                              scan_special_func_state
                                (Env.FunctionOp.get_definition_status env)
                                c_states.default_ctor_state;
                          }
                        end
                     | 1 ->
                        begin
                          (* TODO: implement *)
                          c_states
                        end
                     | _ ->
                        c_states
                   end
                | _ ->
                   c_states
              end

           | _ -> Env.print env; failwith "[ICE]"
         in
         let class_states =
           List.fold_left f member_vars_sf_states cenv_r.Env.cls_member_funcs
         in
         cenv_r.Env.cls_traits <- update_traits_by_states cenv_r.Env.cls_traits class_states
       in

       (**)
       let define_special_members () =
         let open Env in
         let ty = make_class_type env Type_attr.Val Type_attr.Const ctx in

         let define_trivial_defaulted_default_ctor () =
           let fenv = declare_incomple_ctor env in
           let node = TAst.GenericFuncDef (None, Some fenv) in
           (* TODO: check class members are all trivial *)
           let detail =
             Env.FnRecordImplicit (Env.FnDefDefaulted true,
                                  Env.FnKindDefaultConstructor None)
           in
           check_function_env fenv [] Meta_level.Meta ty false;
           complete_function_env fenv node ctor_id_name detail ctx;
         in
         (* implicit default constructor is defined as defaulted and trivial,
          * if there are no user defined constructor.
          * However, some conditions make it as deleted *)
         let _ = match cenv_r.cls_traits.cls_traits_default_ctor_state with
           | Env.FnDefDefaulted true -> (* TODO: check *)
              define_trivial_defaulted_default_ctor ()
           | _ -> ()
         in

         let define_trvial_defaulted_copy_ctor () =
           (* copy ctor *)
           let rhs_ty = make_class_type env Type_attr.Ref Type_attr.Const ctx in
           let fenv = declare_incomple_ctor env in
           let node = TAst.GenericFuncDef (None, Some fenv) in
           let detail =
             Env.FnRecordImplicit (Env.FnDefDefaulted true,
                                  Env.FnKindCopyConstructor None)
           in
           check_function_env fenv [Env.FnParamKindType rhs_ty] Meta_level.Meta ty false;
           complete_function_env fenv node ctor_id_name detail ctx;
         in
         define_trvial_defaulted_copy_ctor ()
       in
       define_special_members ();

       Printf.printf "class %s - complete\n" name_s;
       let node = TAst.ClassDefStmt (
                      name,
                      nbody,
                      opt_attr,
                      Some env
                    ) in

       (* update record *)
       let detail_r = Env.ClsRecordNormal in

       (* TODO: improve *)
       let is_primitive = match opt_attr with
         | Some tbl ->
            begin
              let v = Hashtbl.find_option tbl "primitive" in
              match v with
                Some vv ->
                begin
                  match vv with
                  | None -> true
                  | _ -> failwith "[ERR] primitive attrbute is not able to have some value"
                end
              | None -> false
            end
         | None -> false
       in
       complete_class_env env node detail_r (Some (class_size, class_align));
       cenv_r.Env.cls_traits <- {
         cenv_r.Env.cls_traits with
         Env.cls_traits_is_primitive = is_primitive;
       };

       (node, void_t)
     end

  | TAst.ExternClassDefStmt (
        name, extern_cname, opt_attr, Some env) ->
     if Env.is_checked env then Env.get_rel_ast env, void_t
     else begin
       (* TODO: check duplicate *)
       let name_s = Nodes.string_of_id_string name in
       Printf.printf "extern class %s - unchecked\n" name_s;
       check_class_env env ctx;

       let cenv_r = Env.ClassOp.get_record env in

       (* currently, do not remake a node like other nodes *)
       Printf.printf "extern class %s - complete\n" name_s;

       let is_builtin = find_attr_bool_val ~boot:true
                                           opt_attr "builtin" parent_env ctx
       in
       if not is_builtin then
         failwith "[ERR]";

       let is_novalue = find_attr_bool_val ~boot:true
                                           opt_attr "novalue" parent_env ctx
       in
       Printf.printf "is_novalue : %b \n" is_novalue;

       let is_primitive = find_attr_bool_val ~boot:true
                                             opt_attr "primitive" parent_env ctx
       in
       let is_array_type = find_attr_bool_val ~boot:true
                                              opt_attr "array_type" parent_env ctx
       in

       let define_special_members_and_calc_layout () =
         if not is_array_type then
           begin
             let open Env in
             define_trivial_default_ctor_for_builtin env extern_cname ctx;
             cenv_r.cls_traits <- {
               cenv_r.cls_traits with
               cls_traits_default_ctor_state = Env.FnDefDefaulted true;
             };
             define_trivial_copy_ctor_for_builtin env extern_cname ctx;
             cenv_r.cls_traits <- {
               cenv_r.cls_traits with
               cls_traits_copy_ctor_state = Env.FnDefDefaulted true;
             };
             define_trivial_copy_assign_for_builtin env extern_cname ctx;

             let opt_csize = find_attr_uint32_val ~boot:true
                                                  opt_attr "size" parent_env ctx
             in
             let csize = match opt_csize with
               | Some v -> v
               | None -> failwith "[ERR]"
             in
             let opt_calign = find_attr_uint32_val ~boot:true
                                                   opt_attr "align" parent_env ctx
             in
             let calign = match opt_calign with
               | Some v -> v
               | None -> failwith "[ERR]"
             in

             (csize, calign)
           end
         else
           begin
             (* for Array type *)
             let targ_cval = List.at cenv_r.Env.cls_template_vals 0 in
             let tval_ty = match targ_cval with
               | Ctfe_value.Type ty -> ty
               | _ -> failwith "[ICE]"
             in
             let tval_ty_cenv = Type.as_unique tval_ty in
             let tval_ty_cenv_r = Env.ClassOp.get_record tval_ty_cenv in
             let tval_ty_traits = tval_ty_cenv_r.Env.cls_traits in

             let targ_nval = List.at cenv_r.Env.cls_template_vals 1 in
             let tval_num = match targ_nval with
               | Ctfe_value.Uint32 n -> n
               | _ -> failwith "[ICE] array length"
             in

             (* default constructor *)
             let _ = match tval_ty_traits.Env.cls_traits_default_ctor_state with
               | Env.FnDefDefaulted true ->
                  begin
                    define_trivial_default_ctor_for_builtin env extern_cname ctx;

                    let open Env in
                    cenv_r.cls_traits <- {
                      cenv_r.cls_traits with
                      cls_traits_default_ctor_state = Env.FnDefDefaulted true;
                    };
                  end
               | _ ->
                  failwith "[ERR] not implemented yet"
             in

             (* copy constructor *)
             let _ = match tval_ty_traits.Env.cls_traits_copy_ctor_state with
               | Env.FnDefDefaulted true ->
                  begin
                    define_trivial_copy_ctor_for_builtin env extern_cname ctx;

                    let open Env in
                    cenv_r.cls_traits <- {
                      cenv_r.cls_traits with
                      cls_traits_copy_ctor_state = Env.FnDefDefaulted true;
                    };
                  end
               | _ ->
                  failwith "[ERR] not implemented yet"
             in

             (Uint32.(Type.element_size_of tval_ty * tval_num), Type.align_of tval_ty)
           end
       in
       let opt_layout =
         if is_novalue then
           None
         else
           Some (define_special_members_and_calc_layout ())
       in

       (* update record *)
       let detail_r = Env.ClsRecordExtern {
                          Env.cls_e_name = extern_cname;
                        } in

       complete_class_env env node detail_r opt_layout;
       cenv_r.Env.cls_traits <- {
         cenv_r.Env.cls_traits with
         Env.cls_traits_is_primitive = is_primitive;
       };

       (node, void_t)
     end

  (* *)
  | TAst.MemberVariableDefStmt (v, Some env) ->
     if Env.is_checked env then Env.get_rel_ast env, void_t
     else begin
       (* TODO: implement *)
       let (var_attr, var_name, init_term) = match extract_prev_pass_node v with
         | Ast.VarInit vi -> vi
         | _ -> failwith "unexpected node"
       in
       let (opt_type, opt_init_value) = init_term in
       let var_ty = match opt_type with
         | Some var_type_node ->
            begin
              let (expr_ty, type_expr) =
                resolve_type_with_node var_type_node
                                       parent_env ctx opt_chain_attr
              in
              let var_ty =
                Type.Generator.update_attr_r ctx.sc_tsets.ts_type_gen expr_ty
                                             var_attr
              in
              var_ty
            end
         | None -> failwith "[ERR] type spec is required in class decl"
       in
       check_env env Meta_level.Runtime;    (* TODO: fix *)

       let r = Env.VariableOp.get_record env in
       r.Env.var_type <- var_ty;

       complete_env env node;
       node, void_t
     end

  (* scoped declare *)
  | TAst.VariableDefStmt (var_metalevel, v, None) ->
     begin
       let (var_attr, var_name, init_term) =
         match extract_prev_pass_node v with
         | Ast.VarInit vi -> vi
         | _ -> failwith "unexpected node"
       in
       check_id_is_defined_uniquely parent_env var_name;

       let venv_r = Env.VariableOp.empty_record var_name in
       let venv = Env.create_context_env parent_env (
                                           Env.Variable (venv_r)
                                         )
       in

       let (opt_type, opt_init_value) = init_term in
       let opt_init_value_res =
         opt_init_value
         |> Option.map (fun n -> analyze_expr n parent_env ctx opt_chain_attr)
       in

       (* type check for the variable *)
       let (type_node, value_node, var_ty, var_metalevel) = match opt_type with
         (* variable type is specified *)
         | Some var_type_node ->
            begin
              let (expr_ty, type_expr) =
                resolve_type_with_node var_type_node
                                       parent_env ctx opt_chain_attr
              in
              let var_ty =
                Type.Generator.update_attr_r ctx.sc_tsets.ts_type_gen expr_ty
                                             var_attr
              in
              let (res_expr_node, res_ml) = match opt_init_value_res with
                | Some (expr_node, expr_ty_cat) ->
                   begin
                     adjust_expr_for_type var_ty expr_node expr_ty_cat
                                          parent_env ctx opt_chain_attr
                   end
                | None ->
                   begin
                     (* TODO: implement call default constructor *)
                     failwith "not implemented //"
                   end
              in
              (* TODO: fix var_metalevel *)
              (Some type_expr, res_expr_node, var_ty, var_metalevel)
            end

         (* var_type is infered from initial_value *)
         | None ->
            begin
              let (expr_node, expr_aux) = match opt_init_value_res with
                | Some v -> v
                (* TODO: call default constructor *)
                | None -> failwith "[ERROR] initial value is required";
              in
              let (expr_ty, _, _, expr_ml, _) = expr_aux in
              if Type.has_same_class expr_ty (get_builtin_void_incomplete_type ctx) then
                error_msg "rhs is void type";

              let var_ty =
                Type.Generator.update_attr_r ctx.sc_tsets.ts_type_gen expr_ty
                                             var_attr
              in
              (* TODO: if var has meta level, qual must be immutable/const val *)
              let (conved_node, (conved_type, _, _, conved_ml, _)) =
                adjust_expr_for_type var_ty expr_node expr_aux
                                     parent_env ctx opt_chain_attr
              in

              (* var_metalevel *)
              let var_metalevel =
                let type_attr = Type.type_attr conved_type in
                match type_attr.Type_attr.ta_ref_val with
                | Type_attr.Ref ->
                   Meta_level.bottom var_metalevel Meta_level.Runtime
                | Type_attr.Val ->
                   begin
                     match type_attr.Type_attr.ta_mut with
                     | Type_attr.Mutable ->
                        Meta_level.bottom var_metalevel Meta_level.Runtime
                     | Type_attr.Const
                     | Type_attr.Immutable ->
                        var_metalevel
                     | _ -> failwith "[ICE]"
                   end
                | _ -> failwith "[ICE]"
              in

              if not (Meta_level.is_convertiable_to conved_ml var_metalevel) then
                failwith "[ERR] couldn't convert meta level";

              let mled_node = match var_metalevel with
                | Meta_level.OnlyMeta
                | Meta_level.Meta ->
                   begin
                     (* TODO: check whether the variable is ctfeable.
                      * Ex. it must have trivial destructor *)
                     let ctfe_v =
                       eval_texpr_as_ctfe conved_node var_ty var_metalevel
                                          parent_env ctx opt_chain_attr
                     in
                     Ctfe_engine.register_metavar ctx.sc_ctfe_engine ctfe_v venv;
                     tnode_of_ctfe_val ctfe_v ctx
                   end
                | _ -> conved_node
              in

              (None, mled_node, var_ty, var_metalevel)
            end
       in
       check_env venv var_metalevel;

       Printf.printf "||||||| %s => %s | %s\n" var_name (Type.to_string var_ty) (Meta_level.to_string var_metalevel);

       (* register the variable to the environments *)
       Env.add_inner_env parent_env var_name venv;

       let node = TAst.VariableDefStmt (
                      var_metalevel,
                      TAst.VarInit (var_attr, var_name, (type_node, Some value_node)),
                      Some venv
                    ) in

       let detail_r = Env.VarRecordNormal () in
       let r = Env.VariableOp.get_record venv in
       r.Env.var_type <- var_ty;
       r.Env.var_detail <- detail_r;

       complete_env venv node;
       (node, void_t)
     end

  | TAst.EmptyStmt -> node, get_void_aux ctx

  | _ ->
     begin
       TAst.print node;
       failwith "construct_env: unsupported node or nodes have no valid env"
     end

and analyze_expr ?(making_placeholder=false)
                 ?(enable_ufcs=false)
                 node parent_env ctx attr : ('node * TAst.term_aux_t) =
  let void_t = get_void_aux ctx in
  match node with
  | Ast.BinaryOpExpr (lhs, op, rhs, loc) ->
     begin
       let args = [lhs; rhs] in
       let res = analyze_operator op args loc parent_env ctx attr in
       match res with
       | Some v -> v
       | None ->
          (* TODO: error message *)
          failwith "binary operator is not found"
     end

  | Ast.UnaryOpExpr (op, expr, loc) ->
     begin
       let args = [expr] in
       let res = analyze_operator op args loc parent_env ctx attr in
       match res with
       | Some v -> v
       | None ->
          (* TODO: error message *)
          failwith "unary operator is not found"
     end

  | Ast.SubscriptingExpr (receiver, opt_arg, loc) ->
     begin
       let (op, args) = match opt_arg with
         | Some arg -> (Nodes.BinaryOp "[]", [receiver; arg])
         | None -> (Nodes.UnaryPostOp "[]", [receiver])
       in
       let loc = None in
       let res = analyze_operator (Ast.Id (op, None)) args loc parent_env ctx attr in
       match res with
       | Some v -> v
       | None ->
          (* TODO: error message *)
          failwith "subscripting operator is not found"
     end

  | Ast.CallExpr (receiver, args, loc) ->
     begin
       let (recv_node, (recv_type_info, recv_val_cat, recv_lt, recv_ml, _)) =
         analyze_expr ~making_placeholder:making_placeholder
                      ~enable_ufcs:true
                      receiver parent_env ctx attr
       in

       let eargs =
         evaluate_invocation_args args parent_env ctx attr
       in
       let (arg_exprs, arg_auxs) = eargs |> List.split in
       (*List.iter check_is_args_valid args_types;*)
(*
       let (_, _, _, arg_metalevels, _) = split_aux arg_auxs in
       let args_bottom_ml = Meta_level.calc_bottom arg_metalevels in
 *)
       let {
         Type_info.ti_sort = ty_sort;
         Type_info.ti_template_args = template_args;
       } = recv_type_info in
       let ((node, aux), f_ml) = match ty_sort with
         (* normal function call *)
         | Type_info.FunctionSetTy menv ->
            begin
              (* consider nested expr *)
              let (arg_exprs, arg_auxs) = match recv_node with
                | TAst.NestedExpr (lhs_node, lhs_aux, _, _) ->
                   let args = lhs_node :: arg_exprs in
                   let arg_auxs = lhs_aux :: arg_auxs in
                   (args, arg_auxs)
                | _ -> (arg_exprs, arg_auxs)
              in
              let args = List.combine arg_exprs arg_auxs in
              let call_trg_finfo =
                solve_function_overload args template_args
                                        menv parent_env loc ctx attr
              in
              let call_inst =
                make_call_instruction call_trg_finfo loc None parent_env ctx
              in

              let (f_env, conv_filters, args) = call_trg_finfo in
              let f_ml = f_env.Env.meta_level in

              (call_inst, f_ml)
            end

         (* constructor / operator call *)
         | Type_info.UniqueTy type_cenv ->
            begin
              (* type_cenv will be Type *)
              let recv_ty =
                resolve_texpr_type recv_node recv_type_info recv_ml
                                   parent_env ctx attr
              in
              let recv_cenv = Type.as_unique recv_ty in
              Env.print recv_cenv;
              let f_sto = suitable_storage recv_ty ctx in

              (* call constructor *)
              (* TODO: take into account op call *)
              let res = solve_basic_identifier ~do_rec_search:false
                                               ctor_id_name recv_cenv ctx attr in
              let (_, ctor_env, _) = match res with
                | Some v -> v
                | None -> failwith "constructor not found"
              in
              let call_trg_finfo =
                solve_function_overload eargs []
                                        ctor_env parent_env loc ctx attr
              in
              let call_inst =
                make_call_instruction call_trg_finfo loc (Some f_sto) parent_env ctx
              in

              let (f_env, conv_filters, eargs) = call_trg_finfo in
              let f_ml = f_env.Env.meta_level in

              (call_inst, f_ml)
            end

         | _ -> failwith "not implemented//" (* TODO: call ctor OR operator() *)
       in
       let mled_node = match f_ml with
         | Meta_level.OnlyMeta ->
           begin
             (* TODO: check whether the variable is ctfeable.
              * Ex. it must have trivial destructor *)
             let (ty, _, _, ret_ml, _) = aux in
             let ctfe_v =
               eval_texpr_as_ctfe node ty ret_ml
                                  parent_env ctx None
             in
             tnode_of_ctfe_val ctfe_v ctx
           end
         | _ -> node
       in
       (mled_node, aux)
     end

  | Ast.ElementSelectionExpr (lhs, rhs, _) ->
     begin
       let loc = None in
       let (lhs_node, lhs_aux) =
         analyze_expr ~making_placeholder:making_placeholder
                      lhs parent_env ctx attr
       in

       let (lhs_ty, _, _, _, _) = lhs_aux in
       match Type.type_sort lhs_ty with
       | Type_info.UniqueTy lhs_type_cenv ->
          begin
            let opt_rhs_ty_node =
              select_member_element ~universal_search:enable_ufcs
                                    lhs_aux rhs parent_env ctx attr
            in
            match opt_rhs_ty_node with
            | Some (rhs_ty, rhs_env, rhs_ml) ->
               let node = TAst.NestedExpr (lhs_node, lhs_aux, rhs_ty, Some rhs_env) in
               let prop_ty = propagate_type_attrs rhs_ty lhs_ty ctx in
               let lt = Env.get_scope_lifetime parent_env in (* TODO: fix *)
               let ml = Meta_level.Runtime in (* TODO: fix *)
               (node, (prop_ty, VCatLValue, lt, ml, loc))

            | None ->
               failwith "[ERR] member is not found"
          end

       | _ -> failwith "[ICE]"
     end

  (*| Ast.SubscriptingExpr (lhs, )       *)
  | Ast.NewExpr (expr) ->
     begin
       let (expr_node, expr_aux) =
         analyze_expr ~making_placeholder:making_placeholder
                      expr parent_env ctx attr
       in
       ignore expr_node; failwith "new expr"
     end

  | Ast.StatementTraitsExpr (keyword, block) ->
     begin
       let loc = None in
       match keyword with
       | "semantics" ->
          begin
            let default_ty_attr = {
              Type_attr.ta_ref_val = Type_attr.Val;
              Type_attr.ta_mut = Type_attr.Const;
            } in
            let ty = get_builtin_bool_type default_ty_attr ctx in
            let ty =
              Type.Generator.update_attr ctx.sc_tsets.ts_type_gen ty
                                         Type_attr.Val
                                         Type_attr.Immutable
            in
            assert_valid_type ty;

            let test () =
                (* temporary environment for check whether semantics is valid.
                 * DO NOT append this env to the parent_env.
                 *)
              let temp_env =
                Env.create_scoped_env parent_env (Env.Scope (Env.empty_lookup_table ()))
              in
              ignore @@
                analyze ~opt_attr:attr block temp_env ctx;
              true
            in
            let could_compile = try test() with
                                | _ -> false
            in
            let node = TAst.BoolLit (could_compile, ty) in
            (node, (ty, VCatPrValue, Env.get_scope_lifetime parent_env, Meta_level.Meta, loc))
          end
       | _ -> failwith @@ "__statement_traits : not implemented / " ^ keyword
     end

  | (Ast.Id (name, loc) as id_node)
  | (Ast.InstantiatedId (name, _, loc) as id_node) ->
     begin
       let res =
         solve_identifier ~making_placeholder:making_placeholder
                          id_node parent_env ctx attr
       in
       let (ty, trg_env, ml) = match res with
         | Some v -> v
         | None ->
            error_msg ("id not found : " ^ (Nodes.string_of_id_string name))
       in
       let lt = Env.get_scope_lifetime parent_env in (* TODO: fix *)

       Printf.printf "= %s - %s\n" (Nodes.string_of_id_string name) (Meta_level.to_string ml);

       (* both of id and instantiated_id will be id node *)
       let node = TAst.GenericId (name, Some trg_env) in
       (node, (ty, VCatLValue, lt, ml, loc))
     end

  | Ast.IntLit (i, bits, signed, loc) ->
     begin
       let attr = {
         Type_attr.ta_ref_val = Type_attr.Val;
         Type_attr.ta_mut = Type_attr.Immutable;
       } in
       let ty = get_builtin_int_type ~bits:bits ~signed:signed attr ctx in
       assert_valid_type ty;
       let node = TAst.IntLit (i, bits, signed, ty) in

       (node, (ty, VCatPrValue, Env.get_scope_lifetime parent_env, Meta_level.Meta, loc))
     end

  | Ast.BoolLit (b, loc) ->
     begin
       let attr = {
         Type_attr.ta_ref_val = Type_attr.Val;
         Type_attr.ta_mut = Type_attr.Immutable;
       } in
       let ty = get_builtin_bool_type attr ctx in
       assert_valid_type ty;
       let node = TAst.BoolLit (b, ty) in

       (node, (ty, VCatPrValue, Env.get_scope_lifetime parent_env, Meta_level.Meta, loc))
     end

  | Ast.StringLit (str, loc) ->
     begin
       let attr = {
         Type_attr.ta_ref_val = Type_attr.Val;
         Type_attr.ta_mut = Type_attr.Immutable;
       } in
       let elem_ty = get_builtin_int_type ~bits:8 ~signed:false attr ctx in
       let ptr_ty = get_builtin_raw_ptr_type elem_ty attr ctx in
       assert_valid_type ptr_ty;

       let n_ptr = TAst.StringLit (str, ptr_ty) in
       (n_ptr, (ptr_ty, VCatPrValue, Env.get_scope_lifetime parent_env, Meta_level.Meta, loc))
     end

  | Ast.ArrayLit (elems, _, loc) ->
     begin
       (* TODO: support typings for empty list literal *)
       assert(List.length elems > 0);
       (* evaluate all elementes *)
       let nargs =
         elems |> List.map (fun e -> analyze_expr ~making_placeholder:making_placeholder
                                                  e parent_env ctx attr)
       in

       let array_common_elem =
         let common_elem_arg arga argb =
           let (dn, (s_ty, d1, d2, s_ml, d4)) = arga in
           let (_,  (_,     _, _,  t_ml, _ )) = argb in

           match convert_type s_ty argb parent_env ctx attr with
           | (FuncMatchLevel.ExactMatch, Some (trg_ty, f)) ->
              begin
                (dn, (trg_ty, d1, d2, Meta_level.bottom s_ml t_ml, d4))
              end
           | _ -> failwith "[ERR]"
         in
         List.reduce common_elem_arg nargs
       in
       let (_, (elem_ty, _, _, bottom_ml, _)) = array_common_elem in

       (**)
       let trivial_copy_ctors =
         let conv arg =
           let res = convert_type elem_ty arg parent_env ctx attr in
           match res with
           | (FuncMatchLevel.NoMatch, _) -> failwith "[ERR]"
           | (_, None) -> failwith "[ERR]"
           | (_, (Some (trg_ty, copy_ctor) as m_filter)) ->
              let is_trivial = Env.FunctionOp.is_trivial copy_ctor in
              (is_trivial, m_filter, arg)
         in
         List.map conv nargs
       in

       let static_constructable =
         let is_all_trivial_copyable =
           let f (b, _, _) = b in
           List.for_all f trivial_copy_ctors
         in
         (Meta_level.has_meta_spec bottom_ml) && is_all_trivial_copyable
       in

       (* copy/move ctor *)
       let conved_args =
         let conv index (_, m_filter, arg) =
           if static_constructable then
             apply_conv_filter m_filter arg parent_env ctx
           else
             apply_conv_filter ~opt_operation:(Some (SoArrayElement index))
                               m_filter arg parent_env ctx
         in
         List.mapi conv trivial_copy_ctors
       in
       let (n_nodes, n_auxs) = conved_args |> List.split in

       let array_ty =
         get_builtin_array_type elem_ty
                                (List.length elems)
                                default_ty_attr
                                ctx
       in
       assert_valid_type array_ty;
       let n_array = TAst.ArrayLit (n_nodes, static_constructable, array_ty) in
       (* TODO: FIX *)
       (n_array, (array_ty, VCatPrValue, Env.get_scope_lifetime parent_env, Meta_level.Meta, loc))
     end

  | Ast.ScopeExpr (block) ->
     begin
       let scope_env =
         Env.create_scoped_env parent_env
                               (Env.Scope (Env.empty_lookup_table ()))
       in
       let (nblock, aux) = analyze_t block scope_env ctx in
       parent_env.Env.closed <- scope_env.Env.closed;

       let node = TAst.ScopeExpr (nblock) in
       (node, aux)
     end

  | Ast.IfExpr (cond_expr, then_expr, opt_else_expr, loc) ->
     begin
       let scope_env =
         Env.create_scoped_env parent_env
                               (Env.Scope (Env.empty_lookup_table ()))
       in

       let (n_cond_expr, cond_aux) = analyze_expr cond_expr scope_env ctx attr in

       let bool_ty = get_builtin_bool_type default_ty_attr ctx in
       let (conved_cond_node, conved_auxs) =
         adjust_expr_for_type bool_ty n_cond_expr cond_aux
                              scope_env ctx attr
       in

       let analayze_clause node =
         let clause_scope_env =
           Env.create_scoped_env scope_env
                                 (Env.Scope (Env.empty_lookup_table ()))
         in
         analyze_expr node clause_scope_env ctx attr
       in

       let wrapped_type lhs rhs =
         let open Type_info in
         let open Type_attr in
         assert(Type.has_same_class lhs rhs);

         let attr = {
           ta_ref_val = rv_strong lhs.ti_attr.ta_ref_val rhs.ti_attr.ta_ref_val;
           ta_mut = mut_strong lhs.ti_attr.ta_mut rhs.ti_attr.ta_mut;
         } in

         Type.Generator.update_attr_r ctx.sc_tsets.ts_type_gen
                                      lhs (* or rhs *)
                                      attr
       in

       let (nthen_expr, then_aux) = analayze_clause then_expr in
       let (opt_else_expr, else_aux) = match opt_else_expr with
         | Some else_expr ->
            let (e, aux) = analayze_clause else_expr in
            (Some e, aux)
         | None -> (None, void_t)
       in

       let (then_ty, _, _, _, _) = then_aux in
       let (else_ty, _, _, _, _) = else_aux in
       if not (Type.has_same_class then_ty else_ty) then
         failwith "[ERR]";

       let if_ty = wrapped_type then_ty else_ty in
       let if_cat = VCatLValue in   (* TODO: fix *)
       let if_mt = Env.get_scope_lifetime parent_env in (* TODO: fix *)
       let if_ml = Meta_level.Meta in   (* TODO: fix *)
       let if_loc = loc in
       let if_aux = (if_ty, if_cat, if_mt, if_ml, if_loc) in

       let node = TAst.IfExpr (conved_cond_node, nthen_expr, opt_else_expr, if_ty) in
       (node, if_aux)
     end

  | Ast.ForExpr (opt_var_decl, opt_cond, opt_step, body) ->
     begin
       let scope_env =
         Env.create_scoped_env parent_env
                               (Env.Scope (Env.empty_lookup_table ()))
       in

       let nopt_var_decl =
         let f var_decl =
           analyze var_decl scope_env ctx
         in
         Option.map f opt_var_decl
       in

       let nopt_cond =
         let f cond =
           let (nexpr, aux) = analyze_expr cond scope_env ctx attr in
           nexpr
         in
         Option.map f opt_cond
       in

       let nopt_step =
         let f step =
           let (nexpr, aux) = analyze_expr step scope_env ctx attr in
           nexpr
         in
         Option.map f opt_step
       in

       let body_env =
         Env.create_scoped_env scope_env
                               (Env.Scope (Env.empty_lookup_table ()))
       in
       let (nbody, body_aux) = analyze_expr body body_env ctx attr in

       let node = TAst.ForExpr (nopt_var_decl, nopt_cond, nopt_step, nbody) in
       (node, void_t)
     end

  | Ast.TypeRVConv (rv, args, loc) ->
     begin
       if (List.length args <> 1) then
         error_msg "length of args must be 1";
       let earg =
         evaluate_invocation_args args parent_env ctx attr
         |> List.hd
       in
       let (arg_expr, arg_aux) = earg in
       let (ty, _, _, ml, _) = arg_aux in

       if (not (Type.has_same_class ty ctx.sc_tsets.ts_type_type)) then
         error_msg "the argument must be type";
       let v = eval_texpr_as_ctfe arg_expr ty ml parent_env ctx attr in
       let ty_val = match v with
         | Ctfe_value.Type ty -> ty
         | _ -> failwith "[ICE]"
       in
       let {
         Type_info.ti_attr = ty_attr;
       } = ty_val in
       let nty = Type.Generator.update_attr_r ctx.sc_tsets.ts_type_gen
                                              ty_val
                                              { ty_attr with
                                                Type_attr.ta_ref_val = rv;
                                              } in

       let tnode = TAst.CtxNode nty in
       (tnode, arg_aux)
     end

  | Ast.TypeQualConv (qual, args, loc) ->
     begin
       if (List.length args <> 1) then
         error_msg "length of args must be 1";
       let earg =
         evaluate_invocation_args args parent_env ctx attr
         |> List.hd
       in
       let (arg_expr, arg_aux) = earg in
       let (ty, _, _, ml, _) = arg_aux in

       if (not (Type.has_same_class ty ctx.sc_tsets.ts_type_type)) then
         error_msg "the argument must be type";
       let v = eval_texpr_as_ctfe arg_expr ty ml parent_env ctx attr in
       let ty_val = match v with
         | Ctfe_value.Type ty -> ty
         | _ -> failwith "[ICE]"
       in
       let {
         Type_info.ti_attr = ty_attr;
       } = ty_val in
       let nty = Type.Generator.update_attr_r ctx.sc_tsets.ts_type_gen
                                              ty_val
                                              { ty_attr with
                                                Type_attr.ta_mut = qual;
                                              } in

       let tnode = TAst.CtxNode nty in
       (tnode, arg_aux)
     end

  | _ ->
     begin
       Ast.print node;
       failwith "analyze_expr: unsupported node"
     end

and analyze_boot_expr node parent_env ctx attr : 'ty Ctfe_value.t =
  match node with
  | Ast.IntLit (i, bits, signed, loc) ->
     begin
       match bits with
       | 32 -> if signed then
                 Ctfe_value.Int32 (Int32.of_int i)
               else
                 Ctfe_value.Uint32 (Uint32.of_int i)
       | _ -> failwith ""
     end

  | Ast.BoolLit (b, loc) ->
     begin
       Ctfe_value.Bool b
     end

  | Ast.StringLit (str, loc) ->
     begin
       failwith "[ICE]: not supported yet"
     end

  | _ ->
     begin
       Ast.print node;
       failwith "analyze_expr: unsupported node"
     end

and analyze_operator op_id args loc parent_env ctx attr =
  let eargs = evaluate_invocation_args args parent_env ctx attr in
  (*let (arg_exprs, arg_auxs) = List.split eargs in
  (*List.iter check_is_args_valid args_types_cats;*)

  let (_, _, _, arg_metalevels, _) = split_aux arg_auxs in
  let args_bottom_ml = Meta_level.calc_bottom arg_metalevels in*)

  (*List.iter print_type args_types;*)
  let opt_fs_and_args =
    find_suitable_operator ~universal_search:true
                           op_id eargs parent_env ctx attr in

  Option.map (fun f -> make_call_instruction f loc None parent_env ctx) opt_fs_and_args

and make_call_instruction (f_env, conv_filters, eargs) loc opt_sto parent_env ctx =
  let n_eargs =
    map_conversions ~param_passing:true
                    conv_filters eargs parent_env ctx
  in
  let (n_earg_exprs, _) = n_eargs |> List.split in

  let f_er = Env.FunctionOp.get_record f_env in
  let f_ret_ty = f_er.Env.fn_return_type in
  if not (is_valid_type f_ret_ty) then
    failwith "[ERR] type of this function is not determined";
  let ret_ty_cenv = Type.as_unique f_ret_ty in

  let f_ret_val_cat = ret_val_category f_ret_ty ctx in
  let f_ret_lt = Env.get_scope_lifetime parent_env in (* TODO: fix *)
  let f_ret_ml = ret_ty_cenv.Env.meta_level in

  let ret_ty_sto =
    Option.default_delayed (fun () -> suitable_storage f_ret_ty ctx) opt_sto
  in

  let node = TAst.GenericCallExpr (
                 (ref ret_ty_sto),
                 n_earg_exprs,
                 Some parent_env,
                 Some f_env) in
  let node_aux = (f_ret_ty, f_ret_val_cat, f_ret_lt, f_ret_ml, loc) in
  (node, node_aux)


and analyze_inner node parent_env ctx opt_chain_attr =
  let pre_node = extract_prev_pass_node node in
  analyze ~opt_attr:opt_chain_attr pre_node parent_env ctx

and pre_analyze_inner node parent_env ctx opt_chain_attr =
  let pre_node = extract_prev_pass_node node in
  solve_forward_refs ~opt_attr:opt_chain_attr pre_node parent_env ctx

and extract_prev_pass_node node =
  match node with
  | TAst.PrevPassNode n -> n
  | _ -> failwith "[ICE] not prev node"


and analyze_param f_env param ctx attr =
  let (var_attr, var_name, init_part) = param in
  let param_kind = match init_part with
    (* Ex. :int = 10 *)
    | (Some type_expr, Some defalut_val) ->
       begin
         failwith "declare_function_params : not implemented / default value of param"
       end

    (* Ex. :int *)
    | (Some type_expr, None) ->
       begin
         let ty = resolve_type_with_qual var_attr type_expr f_env ctx attr in
         Env.FnParamKindType ty
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

  let ninit_part = (None, None) in   (* a type node and a default value are no longer necessary *)
  let nparam: TAst.param_init_t = (var_attr, var_name, ninit_part) in
  (nparam, param_kind)

and make_parameter_venv f_env param_name param_ty ctx =
  let venv_r = {
    Env.var_name = param_name;
    Env.var_type = param_ty;
    Env.var_detail = Env.VarRecordNormal ();
  } in
  let venv = Env.create_context_env f_env (
                                      Env.Variable (venv_r)
                                    )
  in
  Env.update_status venv Env.Complete;
  (param_name, venv)


and prepare_params env special_params params_node ctx attr =
  match extract_prev_pass_node params_node with
  | Ast.ParamsList ps ->
     declare_function_params env special_params ps ctx attr

  | _ -> failwith "check_params / unexpected"


and typeinfo_of_paramkind pk =
  match pk with
  | Env.FnParamKindType ty -> ty

and adjust_param_types param_kinds args =
  adjust_param_types' param_kinds args []
  |> Option.map List.rev

and adjust_param_types' param_kinds args acc =
  match (param_kinds, args) with
  | (param_info :: px, _ :: ax) ->
     begin
       match param_info with
       | Env.FnParamKindType ty ->
          adjust_param_types' px ax (ty :: acc)
     end
  | (param_info :: px, []) ->
     begin
       match param_info with
       | Env.FnParamKindType ty ->
          adjust_param_types' px [] (ty :: acc)
     end
  | ([], []) -> Some acc
  | ([], _) -> None

and exclude_optional_params param_kinds =
  let rec exclude_optional_params' param_kinds acc =
    match param_kinds with
    | [] -> acc
    | (k :: ks) ->
       begin
         match k with
         | Env.FnParamKindType ty ->
            exclude_optional_params' ks (ty :: acc)
       end
  in
  exclude_optional_params' param_kinds []
  |> List.rev

and declare_function_params f_env special_params params ctx attr =
  (* analyze parameters *)
  let (nparams, param_kinds) =
    let special_param_kinds =
      special_params
      |> List.map (fun (i, p) -> (i, Env.FnParamKindType p))
    in
    special_param_kinds @
      (params |> List.map (fun p -> analyze_param f_env p ctx attr))
    |> List.split
  in

  (* first, make all of environments.
   * next declare them into the function env *)
  let param_envs =
    let make_env param kind =
      let (_, opt_name, _) = param in
      match kind with
      | Env.FnParamKindType ty ->
         opt_name |> Option.map (fun name -> make_parameter_venv f_env name ty ctx)
    in
    let declare_env (name, venv) =
      Env.add_inner_env f_env name venv;
      venv
    in
    List.map2 make_env nparams param_kinds
    |> List.map (Option.map declare_env)
  in

  (nparams, param_kinds, param_envs)


and check_id_is_defined_uniquely env id =
  let res = Env.find_on_env env id in
  match res with
    Some _ -> failwith "same ids are defined"
  | None -> ()


and solve_identifier ?(do_rec_search=true)
                     ?(making_placeholder=false)
                     ?(exclude=[])
                     id_node env ctx attr
  =
  match id_node with
  | Ast.Id (name, _) ->
     solve_basic_identifier ~do_rec_search:do_rec_search
                            ~making_placeholder:making_placeholder
                            ~exclude:exclude
                            name env ctx attr

  | Ast.InstantiatedId (name, template_args, _) ->
     begin
       Printf.printf "$$$$$ Ast.InstantiatedId\n";
       let (evaled_t_args, _) =
         template_args
         |> List.map (fun e -> eval_expr_as_ctfe e env ctx attr)
         |> List.split
       in
       solve_basic_identifier ~do_rec_search:do_rec_search
                              ~template_args:evaled_t_args
                              ~making_placeholder:making_placeholder
                              ~exclude:exclude
                              name env ctx attr
     end

  | _ -> failwith "unsupported ID type"

and solve_basic_identifier ?(do_rec_search=true)
                           ?(template_args=[])
                           ?(making_placeholder=false)
                           ?(exclude=[])
                           name search_base_env ctx attr
    : (type_info_t * 'env * Meta_level.t) option =
  let type_ty = ctx.sc_tsets.ts_type_type in
  let ty_cenv = Type.as_unique type_ty in

  (* Class is a value of type, thus returns "type" of type, and corresponding env.
   * Ex, "int" -> { type: type, value: int }
   *)
  let single_type_id_node name cenv =
    try_to_complete_env cenv ctx;
    (type_ty, cenv, ty_cenv.Env.meta_level)
  in

  (* TODO: implement merging *)
  let solve (prev_ty, prev_opt_env, _) env : (type_info_t * 'env * Meta_level.t) =
    let { Env.er = env_r; _ } = env in
    match env_r with
    | Env.MultiSet (record) ->
       begin
         match record.Env.ms_kind with
         | Env.Kind.Class ->
            begin
              (* classes will not be overloaded. However, template classes may have
               * some definitions (because of specialization). Thus, type may be unclear...
               *)
              if List.length record.Env.ms_templates <> 0 then
                match template_args with
                | [] ->
                   let ty =
                     Type.Generator.generate_type ctx.sc_tsets.ts_type_gen
                                                  (Type_info.ClassSetTy env)
                                                  template_args
                                                  Type_attr.undef
                   in
                   (ty, env, ty_cenv.Env.meta_level)
                | xs ->
                   if making_placeholder then
                     begin
                       (* TODO: fix it *)
                       let uni_id =
                         Unification.generate_uni_id ctx.sc_unification_ctx in
                       Unification.update_type ctx.sc_unification_ctx
                                               uni_id ctx.sc_tsets.ts_type_type;
                       let ty =
                         Type.Generator.generate_type ctx.sc_tsets.ts_type_gen
                                                      (Type_info.ClassSetTy env)
                                                      template_args
                                                      Type_attr.undef
                       in
                       Unification.update_value ctx.sc_unification_ctx
                                                uni_id (Ctfe_value.Type ty);
                       (**)
                       let ty =
                         Type.Generator.generate_type ctx.sc_tsets.ts_type_gen
                                                      (Type_info.NotDetermined uni_id)
                                                      template_args
                                                      Type_attr.undef
                       in
                       (ty, env, ty_cenv.Env.meta_level)
                     end
                   else
                     begin
                       let instances =
                         instantiate_class_templates env xs
                                                     search_base_env ctx attr
                       in
                       match instances with
                       | [e] -> (type_ty, e, ty_cenv.Env.meta_level)
                       | _ -> failwith "[ERR] ambiguous definitions"
                     end

              else begin
                if (List.length template_args <> 0) then
                  failwith "[ERR] there is no template class";

                match List.length record.Env.ms_normal_instances with
                | 1 ->
                   let single_cenv = List.hd record.Env.ms_normal_instances in
                   single_type_id_node name single_cenv
                | _ -> failwith "[ICE] unexpected : class / multi-set"
              end
            end

         | Env.Kind.Function ->
            begin
              (* functions will be overloaded *)
              let ty =
                Type.Generator.generate_type ctx.sc_tsets.ts_type_gen
                                             (Type_info.FunctionSetTy env)
                                             template_args
                                             {
                                               Type_attr.ta_mut = Type_attr.Immutable;
                                               Type_attr.ta_ref_val = Type_attr.Ref;
                                             }
              in
              (ty, env, env.Env.meta_level)
            end
         | _ -> failwith "unexpected env : multi-set kind"
       end

    (* only builtin classes may be matched *)
    | Env.Class (_) -> single_type_id_node name env

    | Env.Variable (vr) ->
       begin
         try_to_complete_env env ctx;
         let {
           Env.var_type = var_ty;
         } = vr in
         (* TODO: check class variable *)

         (var_ty, env, env.Env.meta_level)
       end

    (* returns **type** of MetaVariable, NOT value *)
    | Env.MetaVariable (uni_id) ->
       begin
         let (term_uni_id, c) =
           Unification.search_type_until_terminal ctx.sc_unification_ctx uni_id
         in
         match c with
         | (Unification.Val ty) ->
            (ty, env, env.Env.meta_level)
         | (Unification.Undef) ->
            let ty =
              Type.Generator.generate_type ctx.sc_tsets.ts_type_gen
                                           (Type_info.NotDetermined uni_id)
                                           template_args
                                           Type_attr.undef
            in
            (ty, env, env.Env.meta_level)
         | _ -> failwith "[ICE] meta ver"
       end

    | _ -> failwith "solve_simple_identifier: unexpected env"
  in

  let name_s = Nodes.string_of_id_string name in
  Printf.printf "-> finding identitifer = %s : rec = %b\n" name_s do_rec_search;
  let oenv = if do_rec_search then
               Env.lookup ~exclude:exclude search_base_env name_s
             else
               Env.find_all_on_env search_base_env name_s
  in
  match oenv with
  | [] -> None
  | envs -> Some (List.fold_left solve (Type_info.undef_ty, Env.undef (), Meta_level.Runtime) envs)


and make_notdetermined_type uni_id ctx =
  Type.Generator.generate_type ctx.sc_tsets.ts_type_gen
                               (Type_info.NotDetermined uni_id)
                               [] (* TODO: currenyly, meta var has no template args. support template template parameters *)
                               Type_attr.undef

and normalize_mata_var_as_type uni_id ctx =
  let (term_uni_id, c) =
    Unification.search_type_until_terminal ctx.sc_unification_ctx uni_id
  in
  match c with
  | (Unification.Val ty) -> ty
  | (Unification.Undef) -> make_notdetermined_type uni_id ctx
  | _ -> failwith "[ICE] meta ver"


and try_to_complete_env env ctx =
  if Env.is_incomplete env then
    match env.Env.rel_node with
    | Some (node) ->
       begin
         let parent_env = Option.get env.Env.parent_env in
         ignore @@ construct_env node parent_env ctx None;
         if not (Env.is_complete env) then
           failwith "? recursice definition is appeared"; (* TODO: exception *)
         ()
       end
    | None -> failwith "[ICE] try to complete env / there is no rel node"
  else
    ()  (* DO NOTHING *)


and convert_type trg_ty src_arg ext_env ctx attr =
  let (_, src_aux) = src_arg in
  let (src_ty, src_val_cat, src_lt, src_ml, _) = src_aux in
  Printf.printf "convert_type from %s to %s\n" (Type.to_string src_ty) (Type.to_string trg_ty);

  if is_type_convertible_to src_ty trg_ty then begin
    (* same type *)
    let open Type_attr in
    match (trg_ty.Type_info.ti_attr, src_ty.Type_info.ti_attr) with
    | ({ta_ref_val = Val}, {ta_ref_val = _}) ->
       begin
         (* copy val/ref to value *)
         let cenv = Type.as_unique trg_ty in
         let res = solve_basic_identifier ~do_rec_search:false
                                          ctor_id_name cenv ctx attr in
         let (_, ctor_env, _) = match res with
           | Some v -> v
           | None -> failwith "constructor not found"
         in
         let m_r = Env.MultiSetOp.get_record ctor_env in
         (* m_r.Env.ms_kind *)
         let ctor_fenvs = m_r.Env.ms_normal_instances in

         let select_move_ctor env =
           let kind = Env.FunctionOp.get_kind env in
           match kind with
           | Env.FnKindMoveConstructor _ -> Some env
           | _ -> None
         in
         let select_copy_ctor env =
           let kind = Env.FunctionOp.get_kind env in
           match kind with
           | Env.FnKindCopyConstructor _ -> Some env
           | _ -> None
         in

         let solve_dup_ctor pred =
           (* NOTICE: copy/move ctors must have a ref paramater at 0th position,
            * because it will cause infinite loop of to call "convert_type"
            *)
           let funcs = List.filter_map pred ctor_fenvs in
           Printf.printf "=> number of funcs = %d\n" (List.length funcs);
           let selected = find_suitable_functions funcs [src_arg] ext_env ctx attr in
           let fns = match selected with
             | (FuncMatchLevel.ExactMatch, fs, _)
             | (FuncMatchLevel.QualConv, fs, _) -> fs
             | _ -> []
           in
           List.map (fun (f, _, _) -> f) fns
         in

         let f = match src_val_cat with
           | Value_category.VCatPrValue ->
              begin
                (* movable, thus lookup move ctor first. *)
                let mv_funcs = solve_dup_ctor select_move_ctor in
                match List.length mv_funcs with
                | 1 -> List.hd mv_funcs
                | 0 ->
                   begin
                     (* copy *)
                     let cp_funcs = solve_dup_ctor select_copy_ctor in
                     match List.length cp_funcs with
                     | 1 -> List.hd cp_funcs
                     | 0 -> failwith "[ERR] no move / copy ctors"
                     | n -> failwith "[ERR] many copy ctors"
                   end
                | n -> failwith "[ERR] many move ctors"
              end

           | Value_category.VCatLValue ->
              begin
                (* copy *)
                let cp_funcs = solve_dup_ctor select_copy_ctor in
                match List.length cp_funcs with
                | 1 -> List.hd cp_funcs
                | 0 -> failwith "[ERR] no copy ctors"
                | n -> failwith "[ERR] many copy ctors"
              end
         in
         (FuncMatchLevel.ExactMatch, Some (trg_ty, f))
       end

    | ({ta_ref_val = Ref; ta_mut = trg_mut},
       {ta_ref_val = Ref; ta_mut = src_mut}) ->
       begin
         let level = match (trg_mut, src_mut) with
           | (Immutable, Immutable) -> FuncMatchLevel.ExactMatch
           | (Immutable, Const)
           | (Immutable, Mutable) -> FuncMatchLevel.NoMatch

           | (Const, Immutable) -> FuncMatchLevel.QualConv
           | (Const, Const) -> FuncMatchLevel.ExactMatch
           | (Const, Mutable) -> FuncMatchLevel.QualConv

           | (Mutable, Immutable)
           | (Mutable, Const) -> FuncMatchLevel.NoMatch
           | (Mutable, Mutable) -> FuncMatchLevel.ExactMatch

           | _ -> failwith "conv"
         in
         (level, None)
       end

    | ({ta_ref_val = Ref; ta_mut = trg_mut},
       {ta_ref_val = Val; ta_mut = src_mut}) ->
       begin
         let level = match (trg_mut, src_mut) with
           | (Immutable, Immutable) -> FuncMatchLevel.ExactMatch
           | (Immutable, Const) -> FuncMatchLevel.QualConv
           | (Immutable, Mutable) -> FuncMatchLevel.NoMatch

           | (Const, Immutable) -> FuncMatchLevel.QualConv
           | (Const, Const) -> FuncMatchLevel.ExactMatch
           | (Const, Mutable) -> FuncMatchLevel.QualConv

           | (Mutable, Immutable)
           | (Mutable, Const) -> FuncMatchLevel.NoMatch
           | (Mutable, Mutable) -> FuncMatchLevel.ExactMatch

           | _ -> failwith "conv"
         in
         (level, None)
       end

    | _ -> failwith ""

  end else begin
    (* TODO: implement type conversion *)
    (FuncMatchLevel.NoMatch, None)
  end

and is_type_convertible_to src_ty trg_ty =
  if Type.has_same_class src_ty trg_ty then
    true
  else begin
    false
  end


and determine_function_return_type opt_ret_type env ctx opt_attr =
  match opt_ret_type with
  | Some (TAst.PrevPassNode ret_ty_expr) ->
     begin
       let ret_ty =
         resolve_type ret_ty_expr env ctx opt_attr
       in
       (ret_ty, false)
     end
  | None ->
     begin
       (* needs return type inference *)
       (Type_info.undef_ty, true)
     end
  | _ -> failwith "[ICE]"

and post_check_function_return_type fr ctx =
  let _ = match Type.type_sort fr.Env.fn_return_type with
    | Type_info.UniqueTy _ -> ()
    | Type_info.Undef ->
       begin
         (* if type is not determined, it should be void *)
         let void_ty = get_builtin_void_type default_ty_attr ctx in
         fr.Env.fn_return_type <- void_ty;
       end
    | _ -> failwith @@ "[ERR] type couldn't be determined / " ^
                         (Nodes.string_of_id_string fr.Env.fn_name)
  in
  ()


and suitable_storage' ~exit_scope
                      ~param_passing
                      trg_ty ctx =
  assert (not (exit_scope && param_passing));
  let cenv = Type.as_unique trg_ty in
  let cr = Env.ClassOp.get_record cenv in

  if cr.Env.cls_traits.Env.cls_traits_is_primitive then
    begin
      let {
        Type_attr.ta_mut = mut;
        Type_attr.ta_ref_val = rv;
      } = trg_ty.Type_info.ti_attr in
      match rv with
      | Type_attr.Ref -> TAst.StoImm
      | Type_attr.Val when param_passing -> TAst.StoImm
      | _ ->
         begin
           match mut with
           | Type_attr.Immutable
           | Type_attr.Const -> TAst.StoImm
           | Type_attr.Mutable ->
              let trg_ref_ty =
                Type.Generator.update_attr_r ctx.sc_tsets.ts_type_gen trg_ty
                                             { trg_ty.Type_info.ti_attr with
                                               Type_attr.ta_ref_val = Type_attr.Ref
                                             }
              in
              TAst.StoStack trg_ref_ty
           | _ -> failwith "[ICE]"
         end
    end
  else
    if exit_scope then
      TAst.StoAgg
    else
      let trg_ref_ty =
        Type.Generator.update_attr_r ctx.sc_tsets.ts_type_gen trg_ty
                                     { trg_ty.Type_info.ti_attr with
                                       Type_attr.ta_ref_val = Type_attr.Ref
                                     }
      in
      TAst.StoStack trg_ref_ty

and suitable_storage ?(opt_operation=None)
                     trg_ty ctx =
  match opt_operation with
  | Some operation ->
     begin
       match operation with
       | SoExitScope ->
          suitable_storage' ~exit_scope:true
                            ~param_passing:false
                            trg_ty ctx
       | SoParamPassing ->
          suitable_storage' ~exit_scope:false
                            ~param_passing:true
                            trg_ty ctx
       | SoArrayElement index ->
          TAst.StoArrayElem (trg_ty, index)
     end
  | None ->
     suitable_storage' ~exit_scope:false
                       ~param_passing:false
                       trg_ty ctx

and apply_conv_filter ?(opt_operation=None)
                      filter expr ext_env ctx =
  let (expr_node, expr_aux) = expr in
  match filter with
  | Some (trg_ty, f_env) ->
     let sto = suitable_storage
                 ~opt_operation:opt_operation
                 trg_ty
                 ctx
     in
     let (_, _, _, expr_ml, _) = expr_aux in
     (* TODO: use default arguments *)
     let node = TAst.GenericCallExpr (ref sto, [expr_node], Some ext_env, Some f_env) in
     let lt = Env.get_scope_lifetime ext_env in (* TODO: fix *)
     let ml = expr_ml in (* TODO: fix *)
     let aux = (trg_ty, VCatPrValue, lt, ml, None) in
     (node, aux)

  | None -> (expr_node, expr_aux)


and adjust_expr_for_type ?(exit_scope=false)
                         trg_ty src_expr src_aux ext_env ctx attr =
  let (match_level, m_filter) =
    convert_type trg_ty (src_expr, src_aux) ext_env ctx attr
  in
  if match_level = FuncMatchLevel.NoMatch then
    failwith "[ERR] cannot convert type";

  let act = match exit_scope with
    | true -> Some SoExitScope
    | false -> None
  in
  apply_conv_filter ~opt_operation:act
                    m_filter (src_expr, src_aux) ext_env ctx


and resolve_type_with_qual ?(making_placeholder=false) ty_attr (expr:Ast.ast) env ctx attr : type_info_t =
  let ty = resolve_type ~making_placeholder:making_placeholder expr env ctx attr in
  Type.Generator.update_attr_r ctx.sc_tsets.ts_type_gen ty ty_attr

and resolve_type ?(making_placeholder=false) (expr:Ast.ast) env ctx attr : type_info_t =
  let (ctfe_val, _) =
    eval_expr_as_ctfe ~making_placeholder:making_placeholder
                      expr env ctx attr
  in
  extract_ctfe_val_as_type ctfe_val

and resolve_texpr_type ?(making_placeholder=false) texpr sem_ty meta_level
                       env ctx attr : type_info_t =
  let ctfe_val =
    eval_texpr_as_ctfe ~making_placeholder:making_placeholder
                       texpr sem_ty meta_level env ctx attr
  in
  extract_ctfe_val_as_type ctfe_val

and resolve_type_with_node ?(making_placeholder=false) expr env ctx attr =
  let (ctfe_val, nexpr) =
    eval_expr_as_ctfe ~making_placeholder:making_placeholder
                      expr env ctx attr
  in
  let ty = extract_ctfe_val_as_type ctfe_val in
  (ty, nexpr)


and extract_ctfe_val_as_type ctfe_val : type_info_t =
  match ctfe_val with
  | Ctfe_value.Type ty -> ty
  | _ -> failwith "This expression must be type"


and eval_expr_as_ctfe ?(making_placeholder=false) expr env ctx attr =
  Printf.printf "----> eval_expr_as_ctfe : begin ; \n";
  let (nexpr, (ty, _, _, ml, _)) =
    analyze_expr ~making_placeholder:making_placeholder
                 expr env ctx attr
  in
  let v = eval_texpr_as_ctfe ~making_placeholder:making_placeholder
                             nexpr ty ml
                             env ctx attr
  in
  (v, nexpr)

and eval_texpr_as_ctfe ?(making_placeholder=false)
                       texpr expr_ty expr_ml
                       env ctx attr =
  let _ = match expr_ml with
    | Meta_level.OnlyRuntime
    | Meta_level.Runtime ->
       TAst.print texpr;
       failwith "[ERR] can not evaluete this value on compile time"
    | _ -> ()
  in

  let ctfe_val =
    match Type.type_sort expr_ty with
    | Type_info.UniqueTy _ ->
       Ctfe_engine.execute ctx.sc_ctfe_engine texpr expr_ty ctx.sc_tsets

    | Type_info.NotDetermined _ ->
       Ctfe_value.Type expr_ty

    | _ -> failwith "[ICE] eval_expr_as_ctfe : couldn't resolve"
  in

  Printf.printf "<---- eval_expr_as_ctfe : end\n";
  ctfe_val


and tnode_of_ctfe_val ctfe_val ctx =
  match ctfe_val with
  | Ctfe_value.Int32 v -> TAst.IntLit (Int32.to_int v, 32, true, get_builtin_int32_type default_ty_attr ctx)
  | _ -> failwith ""


and evaluate_invocation_args args env ctx attr =
  args |> List.map (fun n -> evaluate_invocation_arg n env ctx attr)

and evaluate_invocation_arg expr env ctx attr =
  (* TODO: check CTFE-able node *)
  analyze_expr expr env ctx attr


and find_suitable_operator ?(universal_search=false)
                           op_name_id eargs env ctx attr =
  let opt_callee_function_info =
    let (_, lhs_arg_aux) = List.hd eargs in
    select_member_element ~universal_search:universal_search
                          lhs_arg_aux op_name_id env ctx attr
  in
  let check_type_and_solve_overload (callee_f_ty, callee_f_env, x) =
    let {
      Type_info.ti_sort = ty_sort;
      Type_info.ti_template_args = template_args;
    } = callee_f_ty in
    match ty_sort with
    | Type_info.FunctionSetTy menv ->
       solve_function_overload eargs template_args
                               menv env None ctx attr
    | _ -> failwith "[ICE]: operator must be defined as function"
  in
  opt_callee_function_info |> Option.map check_type_and_solve_overload


(* returns Env of function *)
and solve_function_overload eargs template_args mset_env ext_env loc ctx attr =
  let (args, arg_auxs) = List.split eargs in
  let mset_record = match mset_env.Env.er with
    | Env.MultiSet r -> r
    | _ -> Env.print mset_env;
           failwith "[ICE] solve_function_overload : Only Multiset is accepted"
  in
  if mset_record.Env.ms_kind <> Env.Kind.Function then
    failwith "[ICE] solve_function_overload : sort of menv must be function.";

  (* move the sequence of onlymeta args to template args *)
  let (eargs, template_args) =
    let (_, onlymeta_args_num) =
      let f (hdf, num) arg_aux =
        if not hdf then
          let (_, _, _, ml, _) = arg_aux in
          match ml with
          | Meta_level.OnlyMeta -> (hdf, num + 1)
          | _ -> (true, num)
        else
          (hdf, num)
      in
      List.fold_left f (false, 0) arg_auxs
    in
    Printf.printf "onlymeta_args_num = %d\n" onlymeta_args_num;

    let (meta_args, eargs) = List.split_at onlymeta_args_num eargs in
    let evaled_meta_args =
      let f earg =
        let (arg, (ty, _, _, ml, _)) = earg in
        eval_texpr_as_ctfe arg ty ml ext_env ctx None
      in
      List.map f meta_args
    in
    let template_args = evaled_meta_args @ template_args in

    Printf.printf "[Tl = %d Al = %d]\n" (List.length template_args) (List.length eargs) ;
    (eargs, template_args)
  in
  let (args, arg_auxs) = List.split eargs in

  let (f_level, fs_and_args, errs) = match template_args with
    (* has no template args *)
    | [] ->
       begin
         let (normal_f_level, normal_fs_and_args, nerrs) =
           find_suitable_functions mset_record.Env.ms_normal_instances
                                   eargs
                                   ext_env ctx attr
         in
         Printf.printf "!! normal function candidates = %s / %d\n"
                       (FuncMatchLevel.to_string normal_f_level)
                       (List.length normal_fs_and_args);

         match normal_f_level with
         | FuncMatchLevel.ExactMatch ->
            (normal_f_level, normal_fs_and_args, [])

         (* template functions might have more suitable ones than normal ones *)
         | _ ->
            begin
              let instanced_envs =
                instantiate_function_templates mset_env [] arg_auxs ext_env ctx attr
              in
              let (instanced_f_level, instanced_fs_and_args, terrs) =
                find_suitable_functions instanced_envs eargs ext_env ctx attr
              in
              Printf.printf "!! instanced function candidates = %s / %d\n"
                            (FuncMatchLevel.to_string instanced_f_level)
                            (List.length instanced_fs_and_args);
              if FuncMatchLevel.is_better instanced_f_level normal_f_level then
                (instanced_f_level, instanced_fs_and_args, terrs)
              else
                (normal_f_level, normal_fs_and_args, nerrs)
            end
     end

  (* has template args *)
  | _ ->
     begin
       let instanced_envs =
         instantiate_function_templates mset_env template_args arg_auxs
                                        ext_env ctx attr
       in
       Printf.printf "%%%%%%%%%%%%%%%% instanced_envs -> %d\n" (List.length instanced_envs);
       find_suitable_functions instanced_envs eargs ext_env ctx attr
     end
  in

  if f_level = FuncMatchLevel.NoMatch then
    error (Error.NoMatch (errs, loc));

  assert (List.length fs_and_args <> 0);
  if (List.length fs_and_args) > 1 then
    error_msg "[ERR] ambiguous";

  List.hd fs_and_args


and find_suitable_functions f_candidates args ext_env ctx attr
    : FuncMatchLevel.t * (env_t * conv_filter_t list * earg_t list) list * Error.t list =
  Printf.printf "number of candidates = %d\n" (List.length f_candidates);

  let calc_match_level f_env =
    try_to_complete_env f_env ctx;
    let f_record = Env.FunctionOp.get_record f_env in

    let opt_param_types = adjust_param_types f_record.Env.fn_param_kinds args in
    match opt_param_types with
    | Some param_types ->
       let params_num = List.length param_types in
       let args_num = List.length args in
       if args_num <> params_num then
         let err = Error.DifferentArgNum (params_num, args_num) in
         (FuncMatchLevel.NoMatch, f_env, [], args, Some err)
       else
         let (match_levels, conv_funcs, _, errmap) =
           let conv src_arg trg_ty (match_levels, conv_funcs, idx, errmap) =
             let (l, f) = convert_type trg_ty src_arg ext_env ctx attr in
             let pos = pos_of_earg src_arg in
             let errmap = match l with
               | FuncMatchLevel.NoMatch ->
                  begin
                    let m = match errmap with
                      | Some (m) -> m
                      | None -> Error.PosMap.empty
                    in
                    let m = Error.PosMap.add idx ("nomatch", pos) m in
                    Some m
                  end
               | _ -> errmap
             in
             (l::match_levels, f::conv_funcs, (idx+1), errmap)
           in
           List.fold_right2 conv args param_types ([], [], 0, None)
         in

         (* most unmatch level of parameters becomes function match level *)
         let total_f_level =
           List.fold_left FuncMatchLevel.bottom FuncMatchLevel.ExactMatch match_levels in

         let err = Option.map (fun m -> Error.ConvErr m) errmap in
         (total_f_level, f_env, conv_funcs, args, err)

    | None ->
       let params_num = List.length f_record.Env.fn_param_kinds in
       let args_num = List.length args in
       let err = Error.DifferentArgNum (params_num, args_num) in
       (FuncMatchLevel.NoMatch, f_env, [], args, Some err)
  in

  let collect (cur_order, fs_and_args, errs) candidate
    : FuncMatchLevel.t * (env_t * conv_filter_t list * earg_t list) list * Error.t list =
    let (total_f_level, f_env, conv_funcs, args, err) = calc_match_level candidate in
    let errs = match err with
      | Some e -> e :: errs
      | None -> errs
    in
    if FuncMatchLevel.is_better total_f_level cur_order then
      (* if more better function is found, remake candidates and raise level *)
      (total_f_level, [(f_env, conv_funcs, args)], errs)
    else if FuncMatchLevel.is_same total_f_level cur_order then
      (* if this function has same match level, add to candicates *)
      (cur_order, (f_env, conv_funcs, args) :: fs_and_args, errs)
    else
      (* ignore(do NOT append) function which has lower match level *)
      (cur_order, fs_and_args, errs)
  in
  let (level, fs_and_args, errs) =
    List.fold_left collect (FuncMatchLevel.NoMatch, [], []) f_candidates in

  if level = FuncMatchLevel.NoMatch then
    (level, [], errs)
  else
    (level, fs_and_args, errs)


and instantiate_function_templates menv template_args arg_auxs ext_env ctx attr =
  let instantiate t_env_record =
    let (temp_env, meta_var_names, uni_ids) =
      prepare_instantiate_template t_env_record template_args
                                   ext_env ctx attr
    in

    (* match valuse by arg types *)
    let inner_node = t_env_record.Env.tl_inner_node in
    let inner_node = match inner_node with
      | TAst.NotInstantiatedNode (n, _) -> n
      | _ -> failwith "[ICE] unexpected not instantiated node"
    in
    let (parameters, opt_cond, dn) = match inner_node with
      | Ast.FunctionDefStmt (_, Ast.ParamsList params, _, c, _, _, _) ->
         (params, c, 0)
      | Ast.ExternFunctionDefStmt (_, Ast.ParamsList params, _, _, _, _, _) ->
         (params, None, 0)
      | Ast.MemberFunctionDefStmt (name, Ast.ParamsList params, _, _, _, _) ->
         let is_special = match name with
           | Nodes.Pure s when s = ctor_name -> true
           | _ -> false
         in
         (params, None, if is_special then 0 else 1)
      | _ -> failwith ""
    in

    let param_types =
      List.map (fun decl -> get_param_type decl temp_env ctx attr) parameters
    in
    List.iteri (fun i ty -> Printf.printf "%d: %s\n" i (Type.to_string ty)) param_types;
    Printf.printf "REACHED / get_function_param_types\n";
    arg_auxs
    |> List.map (fun t -> let (ty, _, _, _, _) = t in ty)
    |> List.iteri (fun i ty -> Printf.printf "%d: %s\n" i (Type.to_string ty));
    Printf.printf "REACHED / get_function_arg_types\n";
    (* *)
    let params_type_value =
      param_types
      |> List.map (fun x -> Ctfe_value.Type x)
      |> List.enum
    in
    let args_type_value =
      arg_auxs
      |> List.map (fun t -> let (ty, _, _, _, _) = t in ty)
      |> List.map (fun x -> Ctfe_value.Type x)
      |> List.enum
    in
    Enum.drop dn args_type_value;
    Enum.iter2 (unify_arg_value ctx)
               params_type_value
               args_type_value;

    List.iter (fun c -> print_meta_var c ctx) uni_ids;
    Printf.printf "       REACHED / unify_arg_type \n";

    (**)
    complete_template_instance menv t_env_record
                               meta_var_names uni_ids
                               temp_env opt_cond
                               ctx attr
  in
  run_instantiate menv instantiate


and get_param_type (var_attr, _, init) env ctx attr =
  match init with
  | (Some ty_node, _) ->
     resolve_type_with_qual ~making_placeholder:true
                            var_attr ty_node
                            env ctx attr
  | _ -> failwith "not implemented / param nodes"


and unify_type ctx lhs rhs =
  let uni_map = ctx.sc_unification_ctx in
  match (lhs, rhs) with
  | ({Type_info.ti_sort = Type_info.NotDetermined lhs_uni_t_id;
      Type_info.ti_template_args = lhs_template_args},
     {Type_info.ti_sort = Type_info.NotDetermined rhs_uni_t_id;
      Type_info.ti_template_args = rhs_template_args}) ->
     begin
       (* TODO: check lhs_template_args and rhs_template_args *)
       Unification.link_type uni_map lhs_uni_t_id rhs_uni_t_id
     end
  | (({Type_info.ti_sort = (Type_info.UniqueTy _)} as ty),
     {Type_info.ti_sort = Type_info.NotDetermined uni_t_id})
  | ({Type_info.ti_sort = Type_info.NotDetermined uni_t_id},
     ({Type_info.ti_sort = (Type_info.UniqueTy _)} as ty))
    ->
     begin
       (* TODO: check template args *)
       Unification.update_type uni_map uni_t_id ty
     end
  | _ -> failwith "[ICE] unify_type"


and unify_type_value ctx lhs rhs =
  let uni_map = ctx.sc_unification_ctx in
  match (lhs, rhs) with
  | ({Type_info.ti_sort = Type_info.NotDetermined lhs_uni_t_id},
     {Type_info.ti_sort = Type_info.NotDetermined rhs_uni_t_id}) ->
     begin
       (* TODO: check template args *)
       Printf.printf "!! unify_type_value(T/T) / %d = %d\n" lhs_uni_t_id rhs_uni_t_id;
       Unification.link_value uni_map lhs_uni_t_id rhs_uni_t_id
     end
  | (({Type_info.ti_sort = (Type_info.UniqueTy ty_r);
       Type_info.ti_template_args = args} as ty),
     {Type_info.ti_sort = Type_info.NotDetermined uni_t_id;
      Type_info.ti_template_args = holder_args})
  | ({Type_info.ti_sort = Type_info.NotDetermined uni_t_id;
      Type_info.ti_template_args = holder_args},
     ({Type_info.ti_sort = (Type_info.UniqueTy ty_r);
       Type_info.ti_template_args = args} as ty)) ->
     begin
       Printf.printf "!! unify_type_value(T|V) / %d -> value [Act: %d, Hld: %d]\n"
                     uni_t_id
                     (List.length args)
                     (List.length holder_args);

       (* TODO: support variadic args *)
       Enum.iter2 (unify_arg_value ctx)
                  (List.enum args)
                  (List.enum holder_args);

       Printf.printf "!! unify_type_value(T|V) / <<<<\n";
       Unification.update_value uni_map uni_t_id (Ctfe_value.Type ty)
     end
  | (({Type_info.ti_sort = (Type_info.UniqueTy _)} as lhs_ty),
     ({Type_info.ti_sort = (Type_info.UniqueTy _)} as rhs_ty)) ->
     begin
       (* TODO: check template args *)
       if not (is_type_convertible_to rhs_ty lhs_ty) then
         raise Template_type_mismatch
               (*failwith "[ERR] cannot convert type at unify_type_value"*)
     end

  | (lhs, rhs) ->
     begin
       Printf.printf "lhs==\n";
       Type.print lhs;
       Printf.printf "rhs==\n";
       Type.print rhs;
       failwith "[ICE] unify_value_type"
     end


and unify_arg_value ctx lhs rhs =
  match (lhs, rhs) with
  | (Ctfe_value.Type lhs_ty, Ctfe_value.Type rhs_ty) ->
     begin
       unify_type_value ctx lhs_ty rhs_ty;
     end
  | (Ctfe_value.Undef _, Ctfe_value.Undef _) ->
     failwith "[ERR]"
  | (Ctfe_value.Undef uni_id, v)
  | (v, Ctfe_value.Undef uni_id) ->
     begin
       Printf.printf "!! unify_arg_value(T|V) / %d -> value\n" uni_id;
       Unification.update_value ctx.sc_unification_ctx uni_id v
     end
  | _ -> failwith "[ICE] not implemented (unify_arg_value)"


and print_ctfe_value value =
  Printf.printf "%s" (Ctfe_util.to_string value)


and print_meta_var uni_id ctx =
  let (_, ty_c) =
    Unification.search_type_until_terminal ctx.sc_unification_ctx uni_id
  in
  let (_, val_c) =
    Unification.search_value_until_terminal ctx.sc_unification_ctx uni_id
  in
  Printf.printf "uni_id(%d); type  is => %s\n" uni_id (
                  match ty_c with
                  | Unification.Val ty -> Type.to_string ty
                  | _ -> ">link or undef<"
                );
  Printf.printf "uni_id(%d); value is => %s\n" uni_id (
                  match val_c with
                  | Unification.Val value -> Ctfe_util.to_string value
                  | _ -> ">link or undef<"
                )


and prepare_template_params params_node ctx =
  match params_node with
  | TAst.PrevPassNode (Ast.TemplateParamsList params) ->
     begin
       let normalize param =
         let (meta_var_name, opt_init) = param in
         match opt_init with
         (* :U *)
         | Some (Some ty, None) -> failwith ":U / not supported"
         (* = V *)
         | Some (None, Some value) -> failwith "= V / not supported"
         (* :U = V *)
         | Some (Some ty, Some value) -> failwith ":U = V / not supported"
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
and select_member_element ?(universal_search=false)
                          recv_aux t_id env ctx attr
  : (type_info_t * 'env * Meta_level.t) option =
  let (recv_ty, _, _, _, _) = recv_aux in
  let recv_cenv = Type.as_unique recv_ty in
  Env.print recv_cenv;
  let opt_ty_ctx = solve_identifier ~do_rec_search:false
                                    t_id recv_cenv ctx attr in

  match opt_ty_ctx with
  | Some ty_ctx ->
     begin
       (* member env named id is found in recv_ty_r! *)
       Some ty_ctx
     end

  | None ->
     begin
       (* not found *)
       (* first, find the member function like "opDispatch" in recv_ty_r *)
       (* TODO: implement *)

       (* second, do universal_search *)
       if universal_search then begin
         (* TODO: exclude class envs *)
         solve_identifier ~exclude:[Env.Kind.Class] t_id env ctx attr

       end else
         None
     end


and propagate_type_attrs dest_ty src_ty ctx =
  let open Type_attr in
  let {
    Type_info.ti_attr = dest_attr;
  } = dest_ty in
  let {
    Type_info.ti_attr = src_attr;
  } = src_ty in

  let n_mut = mut_strong dest_attr.ta_mut src_attr.ta_mut in
  Type.Generator.update_attr_r ctx.sc_tsets.ts_type_gen
                               dest_ty
                               { dest_attr with
                                 ta_mut = n_mut;
                               }


and prepare_instantiate_template t_env_record template_args ext_env ctx attr =
  Printf.printf "\n-----\n&& start instantiation = %s\n-----\n\n"
                (Nodes.string_of_id_string t_env_record.Env.tl_name);

  let template_params =
    match t_env_record.Env.tl_params with
    | TAst.NotInstantiatedNode (Ast.TemplateParamsList params, None) -> params
    | _ -> failwith "[ICE] unexpected template params"
  in

  (* In this context, value of MetaVar is treated as TYPE *)

  (* *)
  let (meta_var_names, meta_var_inits) = List.split template_params in

  (* temporary environment for evaluate meta variables.
   * DO NOT append this env to the parent_env.
   *)
  let temp_env =
    Env.create_scoped_env ext_env (Env.Scope (Env.empty_lookup_table ())) in

  let (uni_ids, meta_specs) =
    (* generate meta variables which have no value and no type *)
    let generate_meta_var name =
      let uni_id = Unification.generate_uni_id ctx.sc_unification_ctx in
      let mv_env = Env.create_context_env temp_env (Env.MetaVariable uni_id) in
      let mv_ty = make_notdetermined_type uni_id ctx in
      (uni_id, (mv_ty, mv_env))
    in
    List.map generate_meta_var meta_var_names |> List.split
  in
  (* declare *)
  List.iter2 (fun n (_, e) -> Env.add_inner_env temp_env n e)
             meta_var_names meta_specs;

  (* set types of meta var(template variables) *)
  let set_type_to_meta_var (var_ty, env) opt_init =
    match opt_init with
    (* :U *)
    | Some (Some ty_expr, None) ->
       let var_attr = {
         Type_attr.ta_ref_val = Type_attr.Ref;
         Type_attr.ta_mut = Type_attr.Immutable;
       } in
       let ty = resolve_type_with_qual var_attr ty_expr temp_env ctx None in
       unify_type ctx var_ty ty

    (* = V *)
    | Some (None, Some value) -> failwith "= V / not supported"
    (* :U = V *)
    | Some (Some ty, Some value) -> failwith ":U = V / not supported"
    | Some (None, None) -> failwith "[ICE] unexpected"

    | None ->
       begin
         match Type.type_sort var_ty with
         | Type_info.NotDetermined uni_t_id ->
            unify_type ctx var_ty ctx.sc_tsets.ts_type_type
         | _ -> failwith "[ICE] unexpected"
       end
  in
  List.iter2 set_type_to_meta_var meta_specs meta_var_inits;

  Printf.printf "== PRINT META VARIABLES (after type set)\n";
  List.iter (fun c -> print_meta_var c ctx) uni_ids;

  (* set values of meta var(template variables) *)
  let set_default_value_to_meta_var uni_id =
    let ty = normalize_mata_var_as_type uni_id ctx in
    let ctfe_val = if Type.has_same_class ty ctx.sc_tsets.ts_type_type then
                     begin
                       let ud_ty = make_notdetermined_type uni_id ctx in
                       let type_val = Ctfe_value.Type ud_ty in
                       type_val
                     end
                   else
                     let undef_val = Ctfe_value.Undef uni_id in
                     undef_val
    in
    Unification.update_value ctx.sc_unification_ctx uni_id ctfe_val;
    ctfe_val
  in
  let template_params_default_values = List.map set_default_value_to_meta_var uni_ids in

  Printf.printf "== PRINT META VARIABLES (after default value set)\n";
  List.iter (fun c -> print_meta_var c ctx) uni_ids;

  Printf.printf "\nREACHED / set_default_value\n";

  Printf.printf "len(template_params_default_values) = %d\n"
                (List.length template_params_default_values);
  Printf.printf "len(template_args) = %d\n"
                (List.length template_args);

  (* match values by template args *)
  Enum.iter2 (unify_arg_value ctx)
             (List.enum template_params_default_values)
             (List.enum template_args);

  Printf.printf "\nREACHED / unify_arg_value\n";

  (* TODO: assign default template parameter values *)
  (temp_env, meta_var_names, uni_ids)


and complete_template_instance ?(making_placeholder=false)
                               menv t_env_record meta_var_names uni_ids
                               temp_env opt_cond
                               ctx attr =
  let normalize_meta_var uni_id =
    let normalize_uni_id uni_id search_f update_f =
      let (last_uni_id, c) =
        search_f ctx.sc_unification_ctx uni_id
      in
      match c with
      | Unification.Val v ->
         update_f ctx.sc_unification_ctx uni_id v
      | _ -> raise Instantiation_failed (*failwith "[ERR] not resolved"*)
    in

    let normalize_uni_type uni_id =
      normalize_uni_id uni_id
                       Unification.search_type_until_terminal
                       Unification.update_type
    in
    let normalize_uni_value uni_id =
      let update ctx uni_id v =
        match v with
        | Ctfe_value.Undef _ -> raise Instantiation_failed (*failwith "[ERR] not resolved"*)
        | Ctfe_value.Type ty ->
           begin
             if Type.is_unique_ty ty then
               Unification.update_value ctx uni_id v
             else
               raise Instantiation_failed (*failwith "[ERR] not resolved"*)
           end
        | _ -> Unification.update_value ctx uni_id v
      in
      normalize_uni_id uni_id
                       Unification.search_value_until_terminal
                       update
    in

    normalize_uni_type uni_id;
    normalize_uni_value uni_id
  in
  List.iter normalize_meta_var uni_ids;

  let (inner_node, inner_attr) = match t_env_record.Env.tl_inner_node with
    | TAst.NotInstantiatedNode (n, a) -> (n, a)
    | _ -> failwith "[ICE] unexpected not instantiated node"
  in

  (* cond *)
  let is_instantiable = match opt_cond with
    | Some cond ->
       begin
         (*let scope_env =
                Env.create_scoped_env temp_env
                                      (Env.Scope (Env.empty_lookup_table ()))
              in*)
         let scope_env = temp_env in
         let (n_cond_expr, cond_aux) = analyze_expr cond scope_env ctx attr in
         let bool_ty = get_builtin_bool_type default_ty_attr ctx in
         let (conved_cond_node, (_, _, _, cond_ml, _)) =
           adjust_expr_for_type bool_ty n_cond_expr cond_aux
                                scope_env ctx attr
         in
         let ctfe_v =
           eval_texpr_as_ctfe conved_cond_node bool_ty cond_ml
                              scope_env ctx None
         in
         match ctfe_v with
         | Ctfe_value.Bool b -> b
         | _ -> failwith "[ICE]"
       end
    | None -> true
  in
  if not is_instantiable then
    raise Instantiation_failed;

  List.iter (fun i -> print_meta_var i ctx) uni_ids;

  let mangled_sym =
    uni_ids
    |> List.map (Unification.get_as_value ctx.sc_unification_ctx)
    |> (fun x -> Mangle.s_of_template_args x ctx.sc_tsets)
  in
  let appendix_signature =
    let parameters = match inner_node with
      | Ast.FunctionDefStmt (_, Ast.ParamsList params, _, c, _, _, _) -> params
      | Ast.MemberFunctionDefStmt (_, Ast.ParamsList params, _, _, _, _) -> params
      | Ast.ExternFunctionDefStmt (_, Ast.ParamsList params, _, _, _, _, _) -> params
      | Ast.ClassDefStmt _ -> []
      | Ast.ExternClassDefStmt _ -> []
      | _ -> failwith "[ICE]"
    in
    let param_types =
      List.map (fun decl -> get_param_type decl temp_env ctx attr) parameters
    in
    param_types |> List.map Type.to_string |> String.concat "--"
  in
  let mangled_sym = mangled_sym ^ appendix_signature in
  Printf.printf "TRY making an instance! -> %s\n" mangled_sym;

  let mset_record = Env.MultiSetOp.get_record menv in
  let cache = Hashtbl.find_option mset_record.Env.ms_instanced_args_memo mangled_sym in
  match cache with
  | Some env ->
     Printf.printf "USED CACHE for %s\n" mangled_sym;
     env (* DO NOTHING, because this template is already generated *)
  | None ->
     begin
       let mvs = List.combine meta_var_names uni_ids in
       let env_parent = Option.get menv.Env.parent_env in

       (* instantiate! *)
       let n_ast =
         analyze ~meta_variables:mvs
                 ~opt_attr:inner_attr
                 inner_node env_parent ctx
       in

       let i_env = match n_ast with
         | TAst.GenericFuncDef (_, Some e)
         | TAst.ClassDefStmt (_, _, _, Some e)
         | TAst.ExternClassDefStmt (_, _, _, Some e) -> e
         | _ -> TAst.print n_ast; failwith "[ICE] complete template / cache"
       in

       (* memoize *)
       Hashtbl.add mset_record.Env.ms_instanced_args_memo mangled_sym i_env;
       i_env
     end


(* XXX: should ext_env be parent env of menv...? *)
and instantiate_class_templates menv template_args ext_env ctx attr =
  let instantiate t_env_record =
    let (temp_env, meta_var_names, uni_ids) =
      prepare_instantiate_template t_env_record template_args
                                   ext_env ctx attr
    in
    complete_template_instance menv t_env_record
                               meta_var_names uni_ids
                               temp_env None
                               ctx attr
  in
  run_instantiate menv instantiate


and run_instantiate menv f =
  let instantiate_wrapper f tenv =
    match f tenv with
    | exception Instantiation_failed -> None
    | exception Template_type_mismatch -> None
    | x -> Some x
  in
  let mset_record = Env.MultiSetOp.get_record menv in
  List.filter_map (instantiate_wrapper f) mset_record.Env.ms_templates


and map_conversions ?(param_passing=false) filters args ext_env ctx =
  let f filter arg =
    let act = match param_passing with
      | true -> Some SoParamPassing
      | false -> None
    in
    apply_conv_filter ~opt_operation:act filter arg ext_env ctx
  in
  List.map2 f filters args


and make_class_type cenv rv mut ctx =
  let cr = Env.ClassOp.get_record cenv in
  let template_args = cr.Env.cls_template_vals in
  let attr = {
    Type_attr.ta_ref_val = rv;
    Type_attr.ta_mut = mut;
  } in
  let ts = Type_info.UniqueTy cenv in
  Type.Generator.generate_type ctx.sc_tsets.ts_type_gen
                               ts template_args attr


and declare_incomple_ctor cenv =
  let (base_env, _) = Env.MultiSetOp.find_or_add cenv ctor_id_name Env.Kind.Function in
  let fenv_r = Env.FunctionOp.empty_record ctor_id_name in

  let fenv = Env.create_context_env cenv (
                                      Env.Function (
                                          Env.empty_lookup_table ~init:0 (),
                                          fenv_r)
                                    ) in
  Env.MultiSetOp.add_normal_instances base_env fenv;
  fenv

and declare_incomple_assign cenv =
  let (base_env, _) = Env.MultiSetOp.find_or_add cenv assign_name Env.Kind.Function in
  let fenv_r = Env.FunctionOp.empty_record assign_name in

  let fenv = Env.create_context_env cenv (
                                      Env.Function (
                                          Env.empty_lookup_table ~init:0 (),
                                          fenv_r)
                                    ) in
  Env.MultiSetOp.add_normal_instances base_env fenv;
  fenv


and define_trivial_default_ctor_for_builtin cenv extern_cname ctx =
  let ty = make_class_type cenv Type_attr.Val Type_attr.Const ctx in
  let fenv = declare_incomple_ctor cenv in

  (* interface of default constructor: void -> TYPE *)
  check_function_env fenv [] Meta_level.Meta ty false;

  let node = TAst.GenericFuncDef (None, Some fenv) in

  let detail =
    Env.FnRecordBuiltin (Env.FnDefDefaulted true,
                         Env.FnKindDefaultConstructor None,
                         (Builtin_info.make_builtin_default_ctor_name extern_cname))
  in
  complete_function_env fenv node ctor_id_name detail ctx


and define_trivial_copy_ctor_for_builtin cenv extern_cname ctx =
  let define mut =
    let ty = make_class_type cenv Type_attr.Val mut ctx in
    let rhs_ty = make_class_type cenv Type_attr.Ref mut ctx in
    let fenv = declare_incomple_ctor cenv in

    let detail =
      Env.FnRecordBuiltin (Env.FnDefDefaulted true,
                           Env.FnKindCopyConstructor None,
                           (Builtin_info.make_builtin_copy_ctor_name extern_cname))
    in
    (* interface of default constructor: TYPE -> TYPE *)
    check_function_env fenv [Env.FnParamKindType rhs_ty] Meta_level.Meta ty false;

    let node = TAst.GenericFuncDef (None, Some fenv) in
    complete_function_env fenv node ctor_id_name detail ctx
  in
  define Type_attr.Immutable;
  define Type_attr.Const;
(*define Type_attr.Mutable;*)


and define_trivial_copy_assign_for_builtin cenv extern_cname ctx =
  let ty = make_class_type cenv Type_attr.Ref Type_attr.Mutable ctx in
  let rhs_ty = make_class_type cenv Type_attr.Ref Type_attr.Const ctx in
  let fenv = declare_incomple_assign cenv in

  let detail =
    Env.FnRecordBuiltin (Env.FnDefDefaulted true,
                         Env.FnKindMember,
                         (Builtin_info.make_builtin_copy_assign_name extern_cname))
  in
  (* interface of default constructor: TYPE -> TYPE -> void *)
  check_function_env fenv [Env.FnParamKindType ty; Env.FnParamKindType rhs_ty] Meta_level.Meta ctx.sc_tsets.ts_void_type false;

  let node = TAst.GenericFuncDef (None, Some fenv) in
  complete_function_env fenv node assign_name detail ctx


and get_builtin_void_incomplete_type ctx : 'env type_info =
  ctx.sc_tsets.ts_void_type

and get_builtin_void_type attr ctx : 'env type_info =
  let ty = get_builtin_void_incomplete_type ctx in
  Type.Generator.update_attr_r ctx.sc_tsets.ts_type_gen ty attr


and get_builtin_bool_type attr ctx : 'env type_info =
  let ty = !(ctx.sc_tsets.ts_bool_type_holder) in
  assert (not @@ Type.is_undef ty);
  ty

and get_builtin_int32_type attr ctx : 'env type_info =
  get_builtin_int_type ~bits:32 ~signed:true attr ctx

and get_builtin_int_type ~bits ~signed attr ctx : 'env type_info =
  let ty = match bits with
    | 8 -> if signed then
             failwith "[ICE] get builtin int type"
           else
             !(ctx.sc_tsets.ts_uint8_type_holder)
    | 32 -> if signed then
              !(ctx.sc_tsets.ts_int32_type_holder)
            else
              !(ctx.sc_tsets.ts_uint32_type_holder)
    | _ -> failwith "[ICE] unsupported bits size"
  in
  assert (not @@ Type.is_undef ty);
  (*Type.Generator.update_attr_r ctx.sc_tsets.ts_type_gen ty attr*)
  ty


and get_builtin_raw_ptr_type elem_ty ptr_attr ctx : 'env type_info =
  let raw_ptr_ty = !(ctx.sc_tsets.ts_raw_ptr_type_holder) in
  assert (not @@ Type.is_undef raw_ptr_ty);

  Printf.printf "========= RawPtr Element\n";
  Type.print elem_ty;

  let ty = match Type.type_sort raw_ptr_ty with
    | Type_info.ClassSetTy menv ->
       begin
         let template_args = [Ctfe_value.Type elem_ty] in
         let ext_env = Option.get menv.Env.parent_env in
         let instances =
           instantiate_class_templates menv template_args
                                       ext_env ctx None
         in
         let cenv = match instances with
           | [e] -> e
           | _ -> failwith "[ICE] unexpected array instances"
         in
         Type.Generator.generate_type ctx.sc_tsets.ts_type_gen
                                      (Type_info.UniqueTy cenv)
                                      template_args
                                      ptr_attr
       end
    | _ -> failwith "[ICE] unexpected"
  in
  Type.print ty;
  ty

and get_builtin_array_type elem_ty len arr_attr ctx : 'env type_info =
  let arr_ty = !(ctx.sc_tsets.ts_array_type_holder) in
  assert (not @@ Type.is_undef arr_ty);

  Printf.printf "========= Array Element\n";
  Type.print elem_ty;

  let ty = match Type.type_sort arr_ty with
    | Type_info.ClassSetTy menv ->
       begin
         let template_args = [Ctfe_value.Type elem_ty;
                              Ctfe_value.Uint32 (Uint32.of_int len)
                             ] in
         let ext_env = Option.get menv.Env.parent_env in
         let instances =
           instantiate_class_templates menv template_args
                                       ext_env ctx None
         in
         let cenv = match instances with
           | [e] -> e
           | _ -> failwith "[ICE] unexpected array instances"
         in
         Type.Generator.generate_type ctx.sc_tsets.ts_type_gen
                                      (Type_info.UniqueTy cenv)
                                      template_args
                                      arr_attr
       end
    | _ -> failwith "[ICE] unexpected"
  in
  Type.print ty;
  ty


(* *)
and cache_builtin_type_info preset_ty name ctx =
  match Type.type_sort !preset_ty with
  (* not defined yet *)
  | Type_info.Undef ->
     begin
       Printf.printf "get_builtin_type_info = %s\n" name;

       let res =
         solve_basic_identifier ~do_rec_search:false
                                (Nodes.Pure name)
                                (Option.get ctx.sc_builtin_m_env) ctx None
       in
       match res with
       (* pure type *)
       | Some (ty, c_env, _) when ty == ctx.sc_tsets.ts_type_type ->
          begin
            let prim_ty =
              Type.Generator.generate_type ctx.sc_tsets.ts_type_gen
                                           (Type_info.UniqueTy c_env)
                                           []
                                           default_ty_attr
            in
            preset_ty := prim_ty;
          end

       (* template class *)
       | Some (ty, menv, _) when Type.is_class_set ty ->
          begin
            preset_ty := ty;
          end

       (**)
       | _ -> failwith "[ICE] cache_builtin_type_info: no definition"
     end

  (* already defined *)
  | _ -> ()

and make_type_default_form ?(rv=Type_attr.Val)
                           ?(mut=Type_attr.Const)
                           ty ctx =
  let trg_ty = Type.Generator.update_attr ctx.sc_tsets.ts_type_gen
                                          ty rv mut in
  match is_type_convertible_to ty trg_ty with
  | true -> Some trg_ty
  | false -> None

(*
and get_storage_ref n =
  match n with
  | TAst.GenericCall (_, storage, _, _, _) -> storage
  | TAst.GenericId (_, Some ctx) ->
     begin
       match ctx.Env.er with
       | Env.Variable _ ->
          begin
            ctx.Env.rel_node
              failwith "Yo"
          end
       | _ -> failwith "[ICE]"
     end
  | _ -> failwith ""
 *)

and find_attr_val_impl opt_attr key f =
  match opt_attr with
  | Some tbl ->
     begin
       let opt_value_node = Hashtbl.find_option tbl key in
       match opt_value_node with
         Some value_node ->
         begin
           match value_node with
           | None -> Some (Ctfe_value.Bool true)
           | Some value ->
              begin
                let node = extract_prev_pass_node value in
                let ctfe_v = f node in
                Some (ctfe_v)
              end
         end
       | None -> None
     end
  | None -> None

and find_attr_ctfe_val opt_attr key parent_env ctx =
  let f node =
    let (nnode, (ty, _, _, ml, _)) = analyze_expr node parent_env ctx None in
    eval_texpr_as_ctfe nnode ty ml parent_env ctx None
  in
  find_attr_val_impl opt_attr key f

(* it can treat simple nodes *)
and find_attr_boot_val opt_attr key parent_env ctx =
  let f tnode =
    analyze_boot_expr tnode parent_env ctx None
  in
  find_attr_val_impl opt_attr key f

and find_attr_val is_boot opt_attr key parent_env ctx =
  let f = if is_boot then
            find_attr_boot_val
          else
            find_attr_ctfe_val
  in
  f opt_attr key parent_env ctx

and find_attr_bool_val ?(boot=false) opt_attr key parent_env ctx =
  let opt_v = find_attr_val boot opt_attr key parent_env ctx in
  match opt_v with
  | Some v -> begin match v with
                    | Ctfe_value.Bool b -> b
                    | _ -> failwith "[ERR] not bool value"
              end
  | None -> false (* default value *)

and find_attr_int32_val ?(boot=false) opt_attr key parent_env ctx =
  let opt_v = find_attr_val boot opt_attr key parent_env ctx in
  match opt_v with
  | Some v -> begin match v with
                    | Ctfe_value.Int32 i -> Some i
                    | _ -> failwith "[ERR] not int32 value"
              end
  | None -> None

and find_attr_uint32_val ?(boot=false) opt_attr key parent_env ctx =
  let opt_v = find_attr_val boot opt_attr key parent_env ctx in
  match opt_v with
  | Some v -> begin match v with
                    | Ctfe_value.Uint32 i -> Some i
                    | _ -> failwith "[ERR] not uint32 value"
              end
  | None -> None


and analyze ?(meta_variables=[]) ?(opt_attr=None) node env ctx =
  let (node, _) = analyze_t ~meta_variables:meta_variables
                            ~opt_attr:opt_attr
                            node env ctx
  in
  node

and analyze_t ?(meta_variables=[]) ?(opt_attr=None) node env ctx =
  let snode =
    solve_forward_refs ~meta_variables:meta_variables
                       ~opt_attr:opt_attr
                       node env ctx
  in
  construct_env snode env ctx opt_attr


and get_void_aux ctx =
  let ty = get_builtin_void_type default_ty_attr ctx in
  (ty, VCatPrValue, Type_attr.Static, Meta_level.Meta, None)
