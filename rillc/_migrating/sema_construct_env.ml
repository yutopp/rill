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
open Sema_context
open Sema_forward_ref
open Sema_utils
open Sema_env
open Sema_error

module EArgConvMap =
  struct
    module M = Map.Make(Int)
    include M

    let to_locmap m =
      let conv key (trg_ty, src_arg, level) =
        match level with
        | Function.MatchLevel.NoMatch ->
           let (tast, aux) = src_arg in
           let src_loc = TAst.loc_of tast in
           let src_ty = Aux.ty aux in
           Some (trg_ty, (src_ty, src_loc), level)
        | _ ->
           None
      in
      filter_map conv m
  end

type storage_operation =
  | SoExitScope
  | SoParamPassing
  | SoBind
  | SoArrayElement of int

let string_of_storage_operation sto_op =
  match sto_op with
  | SoExitScope -> "SoExitScope"
  | SoParamPassing -> "SoParamPassing"
  | SoBind -> "SoBind"
  | SoArrayElement i -> Printf.sprintf "SoArrayElement[%d]" i

(* TODO: fix *)
module LifetimeEnv =
  struct
    type t = int

    let init () =
      0

    let add_nest lt_env =
      lt_env + 1

    let nest_level lt_env =
      Some (Env_system.NestLevel.of_int lt_env)
  end

(* TODO: fix *)
module SubExprSpec =
  struct
    type 'n t = {
      env_id: Env_system.EnvId.t;
      nest_level: Env_system.NestLevel.t;
      mutable aux_count: int;
      store: (int, 'n) Hashtbl.t;
    }

    let create base_env lt_env =
      let nest_level =
        Env.calc_nest_level ~ext_nest_level:(LifetimeEnv.nest_level lt_env)
                            base_env
      in
      {
        env_id = Env.get_id base_env;
        nest_level = nest_level;
        aux_count = 0;
        store = Hashtbl.create 10;
      }

    let make_scope_lifetime ?(aux_count=0) spec sub_nest_level =
      Lifetime.LtDynamic (spec.env_id, spec.nest_level, sub_nest_level, aux_count)

    let register_temporary spec id (node, aux) =
      let cache_node_id = Nodes.CachedNodeCounter.generate () in
      let n_node = node |> TAst.map (fun _ -> TAst.SetCacheExpr (cache_node_id, node)) in
      let record = (cache_node_id, aux) in
      Hashtbl.add spec.store id record;
      n_node

    let allocate_aux_count spec =
      let new_id = spec.aux_count in
      if new_id = Int.max_num then
        failwith "[ICE]";
      spec.aux_count <- spec.aux_count + 1;
      new_id

    let remove spec aux_id =
      Hashtbl.remove spec.store aux_id

    let to_list spec =
      Hashtbl.values spec.store

    (* returns (captured obj list, tmp obj list) *)
    let to_lists spec opt_aux_id =
      match opt_aux_id with
      | Some aux_id ->
         let f k v (captured, other) =
           if k = aux_id then
             (v :: captured, other)
           else
             (captured, v :: other)
         in
         Hashtbl.fold f spec.store ([], [])
      | None ->
         ([], Hashtbl.values spec.store |> List.of_enum)
  end


(*
* this function will raise exceptions. NError or Fatal_error.
*)
let rec construct_env node parent_env lt_env ctx opt_chain_attr
  : (TAst.t * TAst.term_aux_t * env_t * LifetimeEnv.t) =
  let void_t = get_void_aux ctx in
  let as_checked env = (Env.get_rel_ast env, void_t, parent_env, lt_env) in
  let loc = TAst.loc_of node in
  match TAst.kind_of node with
  | TAst.Module (inner, pkg_names, mod_name, base_dir, Some env) ->
     begin
       cache_primitive_types env ctx;
       construct_env inner env lt_env ctx opt_chain_attr
     end

  | TAst.StatementList nodes ->
     let (nodes, last_ty, last_env, last_lt_env) =
       let construct_env_with_error_check node env lt_env =
         try
           construct_env node env lt_env ctx opt_chain_attr
         with
         | Normal_error err when ctx.sc_handle_error ->
            Sema_error.store_error_message err ctx;
            (TAst.{kind = TAst.ErrorTerm; loc}, void_t, env, lt_env)
       in
       (* TODO: TCO *)
       let rec p nodes env lt_env =
         match nodes with
         (* if there are no statements, it is void_type *)
         | [] -> ([], void_t, env, lt_env)
         (* if there is only one statement, evaluate it and type is of that *)
         | [x] ->
            let (n, s, e, le) = construct_env_with_error_check x env lt_env in
            ([n], s, e, le)
         | x :: xs ->
            let (n, _, _, le) = construct_env_with_error_check x env lt_env in
            let (ns, last_ty, last_env, last_lt_env) = p xs env le in
            ((n :: ns), last_ty, last_env, last_lt_env)
       in
       p nodes parent_env lt_env
     in
     (node |> TAst.map (fun _ -> TAst.StatementList nodes), last_ty, last_env, last_lt_env)

  | TAst.ExprStmt (TAst.{kind = TAst.PrevPassNode e}) ->
     let temp_obj_spec =
       SubExprSpec.create parent_env lt_env
     in
     let (node, aux) =
       analyze_expr ~making_placeholder:false
                    ~sub_nest:0
                    e parent_env temp_obj_spec ctx opt_chain_attr
     in

     let ((bound_lts, bound_dtor_calls), (_, dtor_calls)) =
       collect_temp_objs temp_obj_spec None parent_env lt_env ctx
     in
     assert (List.length bound_lts = 0);
     assert (List.length bound_dtor_calls = 0);

     let n_node =
       TAst.{kind = TAst.FinalyzeExpr (Some node, dtor_calls); loc = Loc.dummy}
     in
     (node |> TAst.map (fun _ -> TAst.ExprStmt n_node), aux, parent_env, lt_env)

  | TAst.VoidExprStmt (TAst.{kind = TAst.PrevPassNode e}) ->
     let (n_node, _, n_env, n_lt_env) =
       analyze_t ~opt_attr:opt_chain_attr e parent_env lt_env ctx
     in
     (node |> TAst.map (fun _ -> TAst.VoidExprStmt n_node), void_t, n_env, n_lt_env)

  | TAst.ReturnStmt (opt_e) ->
     begin
       let temp_obj_spec = SubExprSpec.create parent_env lt_env in

       let (opt_expr, expr_aux) = match opt_e with
         | Some (TAst.{kind = TAst.PrevPassNode e}) ->
            let (expr, aux) =
              analyze_expr ~making_placeholder:false
                           ~sub_nest:0
                           e parent_env temp_obj_spec ctx opt_chain_attr
            in
            (Some expr, aux)

         (* no return type. thus set as void *)
         | None ->
            let ty = get_builtin_void_type default_ty_attr ctx in
            let val_cat = Value_category.VCatPrValue in
            let sub_nest_lt = (Lifetime.LtSlNormal 0) in
            let lt = SubExprSpec.make_scope_lifetime temp_obj_spec sub_nest_lt in
            let ml = Meta_level.Runtime in (* TODO: fix *)
            (None, Aux.make ty val_cat lt ml)

         | _ -> failwith "[ICE]"
       in

       let ctx_env = parent_env.Env.context_env in
       let ctx_env_r = Env.FunctionOp.get_record ctx_env in

       let ret_ty = match ctx_env_r.Env.fn_is_auto_return_type with
         | true ->
            (* return type will be inferenced from the expression *)
            begin
              let expr_ty = Aux.ty expr_aux in

              let cur_ret_ty = ctx_env_r.Env.fn_return_type in
              match Type.type_sort cur_ret_ty with
              | Type_info.Undef ->
                 (* TODO: check type attrbutes *)
                 let nexpr_ty = match make_type_default_form expr_ty ctx with
                   | Some t -> t
                   | None -> failwith "[ERR] cannot convert"
                 in
                 (* check lifetime violation *)
                 let lts = nexpr_ty.Type_info.ti_aux_generics_args @ nexpr_ty.Type_info.ti_generics_args in
                 if List.length lts > 0 && not (List.exists (fun lt ->
                             List.exists (fun plt ->
                                 (* lt: plt *)
                                 Lifetime_constraints.(lt >= plt)
                               ) (ctx_env_r.Env.fn_generics_vals @ [Lifetime.LtStatic])
                           ) lts) then
                   begin
                     (* TODO: if fn_generics_vals is empty, change an error message *)
                     lts |> List.iter (fun l -> [%Loga.debug "= LT: %s\n" (Lifetime.to_string l)]);
                     ctx_env_r.Env.fn_generics_vals |> List.iter (fun l -> Debug.printf "= P LT: %s\n" (Lifetime.to_string l));
                     failwith "[ERR]"
                   end;

                 ctx_env_r.Env.fn_return_type <- nexpr_ty;
                 nexpr_ty

              | Type_info.UniqueTy _ ->
                 if Type.has_same_class cur_ret_ty expr_ty then
                   expr_ty
                 else
                   let error_loc =
                     (match opt_expr with
                      | Some e -> e
                      | None -> node) |> TAst.loc_of
                   in
                   error (Error_msg.DiffReturnType {loc = error_loc;
                                                    expect = cur_ret_ty;
                                                    actual = expr_ty})
              | _ -> failwith "[ICE]"
          end

         | false ->
            (* return type is already defined *)
            ctx_env_r.Env.fn_return_type
       in

       (**)
       let make_ret_expr expr =
         let (node, _) =
           adjust_expr_for_type ~action:(Some SoExitScope)
                                ret_ty expr expr_aux parent_env temp_obj_spec
                                ctx opt_chain_attr
         in
         node
       in
       let opt_ret_expr = Option.map make_ret_expr opt_expr in

       let df_nodes = Env.get_callee_funcs_when_context_exit parent_env in
       let n_node =
         TAst.{kind = TAst.FinalyzeExpr (opt_ret_expr, df_nodes); loc = Loc.dummy}
       in
       let node = node |> TAst.map (fun _ -> TAst.ReturnStmt (Some n_node)) in

       (**)
       Env.overlap_closed_info true parent_env;

       (node, void_t, parent_env, lt_env)
     end

  | TAst.FunctionDefStmt (
        name, lifetime_specs, params_node, ml, opt_ret_type, opt_cond, body, opt_attr,
        Some env
      ) ->
     if Env.is_checked env then as_checked env else
     begin
       (* TODO: check duplicate *)
       let name_s = Id_string.to_string name in
       Debug.printf "function %s - unchecked\n" name_s;

       let force_inline = Attribute.find_bool_val opt_attr "force_inline" ctx in

       (**)
       let (lt_params, lt_cx) = declare_generics_specs lifetime_specs env in

       (* declare/check parameters *)
       let (params, param_kinds, param_venvs, implicit_aux_lts, implicit_lts) =
         prepare_params env params_node ctx opt_attr
       in
       let implicit_generics_params = implicit_lts @ implicit_aux_lts in
       check_function_params env param_kinds;

       (* check body and check return type *)
       let () =
         let (ret_type, is_auto) =
           determine_function_return_type opt_ret_type
                                          lt_params implicit_lts implicit_aux_lts
                                          env ctx opt_attr
         in
         check_function_env2 env (lt_params @ implicit_generics_params) lt_cx
                             param_kinds ml ret_type is_auto
       in

       (* analyze body *)
       let nbody = analyze_inner body env lt_env ctx opt_attr in

       let _ = post_check_function_return_type env ctx in
       let nbody =
         check_and_insert_suitable_return nbody env ctx opt_attr
       in

       Debug.printf "function %s - complete / %s\n"
                    name_s (Env_system.EnvId.to_string env.Env.env_id);
       let node =
         node |> TAst.map (fun _ -> TAst.GenericFuncDef (Some nbody, Some env))
       in

       (* update record *)
       let fn_spec =
         Env.{
             fn_spec_param_envs = param_venvs;
             fn_spec_force_inline = force_inline;
         }
       in

       complete_function_env env node name
                             (Env.FnRecordNormal (Env.FnDefProvidedByUser,
                                                  Env.FnKindFree,
                                                  fn_spec))
                             ctx;
       (node, void_t, parent_env, lt_env)
     end

  | TAst.MemberFunctionDefStmt (
        name, lifetime_specs, params_node, quals, opt_ret_type, body, opt_attr,
        Some env
      ) ->
     if Env.is_checked env then as_checked env else
     begin
       (* TODO: check duplicate *)
       let name_s = Id_string.to_string name in
       Debug.printf "member function %s - unchecked\n" name_s;

       (* class env *)
       let parent_env = Option.get env.Env.parent_env in
       let ctx_env = parent_env.Env.context_env in

       let implicit_cls_generics_specs =
         let er = Env.ClassOp.get_record ctx_env in
         er.Env.cls_generics_vals
       in

       (**)
       let (lt_params, lt_cx) = declare_generics_specs lifetime_specs env in

       (* *)
       let member_qual default_qual =
         let f qual =
           match qual with
           | Nodes.QualMutable -> Some Type_attr.Mutable
           | Nodes.QualConst -> Some Type_attr.Const
           | Nodes.QualImmutable -> Some Type_attr.Immutable
         in
         let qual_attrs = List.filter_map f quals in
         let f opt_mut ty_mut =
           match opt_mut with
           | None -> Some ty_mut
           | Some mut when mut = ty_mut -> Some ty_mut
           | _ -> failwith "[ERR]"
         in
         List.fold_left f None qual_attrs |> Option.default default_qual
       in

       match name with
       | Id_string.Pure s when s = ctor_name ->
          begin
            (* this function is constructor. *)
            let ret_ty =
              let attr = Type_attr.make Type_attr.Val Type_attr.Const in
              Sema_type.make_class_type ctx_env attr env ctx
            in

            (* check parameters *)
            let (params, param_kinds, param_venvs, implicit_aux_lts, implicit_lts) =
              prepare_params env params_node ctx opt_attr
            in
            let implicit_generics_params = implicit_lts @ implicit_aux_lts in
            let () = match opt_ret_type with
              | Some _ -> failwith "[ERR] constructor cannot have return type"
              | None -> ()
            in

            (* interface of constructor: params -> TYPE *)
            check_function_env2 env (lt_params @ implicit_cls_generics_specs @ implicit_generics_params) lt_cx
                                param_kinds Meta_level.Runtime ret_ty false;

            (* prepare "this" special var *)(* TODO: consider member qual *)
            let this_ty =
              let attr = Type_attr.make (Type_attr.Ref []) Type_attr.Mutable in
              Sema_type.make_class_type ctx_env attr env ctx
            in
            Debug.printf "$ ctor of %s || this: %s\n" (Id_string.to_string (Env.get_name ctx_env)) (Type.to_string this_ty);

            let (id_name, this_venv) =
              make_parameter_venv env (Id_string.Pure "this") this_ty ctx
            in
            Env.add_inner_env env id_name this_venv |> error_if_env_is_dupped Loc.dummy;

            (* analyze body *)
            let nbody = analyze_inner body env lt_env ctx opt_attr in
            let _ = post_check_function_return_type env ctx in
            let nbody =
              check_and_insert_suitable_return ~is_special_func:true
                                               nbody env ctx opt_attr
            in

            Debug.printf "function %s - complete\n" name_s;
            let node =
              node |> TAst.map (fun _ -> TAst.GenericFuncDef (Some nbody, Some env))
            in

            let kind = match Sema_class.constructor_kind param_kinds parent_env ctx with
              | Sema_class.SfDefaultCtor ->
                 Env.FnKindDefaultConstructor (Some this_venv)
              | Sema_class.SfCopyCtor ->
                 Env.FnKindCopyConstructor (Some this_venv)
              | Sema_class.SfUserDefinedCtor ->
                 Env.FnKindConstructor (Some this_venv)
              | _ -> failwith "[ERR] not supported kind of constructor"
            in

            let fn_spec = {
                Env.fn_spec_param_envs = param_venvs;
                Env.fn_spec_force_inline = false;
              }
            in
            let detail =
              Env.FnRecordNormal (Env.FnDefProvidedByUser, kind, fn_spec)
            in
            complete_function_env env node ctor_id_name detail ctx;

            (node, void_t, parent_env, lt_env)
          end

       | Id_string.Pure s when s = dtor_name ->
          (* this function is destructor. *)
          let ret_ty = get_builtin_void_incomplete_type ctx in

          (* check parameters *)
          let (_, param_kinds, _, implicit_aux_lts, implicit_lts) =
            prepare_params env params_node ctx opt_attr
          in
          let implicit_generics_params = implicit_lts @ implicit_aux_lts in

          (* default parametes is also not allowed *)
          let () = match param_kinds with
            | [] -> ()
            | _ -> failwith "[ERR] destructor cannot have parameters"
          in
          (* check return type *)
          let () = match opt_ret_type with
            | None -> ()
            | Some _ -> failwith "[ERR] destructor cannot have return type"
          in

          (* prepare "this" special var *)
          let (this_ty, this_venv) = declare_this_variable env ctx_env ctx in

          (* interface of destructor: TYPE(implicit this) -> void *)
          let param_kinds = [Env.FnParamKindType this_ty] in
          check_function_env2 env (lt_params @ implicit_generics_params) lt_cx
                              param_kinds Meta_level.Runtime ret_ty false;

          (* analyze body *)
          let nbody = analyze_inner body env lt_env ctx opt_attr in
          let _ = post_check_function_return_type env ctx in
          let nbody =
            check_and_insert_suitable_return ~is_special_func:true
                                             nbody env ctx opt_attr
          in

          Debug.printf "function %s - complete\n" name_s;
          let node =
            node |> TAst.map (fun _ -> TAst.GenericFuncDef (Some nbody, Some env))
          in

          let kind = Env.FnKindDestructor None in
          let fn_spec = {
              Env.fn_spec_param_envs = [Some this_venv];
              Env.fn_spec_force_inline = false;
          } in
          let detail =
            Env.FnRecordNormal (Env.FnDefProvidedByUser, kind, fn_spec)
          in
          complete_function_env env node dtor_id_name detail ctx;

          (node, void_t, parent_env, lt_env)

       | _ ->
          begin
            (* prepare "this" *)
            let this_param =
              let attr = {
                Type_attr.ta_ref_val = Type_attr.Ref [];
                Type_attr.ta_mut = member_qual Type_attr.Const; (* default const *)
              } in
              let this_ty = Sema_type.make_class_type ctx_env attr env ctx in
              ((attr, Some (Id_string.Pure "this"), (None, None)), this_ty)
            in

            (* check parameters *)
            let (params, param_types, param_venvs, implicit_aux_lts, implicit_lts) =
              prepare_params env ~special_params:[this_param] params_node ctx opt_attr
            in
            let implicit_generics_params = implicit_lts @ implicit_aux_lts in

            (* check body and check return type *)
            let (ret_type, is_auto) =
              determine_function_return_type opt_ret_type
                                             lt_params implicit_lts implicit_aux_lts
                                             env ctx opt_attr
            in
            check_function_env2 env (lt_params @ implicit_generics_params)
                                lt_cx param_types Meta_level.Runtime ret_type is_auto;

            (* analyze body *)
            let nbody = analyze_inner body env lt_env ctx opt_attr in

            let _ = post_check_function_return_type env ctx in
            let nbody =
              check_and_insert_suitable_return nbody env ctx opt_attr
            in

            Debug.printf "function %s - complete\n" name_s;
            let node =
              node |> TAst.map (fun _ -> TAst.GenericFuncDef (Some nbody, Some env))
            in

            (* update record *)
            let fn_spec = {
                Env.fn_spec_param_envs = param_venvs;
                Env.fn_spec_force_inline = false;
            } in

            complete_function_env env node name
                                  (Env.FnRecordNormal (Env.FnDefProvidedByUser,
                                                       Env.FnKindMember,
                                                       fn_spec))
                                  ctx;
            (node, void_t, parent_env, lt_env)
          end
       end

  | TAst.ExternFunctionDefStmt (
        name, lifetime_specs, params_node, ml, ret_type, _, extern_fname, opt_attr,
        Some env
      ) ->
     if Env.is_checked env then as_checked env else
     begin
       (* TODO: check duplicate *)
       let name_s = Id_string.to_string name in
       Debug.printf "extern function %s - unchecked\n" name_s;

       (**)
       let (lt_params, lt_cx) = declare_generics_specs lifetime_specs env in

       (* check parameters *)
       let (params, param_types, _, implicit_aux_lts, implicit_lts) =
         prepare_params env params_node ctx opt_attr in
       let implicit_generics_params = implicit_lts @ implicit_aux_lts in

       (* check body and check return type *)
       let (ret_type, is_auto) =
         determine_function_return_type (Some ret_type)
                                        lt_params implicit_lts implicit_aux_lts
                                        env ctx opt_attr
       in
       assert(is_auto = false);
       check_function_env2 env (lt_params @ implicit_generics_params) lt_cx
         param_types ml ret_type is_auto;

       let is_builtin = Attribute.find_bool_val opt_attr "builtin" ctx in

       (* body *)
       Debug.printf "extern function %s - complete (builtin=%b)\n" name_s is_builtin;
       let node =
         node |> TAst.map (fun _ -> TAst.GenericFuncDef (None, Some env))
       in

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

       (node, void_t, parent_env, lt_env)
     end

  | TAst.ClassDefStmt (
        name, lifetime_specs, body, opt_attr, Some cenv
      ) ->
     if Env.is_checked cenv then as_checked cenv else
     begin
       (* TODO: check duplicate *)
       let name_s = Id_string.to_string name in
       Debug.printf "class %s - unchecked\n" name_s;

       (**)
       let (lt_params, _) = declare_generics_specs lifetime_specs cenv in

       (* check! *)
       check_class_env cenv lt_params ctx;

       (* resolve member variables first *)
       let cenv_r = Env.ClassOp.get_record cenv in
       let () =
         let f venv =
           let node = Env.get_rel_ast venv in
           ignore (construct_env node cenv lt_env ctx opt_attr)
         in
         List.iter f cenv_r.Env.cls_member_vars
       in

       (**)
       let (class_size, class_align, member_offsets) =
         let member_layouts =
           Sema_definitions.calc_member_layouts cenv_r.Env.cls_member_vars
         in
         Sema_definitions.calc_class_layouts member_layouts
       in

       (*
        * First, scan special functions traits of member variables
        *)
       let member_vars_sf_diagnoses = Sema_class.scan_variables cenv_r in



       (* TODO: improve *)
       let is_primitive = Attribute.find_bool_val opt_attr "primitive" ctx in
       cenv_r.Env.cls_traits <- {
         cenv_r.Env.cls_traits with
         Env.cls_traits_is_primitive = is_primitive;
       };

       (* body *)
       let (nbody, _, _, _) =
         construct_env body cenv lt_env ctx opt_attr
       in

       (* TODO: check class characteristics *)
       let () =
         let class_states =
           Sema_class.scan_member_funcs member_vars_sf_diagnoses name_s cenv_r cenv ctx
         in

         cenv_r.Env.cls_traits <-
           Sema_class.SpecialMemberStates.diagnoses_to_traits cenv_r.Env.cls_traits class_states
       in

       (**)
       let define_special_members () =
         let () = match cenv_r.Env.cls_traits.Env.cls_traits_default_ctor_state with
           | Env.FnDefDefaulted true ->
              define_trivial_default_ctor cenv ctx
           | Env.FnDefDefaulted false ->
              define_implicit_default_ctor cenv lt_env ctx
           | _ -> ()
         in

         let () = match cenv_r.Env.cls_traits.Env.cls_traits_copy_ctor_state with
           | Env.FnDefDefaulted true ->
              define_trivial_copy_ctor cenv ctx
           | Env.FnDefDefaulted false ->
              define_implicit_copy_ctor cenv lt_env ctx
           | _ -> ()
         in

         ()
       in
       define_special_members ();

       Debug.printf "class %s - complete\n" name_s;
       let node =
         node |> TAst.map (fun _ ->
                           TAst.ClassDefStmt (
                               name,
                               [],   (* TODO *)
                               nbody,
                               opt_attr,
                               Some cenv
                             ))
       in

       (* update record *)
       let detail_r = Env.ClsRecordNormal in

       complete_class_env cenv node detail_r (Some (class_size, class_align));

       (node, void_t, parent_env, lt_env)
     end

  | TAst.ExternClassDefStmt (
        name, lifetime_specs, extern_cname, opt_body, opt_attr, Some cenv
      ) ->
     if Env.is_checked cenv then as_checked cenv else
     begin
       (* TODO: check duplicate *)
       let name_s = Id_string.to_string name in
       Debug.printf "extern class %s - unchecked\n" name_s;

       (**)
       let (lt_params, _) = declare_generics_specs lifetime_specs cenv in
       check_class_env cenv lt_params ctx;

       let cenv_r = Env.ClassOp.get_record cenv in

       (* currently, do not remake a node like other nodes *)
       Debug.printf "extern class %s - complete\n" name_s;

       let is_builtin = Attribute.find_bool_val opt_attr "builtin" ctx in
       if not is_builtin then
         failwith "[ERR]";

       let is_novalue = Attribute.find_bool_val opt_attr "novalue" ctx
       in
       Debug.printf "is_novalue : %b \n" is_novalue;

       let is_primitive = Attribute.find_bool_val opt_attr "primitive" ctx
       in
       let is_array_type = Attribute.find_bool_val opt_attr "array_type" ctx
       in
       let has_ptr_constraints = Attribute.find_bool_val opt_attr "ptr_constraints" ctx
       in
       cenv_r.Env.cls_traits <- {
         cenv_r.Env.cls_traits with
         Env.cls_traits_is_primitive = is_primitive;
       };

       let define_special_members_and_calc_layout () =
         if not is_array_type then
           begin
             let open Env in
             (* default constructor *)
             define_trivial_default_ctor_for_builtin
               cenv extern_cname ctx;
             cenv_r.cls_traits <- {
               cenv_r.cls_traits with
               cls_traits_default_ctor_state = Env.FnDefDefaulted true;
               };

             (* copy constructor *)
             define_trivial_copy_ctor_for_builtin
               ~has_ptr_constraints:has_ptr_constraints
               cenv extern_cname ctx;
             cenv_r.cls_traits <- {
               cenv_r.cls_traits with
               cls_traits_copy_ctor_state = Env.FnDefDefaulted true;
               };

             (* copy assign operator *)
             define_trivial_copy_assign_for_builtin
               ~has_ptr_constraints:has_ptr_constraints
               cenv extern_cname ctx;

             let opt_csize =
               Attribute.find_uint32_val opt_attr "size" ctx
             in
             let csize = match opt_csize with
               | Some v -> v
               | None -> failwith "[ERR]"
             in

             let opt_calign =
               Attribute.find_uint32_val opt_attr "align" ctx
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
             let () = match tval_ty_traits.Env.cls_traits_default_ctor_state with
               (* elements has TRIVIAL default constructor.
                * So we do not need to call default ctors of elements *)
               | Env.FnDefDefaulted true ->
                  define_trivial_default_ctor_for_builtin cenv extern_cname ctx;
                  cenv_r.Env.cls_traits <- {
                    cenv_r.Env.cls_traits with
                    Env.cls_traits_default_ctor_state = Env.FnDefDefaulted true;
                  };
               (*
                *)
               | Env.FnDefDefaulted false
               | Env.FnDefProvidedByUser ->
                  define_implicit_default_ctor_for_array cenv lt_env tval_ty tval_num ctx;
                  cenv_r.Env.cls_traits <- {
                    cenv_r.Env.cls_traits with
                    Env.cls_traits_default_ctor_state = Env.FnDefDefaulted false;
                  };
               (*
                *)
               | Env.FnDefDeleted ->
                  cenv_r.Env.cls_traits <- {
                    cenv_r.Env.cls_traits with
                    Env.cls_traits_default_ctor_state = Env.FnDefDeleted;
                  };
             in

             (* copy constructor *)
             let () = match tval_ty_traits.Env.cls_traits_copy_ctor_state with
               | Env.FnDefDefaulted true ->
                  define_trivial_copy_ctor_for_builtin cenv extern_cname ctx;
                  cenv_r.Env.cls_traits <- {
                    cenv_r.Env.cls_traits with
                    Env.cls_traits_copy_ctor_state = Env.FnDefDefaulted true;
                  };
               | Env.FnDefDefaulted false
               | Env.FnDefProvidedByUser ->
                  define_implicit_copy_ctor_for_array cenv lt_env tval_ty tval_num ctx;
                  cenv_r.Env.cls_traits <- {
                    cenv_r.Env.cls_traits with
                    Env.cls_traits_copy_ctor_state = Env.FnDefDefaulted false;
                  };
               | _ ->
                  failwith "[ERR] not implemented yet (copy ctor)"
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

       complete_class_env cenv node detail_r opt_layout;

       (node, void_t, parent_env, lt_env)
     end

  (* *)
  | TAst.MemberVariableDefStmt (v, Some env) ->
     if Env.is_checked env then as_checked env else
     begin
       (* TODO: implement *)
       let (var_attr, var_name, init_term) =
         match Ast.kind_of (extract_prev_pass_node v) with
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
              assert_valid_type var_ty;
              var_ty
            end
         | None -> failwith "[ERR] type spec is required in class decl"
       in
       check_env env Meta_level.Runtime;    (* TODO: fix *)

       let r = Env.VariableOp.get_record env in
       r.Env.var_type <- var_ty;

       complete_env env node;
       (node, void_t, parent_env, lt_env)
     end

  (* scoped declare *)
  | TAst.VariableDefStmt (var_metalevel, v, None) ->
     begin
       let (var_attr, var_name, init_term) =
         match Ast.kind_of (extract_prev_pass_node v) with
         | Ast.VarInit vi -> vi
         | _ -> failwith "unexpected node"
       in

       let venv_r = Env.VariableOp.empty_record var_name in
       let venv =
         Env.create_scoped_env ~has_ns:false
                               ~ext_nest_level:(LifetimeEnv.nest_level lt_env)
                               parent_env var_name
                               (Env.Variable (venv_r))
                               loc
       in

       let temp_obj_spec = SubExprSpec.create parent_env lt_env in
       let (opt_type, opt_init_value) = init_term in
       let opt_init_value_res =
         opt_init_value
         |> Option.map (fun node ->
                        analyze_expr ~sub_nest:0
                                     node parent_env temp_obj_spec ctx opt_chain_attr
                       )
       in

       (* type check for the variable *)
       let (type_node, value_node, value_aux, var_ty, var_metalevel) =
         match opt_type with
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
              let (res_expr_node, res_expr_aux) = match opt_init_value_res with
                | Some (expr_node, expr_ty_cat) ->
                   begin
                     adjust_expr_for_type ~action:(Some SoBind)
                                          var_ty expr_node expr_ty_cat
                                          parent_env temp_obj_spec ctx opt_chain_attr
                   end
                | None ->
                   begin
                     (* TODO: implement call default constructor *)
                     (TAst.{kind = TAst.Undef var_ty; loc = Loc.dummy}, Aux.make var_ty Value_category.VCatPrValue Lifetime.LtStatic Meta_level.Runtime)
                   end
              in
              (* TODO: fix var_metalevel *)
              (Some type_expr, res_expr_node, res_expr_aux, var_ty, var_metalevel)
            end

         (* var_type is infered from initial_value *)
         | None ->
            begin
              let (expr_node, expr_aux) = match opt_init_value_res with
                | Some v ->
                   v
                | None ->
                   failwith "[ERROR] initial value is required";
              in
              let {
                  Aux.ta_type = expr_ty;
                  Aux.ta_ml = expr_ml;
              } = expr_aux in
              if Type.has_same_class expr_ty (get_builtin_void_incomplete_type ctx) then
                error_msg "rhs is void type";

              let var_ty =
                Type.Generator.update_attr_r ctx.sc_tsets.ts_type_gen expr_ty
                                             var_attr
              in
              (* TODO: if var has meta level, qual must be immutable/const val *)
              let (conved_node, conved_aux) =
                adjust_expr_for_type ~action:(Some SoBind)
                                     var_ty expr_node expr_aux
                                     parent_env temp_obj_spec ctx opt_chain_attr
              in

              let {
                  Aux.ta_type = conved_type;
                  Aux.ta_ml = conved_ml;
              } = conved_aux in

              (* var_metalevel, pull up baseline of meta level when values are ref or mutable *)
              let var_metalevel =
                let type_attr = Type.type_attr conved_type in
                match type_attr.Type_attr.ta_ref_val with
                | Type_attr.Ref _ ->
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
                     Debug.printf "=> CONSTEXPR\n";
                     let ctfe_v =
                       eval_texpr_as_ctfe conved_node var_ty var_metalevel
                                          parent_env ctx opt_chain_attr
                     in
                     Ctfe_engine.register_metavar ctx.sc_ctfe_engine ctfe_v venv;
                     tnode_of_ctfe_val ctfe_v ctx
                   end
                | _ -> conved_node
              in

              (None, mled_node, conved_aux, var_ty, var_metalevel)
            end
       in
       check_env venv var_metalevel;

       let ((_, dtors_when_exit), (_, dtor_calls)) =
         collect_temp_objs temp_obj_spec (Some value_aux) parent_env lt_env ctx
       in

       (**)
       List.iter (Env.append_callee_when_exit parent_env) dtors_when_exit;

       Debug.printf "DEFINE_VARIABLE %s: %s (%s)"
                    (Id_string.to_string var_name)
                    (Type.to_string var_ty)
                    (Meta_level.to_string var_metalevel);

       (* register the variable to the environments *)
       Env.add_inner_env parent_env var_name venv |> error_if_env_is_dupped loc;

       let value_node =
         TAst.{kind = TAst.FinalyzeExpr(Some value_node, dtor_calls); loc = Loc.dummy} in
       let node =
         node
         |> TAst.map (fun _ ->
                      TAst.VariableDefStmt (
                          var_metalevel,
                          TAst.{kind = TAst.VarInit (var_attr, var_name, (type_node, Some value_node));
                                loc = Loc.dummy},
                          Some venv
                        ))
       in

       let var_lt = match var_attr.Type_attr.ta_ref_val with
         | Type_attr.Ref _  ->
            (* TODO: fix *)
            SubExprSpec.make_scope_lifetime temp_obj_spec (Lifetime.LtSlNormal 0)
         | Type_attr.Val ->
            SubExprSpec.make_scope_lifetime temp_obj_spec (Lifetime.LtSlNormal 0)
         | _ -> failwith "[ICE]"
       in
       let detail_r = Env.VarRecordNormal () in
       (* TODO: recheck var_lt *)
       complete_variable_env venv node var_ty var_lt detail_r ctx;

       let n_lt_env = LifetimeEnv.add_nest lt_env in
       (node, void_t, (*parent_env*)venv(**), n_lt_env)
     end

  | TAst.EmptyStmt ->
     (node, void_t, parent_env, lt_env)

  | _ ->
     TAst.print node;
     failwith "construct_env: unsupported node or nodes have no valid env"

and analyze_expr ?(making_placeholder=false)
                 ~sub_nest
                 ?(enable_ufcs=false)
                 (node : Ast.t) parent_env temp_obj_spec ctx attr
    : ('node * TAst.term_aux_t) =
  let void_t = get_void_aux ctx in
  let loc = Ast.loc_of node in
  match Ast.kind_of node with
  | Ast.BinaryOpExpr (lhs, op, rhs, _) ->
     begin
       let args = [lhs; rhs] in
       let (res, _) =
         analyze_operator ~sub_nest:(sub_nest+1)
                          op args loc parent_env temp_obj_spec ctx attr
       in
       match res with
       | Ok v -> v
       | Bad err -> error err
     end

  | Ast.UnaryOpExpr (op, expr, _) ->
     begin
       let args = [expr] in
       let (res, eargs) =
         analyze_operator ~sub_nest:(sub_nest+1)
                          op args loc parent_env temp_obj_spec ctx attr
       in
       match res with
       | Ok v -> v
       | Bad err -> error err
     end

  | Ast.SubscriptingExpr (receiver, opt_arg, _) ->
     begin
       let (op, args) = match opt_arg with
         | Some arg -> (Id_string.BinaryOp "[]", [receiver; arg])
         | None -> (Id_string.UnaryPostOp "[]", [receiver])
       in
       let (res, _) =
         let op_id_node = node |> Ast.map (fun _ -> Ast.Id (op, [], ())) in
         analyze_operator ~sub_nest:(sub_nest+1)
                          op_id_node args loc parent_env temp_obj_spec ctx attr
       in
       match res with
       | Ok v -> v
       | Bad err -> error err
     end

  | Ast.CallExpr (receiver, args, _) ->
     begin
       let (recv_node, recv_aux) =
         analyze_expr ~making_placeholder:making_placeholder
                      ~sub_nest:sub_nest
                      ~enable_ufcs:true
                      receiver parent_env temp_obj_spec ctx attr
       in
       let {
         Aux.ta_type = recv_type_info;
         Aux.ta_vcat = recv_val_cat;
         Aux.ta_lt = recv_lt;
         Aux.ta_ml = recv_ml;
       } = recv_aux in
       let eargs =
         evaluate_invocation_args ~sub_nest:(sub_nest+1)
                                  args parent_env temp_obj_spec ctx attr
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
         Type_info.ti_generics_args = generics_args;
       } = recv_type_info in
       Debug.printf "=>=>=> %d / %s\n"
                    (List.length generics_args)
                    (Meta_level.to_string recv_ml);
       let ((node, aux), call_trg_finfo) = match ty_sort with
         (* normal function call *)
         | Type_info.FunctionSetTy m_envs ->
            begin
              (* consider nested expr *)
              let (arg_exprs, arg_auxs) = match TAst.kind_of recv_node with
                | TAst.NestedExpr (lhs_node, lhs_aux, _, _) ->
                   (lhs_node :: arg_exprs, lhs_aux :: arg_auxs)
                | _ ->
                   (arg_exprs, arg_auxs)
              in
              let args = List.combine arg_exprs arg_auxs in
              let call_trg_finfo =
                solve_function_overload recv_ml args template_args
                                        m_envs parent_env loc ctx attr
              in
              let call_inst =
                match make_call_instruction call_trg_finfo loc None parent_env temp_obj_spec ctx with
                | Ok res ->
                   res
                | Bad err ->
                   failwith "[ERR]"
              in

              (call_inst, call_trg_finfo)
            end

         (* constructor / operator call *)
         | Type_info.UniqueTy type_cenv ->
            begin
              (* type_cenv will be Type *)
              let recv_ty =
                resolve_texpr_type recv_node recv_type_info recv_ml
                                   parent_env ctx attr
              in
              assert_valid_type recv_ty;

              let cls_generics_map = make_ctor_generics_map recv_ty in

              let recv_cenv = Type.as_unique recv_ty in
              let f_sto = suitable_storage recv_ty ctx in

              (* call constructor *)
              (* TODO: take into account op call *)
              let (res, _) = solve_basic_identifier ~do_rec_search:false
                                                    ~loc:loc
                                                    ctor_id_name [] recv_cenv ctx attr in
              let (ctor_ty, _, _, ctor_ml, _) = match res with
                | Some v -> v
                | None -> failwith "[ERR] constructor not found"
              in
              let ctor_envs = match Type.type_sort ctor_ty with
                | Type_info.FunctionSetTy es -> es
                | _ -> failwith "[ERR] constructor must be function"
              in
              let call_trg_finfo =
                solve_function_overload ctor_ml eargs []
                                        ctor_envs parent_env loc ctx attr
              in
              let call_inst =
                match make_call_instruction ~mm:cls_generics_map call_trg_finfo loc (Some f_sto) parent_env temp_obj_spec ctx with
                | Ok res -> res
                | Bad err -> failwith "[ERR]"
              in

              (call_inst, call_trg_finfo)
            end

         | _ -> failwith "not implemented//" (* TODO: call ctor OR operator() *)
       in
       let (f_env, conv_filters, eargs) = call_trg_finfo in
       let f_ml = f_env.Env.meta_level in

       let mled_node = match f_ml with
         | Meta_level.OnlyMeta ->
            (* TODO: check whether the variable is ctfeable.
             * Ex. it must have trivial destructor *)
            (* let () = failwith "" in *)
            Debug.printf "ONLYMETA CTFE: %s" (Env.get_name f_env |> Id_string.to_string);
            let {
              Aux.ta_type = ty;
              Aux.ta_ml = ret_ml;
            } = aux in
            let ctfe_v =
              eval_texpr_as_ctfe node ty ret_ml
                                 parent_env ctx None
            in
            tnode_of_ctfe_val ctfe_v ctx
         | _ ->
            node
       in
       (mled_node, aux)
     end

  | Ast.ElementSelectionExpr (lhs, rhs, _) ->
     begin
       let (lhs_node, lhs_aux) =
         analyze_expr ~making_placeholder:making_placeholder
                      ~sub_nest:sub_nest
                      lhs parent_env temp_obj_spec ctx attr
       in
       let {
         Aux.ta_type = lhs_ty;
         Aux.ta_lt = rhs_lt;
       } = lhs_aux in
       match Type.type_sort lhs_ty with
       | Type_info.UniqueTy lhs_type_cenv ->
          begin
            let (opt_rhs_ty_node, hist) =
              select_member_element ~universal_search:enable_ufcs
                                    lhs_aux rhs parent_env ctx attr
            in
            match opt_rhs_ty_node with
            | Some (rhs_ty, rhs_env, rhs_lt, rhs_ml, _) ->
               let {
                 Aux.ta_type = recv_ty;
               } = lhs_aux in

               Debug.printf "= select_member_element === >>> START\n";
               let f lt =
                 Debug.printf "LT : %s\n" (Lifetime.to_string lt);
               in
               Debug.printf "= select_member_element === RHS >>> \n";
               List.iter f rhs_ty.Type_info.ti_generics_args;
               Debug.printf "= select_member_element === RHS <<< \n";

               Debug.printf "= select_member_element === <<< END\n";

               let cls_generics_map = make_ctor_generics_map recv_ty in
               Debug.printf "= select_member_element === generated map\n";

               let f3 lt =
                 Sema_lifetime.solve_var lt cls_generics_map
               in
               let new_generics_args = List.map f3 rhs_ty.Type_info.ti_generics_args in
               List.iter f new_generics_args;

               Debug.printf "= select_member_element === new generics\n";
               let node =
                 TAst.{kind = NestedExpr (lhs_node, lhs_aux, rhs_ty, Some rhs_env); loc}
               in
               let prop_ty = propagate_type_attrs rhs_ty lhs_ty ctx in
               let prop_ty = create_updated_type ~generics_args:new_generics_args
                                                 prop_ty ctx in
               Debug.printf "= select_member_element === %s\n" (Type.to_string prop_ty);
               let lt =
                 (* TODO: fix *)
                 SubExprSpec.make_scope_lifetime temp_obj_spec (Lifetime.LtSlNormal 0)
               in
               let ml = Meta_level.Runtime in (* TODO: fix *)
               let aux = Aux.make prop_ty VCatLValue lt ml in
               (node, aux)

            | None ->
               error (Error_msg.MemberNotFound (Type.as_unique lhs_ty, hist, loc))
          end

       | _ -> failwith "[ICE]"
     end

  | Ast.StatementTraitsExpr (keyword, block) ->
     begin
       match keyword with
       | "semantics" ->
          begin
            let default_ty_attr = {
              Type_attr.ta_ref_val = Type_attr.Val;
              Type_attr.ta_mut = Type_attr.Immutable;
            } in
            let ty = Sema_builtin.get_builtin_bool_type default_ty_attr ctx in
            let ty =
              Type.Generator.update_attr ctx.sc_tsets.ts_type_gen ty
                                         Type_attr.Val
                                         Type_attr.Immutable
            in
            assert_valid_type ty;

            let test () =
              (* A temporary environment for checking whether semantics is valid or not.
               * DO NOT append this env to the parent_env.
               *)
              let ctx_tmp = Sema_context.make_temporary_context ctx in
              let temp_env =
                Env.create_scoped_env parent_env (Id_string.Pure "<tmp>")
                                      (Env.Scope (Env.empty_lookup_table ()))
                                      None
              in
              let (_, _) =
                analyze_expr ~sub_nest:sub_nest
                             block temp_env temp_obj_spec ctx_tmp attr
              in
              true
            in
            let could_compile =
              try test () with
              | e ->
                 Debug.printf "STATEMENT TRAIT ERROR: %s\n"
                              (Printexc.to_string e);
                 false
            in
            let node = TAst.{kind = BoolLit (could_compile, ty); loc} in
            let sub_nest_lt = (Lifetime.LtSlNormal sub_nest) in
            let lt = SubExprSpec.make_scope_lifetime temp_obj_spec sub_nest_lt in
            let aux = Aux.make ty VCatPrValue lt Meta_level.Meta in
            (node, aux)
          end
       | _ -> failwith @@ "__statement_traits : not implemented / " ^ keyword
     end

  | Ast.Id (name, _, _)
  | Ast.InstantiatedId (name, _, _, _)->
     begin
       let (res, hist) =
         solve_identifier ~making_placeholder:making_placeholder
                          node parent_env ctx attr
       in
       let (ty, trg_env, lt, ml, lt_map) = match res with
         | Some v -> v
         | None ->
            (* member? TODO: fix *)
            error (Error_msg.MemberNotFound (parent_env, hist, loc))
       in

       (*parent_env.Env.generics_constraints <-
         Env.Constraint_record.merge parent_env.Env.generics_constraints gcs;*)

       Debug.printf "ID = %s - %s\n" (Id_string.to_string name) (Meta_level.to_string ml);

       (* both of id and instantiated_id will be id node *)
       let generics_args = Sema_lifetime.LifetimeMap.to_list lt_map in
       let node = TAst.{kind = GenericId (name, generics_args, Some trg_env); loc} in
       let aux = Aux.make ty VCatLValue lt ml in
       (node, aux)
     end

  | Ast.IntLit (i, bits, signed, _) ->
     begin
       let attr = {
         Type_attr.ta_ref_val = Type_attr.Val;
         Type_attr.ta_mut = Type_attr.Immutable;
       } in
       let ty = Sema_builtin.get_builtin_int_type ~bits:bits ~signed:signed attr ctx in
       assert_valid_type ty;
       let node = TAst.{kind = IntLit (i, bits, signed, ty); loc} in

       let vc = VCatPrValue in
       let sub_nest_lt = (Lifetime.LtSlNormal sub_nest) in
       let lt = SubExprSpec.make_scope_lifetime temp_obj_spec sub_nest_lt in
       let ml = Meta_level.Meta in
       let aux = Aux.make ty vc lt ml in
       (node, aux)
     end

  | Ast.BoolLit (b, _) ->
     begin
       let attr = {
         Type_attr.ta_ref_val = Type_attr.Val;
         Type_attr.ta_mut = Type_attr.Immutable;
       } in
       let ty = Sema_builtin.get_builtin_bool_type attr ctx in
       assert_valid_type ty;
       let node = TAst.{kind = BoolLit (b, ty); loc} in

       let vc = VCatPrValue in
       let sub_nest_lt = (Lifetime.LtSlNormal sub_nest) in
       let lt = SubExprSpec.make_scope_lifetime temp_obj_spec sub_nest_lt in
       let ml = Meta_level.Meta in
       let aux = Aux.make ty vc lt ml in
       (node, aux)
     end

  | Ast.StringLit (str, _) ->
     begin
       let attr = {
         Type_attr.ta_ref_val = Type_attr.Val;
         Type_attr.ta_mut = Type_attr.Immutable;
       } in
       (* val(immutable(uint8)) *)
       let elem_ty = Sema_builtin.get_builtin_int_type ~bits:8 ~signed:false attr ctx in
       (* val(immutable(raw_ptr!(val(immutable(uint8)))) *)
       let ptr_ty = get_builtin_raw_ptr_type elem_ty attr ctx in
       assert_valid_type ptr_ty;

       let n_ptr = TAst.{kind = StringLit (str, ptr_ty); loc} in
       let vc = VCatPrValue in
       let sub_nest_lt = (Lifetime.LtSlNormal sub_nest) in
       let lt = SubExprSpec.make_scope_lifetime temp_obj_spec sub_nest_lt in
       let ml = Meta_level.Meta in
       let aux = Aux.make ptr_ty vc lt ml in
       (n_ptr, aux)
     end

  | Ast.ArrayLit (elems, _, _) ->
     begin
       (* TODO: support typings for empty list literal *)
       assert(List.length elems > 0);
       (* evaluate all elementes *)
       let nargs =
         elems |> List.map (fun e -> analyze_expr ~making_placeholder:making_placeholder
                                                  ~sub_nest:sub_nest (* TODO: *)
                                                  e parent_env temp_obj_spec ctx attr)
       in

       let array_common_elem =
         let common_elem_arg arga argb =
           let (dn, s_aux) = arga in
           let (_,  t_aux) = argb in

           match convert_type (Aux.ty s_aux) argb parent_env ctx attr with
           | (Function.MatchLevel.ExactMatch, Function.ConvFunc (trg_ty, f, _)) ->
              begin
                let new_aux =
                  { s_aux with
                    Aux.ta_type = trg_ty;
                    Aux.ta_ml = Meta_level.bottom (Aux.ml s_aux) (Aux.ml t_aux)
                  }
                in
                (dn, new_aux)
              end
           | _ -> failwith "[ERR]"
         in
         List.reduce common_elem_arg nargs
       in
       let (_, elem_aux) = array_common_elem in
       let {
         Aux.ta_type = elem_ty;
         Aux.ta_ml = bottom_ml;
       } = elem_aux in

       (**)
       let trans_func_specs =
         let conv arg =
           let elem_cenv = Type.as_unique elem_ty in
           let res = convert_type elem_ty arg parent_env ctx attr in
           match res with
           | (Function.MatchLevel.NoMatch, _) -> failwith "[ERR]"
           | (_, Function.Trans _) -> failwith "[ERR]"
           | (_, (Function.ConvFunc (trg_ty, trans_func, _) as m_filter)) ->
              let is_primitive = Env.ClassOp.is_primitive elem_cenv in
              let is_trivial = Env.FunctionOp.is_trivial trans_func in
              (is_trivial && is_primitive, m_filter, arg)
         in
         List.map conv nargs
       in

       let statically_constructable =
         let is_all_statically_constructable =
           let f (b, _, _) = b in
           List.for_all f trans_func_specs
         in
         (Meta_level.has_meta_spec bottom_ml) && is_all_statically_constructable
       in

       (* copy/move ctor *)
       let conved_args =
         let conv index (_, m_filter, arg) =
           if statically_constructable then
             apply_conv_filter m_filter arg parent_env temp_obj_spec ctx
           else
             apply_conv_filter ~opt_operation:(Some (SoArrayElement index))
                               m_filter arg parent_env temp_obj_spec ctx
         in
         List.mapi conv trans_func_specs
       in
       let (n_nodes, n_auxs) = conved_args |> List.split in

       Debug.printf "ARRAY statically_constructable = %b\n" statically_constructable;

       let array_ty =
         get_builtin_array_type elem_ty
                                (List.length elems)
                                default_ty_attr
                                ctx
       in
       assert_valid_type array_ty;
       let n_array = TAst.{kind = ArrayLit (n_nodes, statically_constructable, array_ty); loc} in
       (* TODO: FIX *)
       let sub_nest_lt = (Lifetime.LtSlNormal sub_nest) in
       let lt = SubExprSpec.make_scope_lifetime temp_obj_spec sub_nest_lt in
       let aux = Aux.make array_ty VCatPrValue lt Meta_level.Meta in
       (n_array, aux)
     end

  | Ast.ScopeExpr block ->
     begin
       let scope_env =
         Env.create_scoped_env parent_env (Id_string.Pure "<scope>")
                               (Env.Scope (Env.empty_lookup_table ()))
                               loc
       in
       let lt_env = LifetimeEnv.init () in
       let (nblock, aux, _, _) = analyze_t block scope_env lt_env ctx in

       (**)
       let df_nodes = Env.get_callee_funcs_when_scope_exit scope_env in
       let n_node = TAst.{kind = FinalyzeExpr (Some nblock, df_nodes); loc} in
       let node = TAst.{kind = ScopeExpr n_node; loc} in

       (* propagete *)
       Env.overlap_closed_info scope_env.Env.closed parent_env;

       (node, aux)
     end

  (* TODO: implement dtor *)
  | Ast.IfExpr (cond_expr, then_expr, opt_else_expr, _) ->
     begin
       (* base scope for if expr *)
       let scope_env =
         Env.create_scoped_env parent_env (Id_string.Pure "<if_scope>")
                               (Env.Scope (Env.empty_lookup_table ()))
                               loc
       in

       let (n_cond_expr, cond_aux) =
         analyze_expr ~sub_nest:sub_nest (* TODO: *)
                      cond_expr scope_env temp_obj_spec ctx attr
       in

       let bool_ty = Sema_builtin.get_builtin_bool_type default_ty_attr ctx in
       let (conved_cond_node, conved_auxs) =
         adjust_expr_for_type bool_ty n_cond_expr cond_aux
                              scope_env temp_obj_spec ctx attr
       in

       (**)
       let analayze_clause node =
         let clause_scope_env =
           Env.create_scoped_env scope_env (Id_string.Pure "<if_scope>")
                                 (Env.Scope (Env.empty_lookup_table ()))
                                 None
         in
         let (expr, aux) =
           analyze_expr ~sub_nest:sub_nest (* TODO: *)
                        node clause_scope_env temp_obj_spec ctx attr
         in
         (expr, aux, Env.is_closed clause_scope_env)
       in

       (* then clause *)
       let (nthen_expr, then_aux, then_closed) = analayze_clause then_expr in

       (* else clause *)
       let (opt_else_expr, else_aux, else_closed) = match opt_else_expr with
         | Some else_expr ->
            let (e, aux, is_closed) = analayze_clause else_expr in
            (Some e, aux, is_closed)
         | None -> (None, void_t, false) (* assume NOT closed flow *)
       in

       let then_ty = Aux.ty then_aux in
       let else_ty = Aux.ty else_aux in
       if not (Type.has_same_class then_ty else_ty) then
         failwith @@ "[ERR] type of if expr " ^ (Type.to_string then_ty) ^ " / " ^ (Type.to_string else_ty) ^ " / loc: " ^ (Loc.to_string loc);

       Env.overlap_closed_info (then_closed && else_closed) scope_env;
       Env.overlap_closed_info (Env.is_closed scope_env) parent_env;

       (**)
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

       let if_ty = wrapped_type then_ty else_ty in
       let if_cat = VCatLValue in   (* TODO: fix *)
       let sub_nest_lt = (Lifetime.LtSlNormal sub_nest) in
       let if_mt = SubExprSpec.make_scope_lifetime temp_obj_spec sub_nest_lt in (* TODO: fix *)
       let if_ml = Meta_level.Meta in   (* TODO: fix *)
       let if_aux = Aux.make if_ty if_cat if_mt if_ml in

       let node =
         TAst.{kind = IfExpr (conved_cond_node, nthen_expr, opt_else_expr, if_ty); loc}
       in
       (node, if_aux)
     end

  (* TODO: implement dtor *)
  | Ast.ForExpr (opt_var_decl, opt_cond, opt_step, body) ->
     begin
       let scope_env =
         Env.create_scoped_env parent_env (Id_string.Pure "<for_scope>")
                               (Env.Scope (Env.empty_lookup_table ()))
                               loc
       in
       let lt_env = LifetimeEnv.init () in

       let nopt_var_decl =
         let f var_decl =
           analyze var_decl scope_env lt_env ctx
         in
         Option.map f opt_var_decl
       in

       let nopt_cond =
         let f cond =
           let (nexpr, aux) =
             analyze_expr ~sub_nest:sub_nest (* TODO: *)
                          cond scope_env temp_obj_spec ctx attr
           in
           nexpr
         in
         Option.map f opt_cond
       in

       let nopt_step =
         let f step =
           let (nexpr, aux) =
             analyze_expr ~sub_nest:sub_nest (* TODO: *)
                          step scope_env temp_obj_spec ctx attr
           in
           nexpr
         in
         Option.map f opt_step
       in

       let body_env =
         Env.create_scoped_env scope_env (Id_string.Pure "<for_scope>")
                               (Env.Scope (Env.empty_lookup_table ()))
                               None
       in
       let (nbody, body_aux) =
         analyze_expr ~sub_nest:sub_nest (* TODO: *)
                      body body_env temp_obj_spec ctx attr
       in

       let node =
         TAst.{kind = ForExpr (nopt_var_decl, nopt_cond, nopt_step, nbody); loc}
       in
       (node, void_t)
     end

  | Ast.TypeRVConv (rv, args, _) ->
     begin
       if (List.length args <> 1) then
         error_msg "length of args must be 1";
       let earg =
         evaluate_invocation_args ~sub_nest:(sub_nest+1)
                                  args parent_env temp_obj_spec ctx attr
         |> List.hd
       in
       let (arg_expr, arg_aux) = earg in
       let {
         Aux.ta_type = ty;
         Aux.ta_ml = ml;
       } = arg_aux in

       if (not (Type.has_same_class ty ctx.sc_tsets.ts_type_type)) then
         error_msg "the argument must be type";
       let v = eval_texpr_as_ctfe arg_expr ty ml parent_env ctx attr in
       let ty_val = match v with
         | Ctfe_value.Type ty -> ty
         | _ -> failwith "[ICE]"
       in
       let aux_generics =
         match rv with
         | Type_attr.Val -> []
         | Type_attr.Ref _ ->
            let xs = ty_val.Type_info.ti_aux_generics_args in
            (*assert (List.length xs = 1)*)
            assert (List.length xs <= 1);
            xs
         | _ -> failwith ""
       in
       let _ = aux_generics in
       let {
         Type_info.ti_attr = ty_attr;
       } = ty_val in
       let nty = Type.Generator.update_attr_r ctx.sc_tsets.ts_type_gen
                                              ty_val
                                              { ty_attr with
                                                Type_attr.ta_ref_val = rv;
                                              } in
       let nty = Type.Generator.update_attr_r3 ctx.sc_tsets.ts_type_gen
                                               nty
                                               aux_generics
                                               nty.Type_info.ti_generics_args
       in
       let tnode = TAst.{kind = CtxNode nty; loc} in
       (tnode, arg_aux)
     end

  | Ast.TypeQualConv (qual, args, _) ->
     begin
       if (List.length args <> 1) then
         error_msg "length of args must be 1";
       let earg =
         evaluate_invocation_args ~sub_nest:(sub_nest+1)
                                  args parent_env temp_obj_spec ctx attr
         |> List.hd
       in
       let (arg_expr, arg_aux) = earg in
       let {
         Aux.ta_type = ty;
         Aux.ta_ml = ml;
       } = arg_aux in

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

       let tnode = TAst.{kind = CtxNode nty; loc} in
       (tnode, arg_aux)
     end

  | _ ->
     begin
       Ast.print node;
       failwith "analyze_expr: unsupported node"
     end

and analyze_boot_expr node parent_env ctx attr : 'ty Ctfe_value.t =
  match Ast.kind_of node with
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

and analyze_operator ~sub_nest
                     op_id args loc parent_env temp_obj_spec ctx attr =
  let eargs =
    evaluate_invocation_args ~sub_nest:(sub_nest+1)
                             args parent_env temp_obj_spec ctx attr
  in
  (*let (arg_exprs, arg_auxs) = List.split eargs in
  (*List.iter check_is_args_valid args_types_cats;*)

  let (_, _, _, arg_metalevels, _) = split_aux arg_auxs in
  let args_bottom_ml = Meta_level.calc_bottom arg_metalevels in*)

  (*List.iter print_type args_types;*)
  let opt_fs_and_args =
    find_suitable_operator ~universal_search:true
                           op_id eargs loc parent_env ctx attr in
  let open Result.Monad in
  let res =
    opt_fs_and_args >>=
      (fun f -> make_call_instruction ~sub_nest:sub_nest
                                      f loc None parent_env temp_obj_spec ctx)
  in
  (res, eargs)

and is_all_generics_bound m generis_params =
  ()

and make_call_instruction ?(sub_nest=0)
                          ?(generics_args=[])
                          ?(mm=Sema_lifetime.LifetimeMap.empty)
                          (f_env, conv_filters, eargs)
                          loc opt_sto parent_env temp_obj_spec ctx =
  [%Loga.debug "= make_call_instruction >>>>> %s"
               (Id_string.to_string (Env.get_name f_env))];

  let n_eargs =
    map_conversions ~param_passing:true
                    conv_filters eargs parent_env temp_obj_spec ctx
  in
  let (n_earg_exprs, _) = n_eargs |> List.split in

  let f_er = Env.FunctionOp.get_record f_env in

  let f pk =
    match pk with
    | Env.FnParamKindType ty ->
       Debug.printf "  = param type : %s\n" (Type.to_string ty)
  in
  List.iter f ((Env.FunctionOp.get_record f_env).Env.fn_param_kinds);
  let f ty =
    Debug.printf "  = return type : %s\n" (Type.to_string ty)
  in
  f ((Env.FunctionOp.get_record f_env).Env.fn_return_type);

  (* make generics rel map *)

  (* TODO: check all generics values are bound
   * if explicit parameters are not filled, treat as error
   * if implicit parameters are not filled, treat as ICE
   *)

  (* bind explicit lifetime args to explicit params
   * if the number of arguments is larger then the number of params, it becomes error
   *)
  let mm =
    Sema_lifetime.map_generics_args ~m:mm f_er.Env.fn_generics_vals generics_args
  in
  let mm =
    try
      Sema_lifetime.lifetime_map_generics_in_params mm f_er.Env.fn_param_kinds n_eargs
    with
    | Sema_lifetime.Lifetime_param_length_is_different (param_ty, arg_ty) ->
       Debug.printf "length of generics params is different: %s / PARAMS: %s / ARGS: %s"
                    (Env.get_name f_env |> Id_string.to_string)
                    (Type.to_string param_ty)
                    (Type.to_string arg_ty);
       failwith "[ERR]"
  in

  (* TODO: check all generics values are bound *)
  let c lt =
    [%Loga.debug "BINDED? -> %s" (Lifetime.to_string lt)];
    let tr = Sema_lifetime.solve_var lt mm in
    [%Loga.debug "BOUND <- %s" (Lifetime.to_string tr)];
  in
  List.iter c f_er.Env.fn_generics_vals;

  let f_ret_ty = f_er.Env.fn_return_type in
  if not (is_valid_type f_ret_ty) then
    error (Error_msg.ReturnTypeIsNotDetermined {loc = loc; env = f_env});

  Debug.printf "= LIFETIME CONSTRAINT = START >>>>> %s"
               (Id_string.to_string (Env.get_name f_env));
  (* lifetime: check constraints *)
  List.iter (Sema_lifetime.lifetime_check_constraint mm) f_env.Env.generics_constraints;
  Debug.printf "= LIFETIME CONSTRAINT = FINISH <<<<< %s"
               (Id_string.to_string (Env.get_name f_env));

  (* check return type lifetime *)

  let f s lt =
    Debug.printf "LIFETIME 3 %s : %s\n" s (Lifetime.to_string lt);
    let tr = Sema_lifetime.solve_var lt mm in
    Debug.printf "-> %s\n" (Lifetime.to_string tr);
    tr
  in
  let aux_gs = List.map (f "aux") f_ret_ty.Type_info.ti_aux_generics_args in
  let gs = List.map (f "gen") f_ret_ty.Type_info.ti_generics_args in
  let f_ret_ty = Type.Generator.update_attr_r3 ctx.sc_tsets.ts_type_gen f_ret_ty aux_gs gs in
  Debug.printf "= LIFETIME RETURN = FINISH <<<<< %s / %s" (Id_string.to_string (Env.get_name f_env)) (Type.to_string f_ret_ty);

  let arg_lts =
    let g param_ty =
      (param_ty.Type_info.ti_generics_args @ param_ty.Type_info.ti_aux_generics_args)
    in
    g f_ret_ty
  in
  List.map (Lifetime.to_string) arg_lts |> String.join ", " |> Debug.printf "PP = %s";
  List.map (Lifetime.to_string) ((aux_gs @ gs)) |> String.join ", " |> Debug.printf "CR = %s";

  let ret_ty_sto =
    Option.default_delayed (fun () -> suitable_storage f_ret_ty ctx) opt_sto
  in
  Debug.printf "storage = %s" (TAst.string_of_stirage ret_ty_sto);

  let node = TAst.{kind = GenericCallExpr (
                              ret_ty_sto,
                              n_earg_exprs,
                              Some parent_env,
                              Some f_env); loc} in

  let f_ret_val_cat = ret_val_category f_ret_ty ctx in
  let aux_lt_count = SubExprSpec.allocate_aux_count temp_obj_spec in
  let f_ret_lt =
    (* TODO: fix
     * set new lifetime only when return values are VALUE *)
    let sub_nest_lt = (Lifetime.LtSlNormal 0) in
    SubExprSpec.make_scope_lifetime ~aux_count:aux_lt_count temp_obj_spec sub_nest_lt
  in

  let f_ml = f_env.Env.meta_level in
  let arg_mls = n_eargs |> List.map (fun (_, aux) -> Aux.ml aux) in
  let bottom_ml = List.fold_left Meta_level.bottom f_ml arg_mls in

  [%Loga.debug "FUNC ML => %s" (Meta_level.to_string f_ml)];
  List.iter (fun ml ->
             [%Loga.debug "ARG ML => %s" (Meta_level.to_string ml)]
            ) arg_mls;
  [%Loga.debug "BTM ML => %s" (Meta_level.to_string bottom_ml)];

  let node_aux = Aux.make f_ret_ty f_ret_val_cat f_ret_lt bottom_ml in
  (* TODO: fix
   * set new lifetime only when return values are VALUE *)
  let n_node =
    SubExprSpec.register_temporary temp_obj_spec aux_lt_count (node, node_aux)
  in

  Ok (n_node, node_aux)


and analyze_inner node parent_env lt_env ctx opt_chain_attr =
  let pre_node = extract_prev_pass_node node in
  analyze ~opt_attr:opt_chain_attr pre_node parent_env lt_env ctx

and pre_analyze_inner node parent_env ctx opt_chain_attr =
  let pre_node = extract_prev_pass_node node in
  solve_forward_refs ~opt_attr:opt_chain_attr pre_node parent_env ctx

and extract_prev_pass_node node : Ast.t =
  match TAst.kind_of node with
  | TAst.PrevPassNode n -> n
  | _ -> failwith "[ICE] not prev node"


and analyze_param f_env param ctx attr =
  let (var_attr, var_name, init_part) = param in

  let (param_kind, aux_lacking_envs, lacking_envs) =
    match init_part with
    (* Ex. :int = 10 *)
    | (Some type_expr, Some defalut_val) ->
       failwith "[ICE] declare_function_params : not implemented / default value of param"

    (* Ex. :int *)
    | (Some type_expr, None) ->
       let (ty, aux_lacking_envs, lacking_envs) =
         resolve_type_with_qual_and_generics_placeholder var_attr type_expr
                                                         f_env ctx attr
       in
       (Env.FnParamKindType ty, aux_lacking_envs, lacking_envs)

    (* Ex. = 10 *)
    | (None, Some defalut_val) ->
       (* type is inferenced from defalut_val *)
       failwith "[ICE] declare_function_params : not implemented / infer type from value"

    | _ ->
       (* TODO: change to exception *)
       failwith "[ICE] type or default value is required"
  in

  let ninit_part = (None, None) in   (* a type node and a default value are no longer necessary *)
  let nparam: TAst.param_init_t = (var_attr, var_name, ninit_part) in
  (nparam, param_kind, aux_lacking_envs, lacking_envs)

and make_parameter_venv f_env param_id_name param_ty ctx : (Id_string.t * 'env) =
  let loc = None in
  let lifetime = match param_ty.Type_info.ti_attr.Type_attr.ta_ref_val with
    | Type_attr.Val ->
       let sub_nest_lt = (Lifetime.LtSlNormal 0) in
       Env.get_scope_lifetime sub_nest_lt f_env
    | Type_attr.Ref _ ->
       begin
         match param_ty.Type_info.ti_aux_generics_args with
         | [lt] -> lt
         | _ -> failwith ""
       end
    | _ -> failwith ""
  in

  let venv_r = {
    Env.var_name = param_id_name;
    Env.var_lifetime = lifetime;
    Env.var_type = param_ty;
    Env.var_detail = Env.VarRecordNormal ();
  } in
  let venv =
    Env.create_context_env ~meta_level:Meta_level.Runtime
                           f_env param_id_name
                           (Env.Variable (venv_r))
                           loc
  in
  Env.update_status venv Env.Complete;
  (param_id_name, venv)

and prepare_params env ?(special_params=[]) params_node ctx attr =
  match Ast.kind_of (extract_prev_pass_node params_node) with
  | Ast.ParamsList ps ->
     declare_function_params env special_params ps ctx attr
  | _ -> failwith "[ICE] check_params / unexpected"

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


and analyze_parameters ?(special_params=[]) params parent_env ctx attr =
  let (nparams, param_kinds, implicit_aux_lt_envs, implicit_lt_envs) =
    let special_param_info_list =
      special_params
      |> List.map (fun (init, ty) -> (init, Env.FnParamKindType ty, [], []))
    in
    let param_info_list =
      (params |> List.map (fun p -> analyze_param parent_env p ctx attr))
    in
    let split (a, b, c, d) (ax, bx, cx, dx) =
      (a::ax, b::bx, c @ cx, d @ dx)
    in
    List.fold_right split (special_param_info_list @ param_info_list) ([], [], [], [])
  in
  (nparams, param_kinds, implicit_aux_lt_envs, implicit_lt_envs)

(**)
and declare_function_params f_env special_params params ctx attr =
  (* analyze parameters *)
  let (nparams, param_kinds, implicit_aux_lt_envs, implicit_lt_envs) =
    let special_param_kinds =
      special_params
      |> List.map (fun (init, ty) -> (init, Env.FnParamKindType ty, [], []))
    in
    let param_info_list =
      special_param_kinds @ (params |> List.map (fun p -> analyze_param f_env p ctx attr))
    in
    let split (a, b, c, d) (ax, bx, cx, dx) =
      (a::ax, b::bx, c @ cx, d @ dx)
    in
    List.fold_right split param_info_list ([], [], [], [])
  in

  (* first, make all of environments.
   * next declare them into the function env *)
  let param_envs =
    let make_env param kind =
      let (_, opt_name, _) = param in
      match kind with
      | Env.FnParamKindType ty ->
         Debug.printf "params -> %s: %s"
                      (Option.map_default Id_string.to_string "<opt>" opt_name)
                      (ty |> Type.to_string);
         ty.Type_info.ti_aux_generics_args
         |> List.iter (fun lt -> Debug.printf "PARAM AUX LT -> %s\n" (Lifetime.to_string lt));
         ty.Type_info.ti_generics_args
         |> List.iter (fun lt -> Debug.printf "PARAM LT -> %s\n" (Lifetime.to_string lt));

         opt_name
         |> Option.map (fun name -> make_parameter_venv f_env name ty ctx)
    in
    let declare_env (id_name, venv) =
      Env.add_inner_env f_env id_name venv |> error_if_env_is_dupped Loc.dummy;
      venv
    in
    List.map2 make_env nparams param_kinds
    |> List.map (Option.map declare_env)
  in

  (* declare lacking envs for lifetimes.
   * aux envs are appened at last *)
  let lacking_envs = implicit_lt_envs @ implicit_aux_lt_envs in
  let () =
    let declare_lt i lt_env =
      let id_name = Id_string.Pure (Printf.sprintf "`__%d" i) in
      Env.add_inner_env f_env id_name lt_env |> error_if_env_is_dupped Loc.dummy;
    in
    List.iteri declare_lt lacking_envs
  in

  let to_lt_from_env env =
    match Env.get_env_record env with
    | Env.LifetimeVariable lt -> lt
    | _ -> failwith ""
  in

  (nparams,
   param_kinds,
   param_envs,
   implicit_aux_lt_envs |> List.map to_lt_from_env,
   implicit_lt_envs |> List.map to_lt_from_env)

and check_function_params f_env param_kinds =
  let check kind =
    match kind with
    | Env.FnParamKindType ty ->
       if not (is_valid_type ty) then
         failwith "[ERR] parameter is not valid"
  in
  List.iter check param_kinds

and collect_temp_objs spec opt_last_v parent_env lt_env ctx =
  let opt_aux_id = match opt_last_v with
    | Some last_v ->
       begin
         let lt = Aux.lt last_v in
         match lt with
         | Lifetime.LtDynamic (_, _, _, n) -> Some n
         | _ -> None
       end
    | None -> None
  in
  let (captured_objs, temp_objs) = SubExprSpec.to_lists spec opt_aux_id in
  let make_dtor_pairs obj_list =
    obj_list
    |> List.map (find_destructor_node parent_env lt_env ctx)
    |> List.filter_map identity
    |> List.split
  in
  (captured_objs |> make_dtor_pairs, temp_objs |> make_dtor_pairs)

and find_destructor_node ext_env lt_env ctx (cache_node_id, node_aux) =
  let ty = Aux.ty node_aux in
  let cenv = Type.as_unique ty in
  let cenv_r = Env.ClassOp.get_record cenv in
  let opt_dtor = cenv_r.Env.cls_dtor in
  match opt_dtor with
  | Some dtor_f_env ->
     let n_node_aux =
       let {
         Aux.ta_type = ty;
         Aux.ta_vcat = cat;
         Aux.ta_lt = lt;
         Aux.ta_ml = ml;
       } = node_aux in
       (* in destructor, full access to 'this' variable is allowed *)
       let n_ty = force_change_type_mut ty Type_attr.Mutable ctx in
       let aux = Aux.make n_ty cat lt ml in
       aux
     in
     let c_node = TAst.{kind = GetCacheExpr cache_node_id; loc = Loc.dummy} in
     let selected =
       find_suitable_functions [dtor_f_env] [(c_node, n_node_aux)] ext_env ctx None
     in
     let fs_and_args = match selected with
       | (Function.MatchLevel.ExactMatch, [fs], _)
       | (Function.MatchLevel.QualConv, [fs], _) -> fs
       | _ -> failwith "[ERR]"
     in
     let (t_ast, _) =
       let temp_obj_spec = SubExprSpec.create ext_env lt_env in
       match make_call_instruction fs_and_args None None ext_env temp_obj_spec ctx with
       (* TODO: check wheater temp_obj_spec is empty *)
       | Ok res -> res
       | Bad err -> failwith ""
     in
     let res = t_ast in
     Some (cache_node_id, res)
  | None -> None


and check_id_is_defined_uniquely env id =
  let res = Env.find_on_env env id in
  match res with
    Some _ -> failwith "same ids are defined"
  | None -> ()


and solve_identifier ?(do_rec_search=true)
                     ?(making_placeholder=false)
                     ?(exclude=[])
                     (id_node: Ast.t) env ctx attr
    : (type_info_t * env_t * Lifetime.t * Meta_level.t * 'v Sema_lifetime.LifetimeMap.t) option * env_t list
  =
  let loc = Ast.loc_of id_node in
  match Ast.kind_of id_node with
  | Ast.Id (name, generics_specs, _) ->
     let generics_args =
       List.map (fun s -> lifetime_spec_to_value s env ctx attr)
                generics_specs
     in
     solve_basic_identifier ~do_rec_search:do_rec_search
                            ~loc:loc
                            ~making_placeholder:making_placeholder
                            ~exclude:exclude
                            name generics_args env ctx attr

  | Ast.InstantiatedId (name, template_args, generics_specs, _) ->
     Debug.printf "$$$$$ Ast.InstantiatedId: %s\n" (Id_string.to_string name);
     let generics_args =
       List.map (fun s -> lifetime_spec_to_value s env ctx attr)
                generics_specs
     in

     let (evaled_t_args, _) =
       template_args
       |> List.map (fun e -> eval_expr_as_ctfe e env ctx attr)
       |> List.split
     in
     solve_basic_identifier ~do_rec_search:do_rec_search
                            ~loc:loc
                            ~template_args:evaled_t_args
                            ~making_placeholder:making_placeholder
                            ~exclude:exclude
                            name generics_args env ctx attr

  | _ -> failwith "unsupported ID type"

and lifetime_spec_to_value spec current_env ctx attr =
  let (opt_aux, hist) = solve_basic_identifier spec [] current_env ctx attr in
  let res = match opt_aux with
    | Some (ty, env, _, _, _) ->
       (* assert ty is lifetime *)
       let v_lt = match Env.get_env_record env with
         | Env.LifetimeVariable lt -> lt
         | _ -> failwith ""
       in
       v_lt
    | None ->
       (* TODO: fix error *)
       error (Error_msg.MemberNotFound (current_env, hist, None))
  in
  res

and solve_basic_identifier ?(do_rec_search=true)
                           ?(loc=Loc.dummy)
                           ?(template_args=[])
                           ?(making_placeholder=false)
                           ?(exclude=[])
                           id_name generics_args search_base_env ctx attr
    : (type_info_t * env_t * Lifetime.t * Meta_level.t *'v Sema_lifetime.LifetimeMap.t) option * env_t list =

  let name = Id_string.to_string id_name in
  Debug.printf "-> finding identifier = %s : rec = %b\n" name do_rec_search;

  let (oenv, search_env_history) =
    if do_rec_search then
      Env.lookup ~exclude:exclude search_base_env id_name
    else
      (Env.find_all_on_env search_base_env id_name, [search_base_env])
  in

  let opt_trg_env =
    match oenv with
    | [] ->
       None
    | envs ->
       let init = (
         Type_info.undef_ty,
         Env.undef (),
         Lifetime.LtUndef,
         Meta_level.Runtime,
         Sema_lifetime.LifetimeMap.empty
       ) in

       let to_detail e =
         env_to_detail e template_args generics_args
                       making_placeholder id_name search_base_env ctx attr
       in
       Some (List.fold_left (fun acc e -> merge_envs acc (to_detail e) ctx) init envs)
  in
  (opt_trg_env, search_env_history)

and merge_envs p_d d ctx =
  let (p_ty, p_env, p_lt, p_ml, p_lt_map) = p_d in
  let (ty, env, lt, ml, lt_map) = d in
  match (Type.type_sort p_ty, Type.type_sort ty) with
  | (Type_info.Undef, _) ->
     (* if prev detail is initial value, ignore it *)
     d
  | (Type_info.FunctionSetTy p_m_envs, Type_info.FunctionSetTy m_envs) ->
     (* overload set *)
     let n_ty_sort = Type_info.FunctionSetTy (p_m_envs @ m_envs) in
     let n_ty = Type.Generator.update_sort ctx.sc_tsets.ts_type_gen ty n_ty_sort in
     (n_ty, env, lt, ml, lt_map)
  | _ ->
     Debug.printf "primary: %s" (Type.to_string p_ty);
     Debug.printf "new    : %s" (Type.to_string ty);
     failwith "unsupported"

and env_to_detail env template_args generics_args making_placeholder name cur_env ctx attr
    : (type_info_t * 'env * Lifetime.t * Meta_level.t * 'v Sema_lifetime.LifetimeMap.t) =

  (* type of type *)
  let type_ty = ctx.sc_tsets.ts_type_type in
  let ty_cenv = Type.as_unique type_ty in

  (* Class is a value of type, thus returns "type" of type, and corresponding env.
   * Ex, "int" -> { type: type, value: int }
   *)
  let single_type_id_node name cenv =
    assume_env_is_checked cenv ctx;

    Debug.printf "= LIFETIME ASSIGN basic assign = START >>>>>\n\n";
    let cenv_r = Env.ClassOp.get_record cenv in
    let generics_params = cenv_r.Env.cls_generics_vals in

    let lt_map = Sema_lifetime.map_generics_args generics_params generics_args in
    Debug.printf "= LIFETIME ASSIGN basic assign = FINISH <<<<<\n\n";
    Debug.printf "CLASS GEN =>=>=> %d\n" (List.length generics_args);

    (type_ty, cenv, Lifetime.LtStatic, ty_cenv.Env.meta_level, lt_map)
  in

  let { Env.er = env_r } = env in
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
                                                generics_args
                                                Type_attr.undef
                 in
                 (ty, env, Lifetime.LtStatic, ty_cenv.Env.meta_level, Sema_lifetime.LifetimeMap.empty)
              | xs ->
                 if making_placeholder then
                   begin
                     (* TODO: fix it *)
                     let uni_id =
                       Unification.generate_uni_id ctx.sc_unification_ctx in

                     (* set type as 'type' to uni_id *)
                     Unification.update_type ctx.sc_unification_ctx
                                             uni_id ctx.sc_tsets.ts_type_type;

                     (* set value to uni_id *)
                     let ty =
                       Type.Generator.generate_type ctx.sc_tsets.ts_type_gen
                                                    (Type_info.ClassSetTy env)
                                                    template_args (* ... *)
                                                    []            (* TODO *)
                                                    Type_attr.undef
                     in
                     Unification.update_value ctx.sc_unification_ctx
                                              uni_id (Ctfe_value.Type ty);

                     (**)
                     let ty =
                       Type.Generator.generate_type ctx.sc_tsets.ts_type_gen
                                                    (Type_info.NotDetermined uni_id)
                                                    template_args (* ... *)
                                                    []            (* TODO *)
                                                    Type_attr.undef
                     in
                     (ty, env, Lifetime.LtStatic, ty_cenv.Env.meta_level, Sema_lifetime.LifetimeMap.empty)
                   end
                 else
                   begin
                     let (instances, _) =
                       instantiate_class_templates env xs
                                                   cur_env ctx attr
                     in
                     match instances with
                     | [single_cenv] -> single_type_id_node name single_cenv
                     | _ -> failwith "[ERR] ambiguous definitions"
                   end

            else begin
              if (List.length template_args <> 0) then
                failwith "[ERR] there is no template class";

              match record.Env.ms_normal_instances with
              | [single_cenv] ->
                 single_type_id_node name single_cenv
              | _ -> failwith "[ICE] unexpected : class / multi-set"
              end
          end

       | Env.Kind.Function meta_level ->
          (* functions will be overloaded *)
          Debug.printf "FUNC GEN =>=>=> %d\n" (List.length generics_args);
          let ty =
            Type.Generator.generate_type ctx.sc_tsets.ts_type_gen
                                         (Type_info.FunctionSetTy [env])
                                         template_args
                                         generics_args
                                         {
                                           Type_attr.ta_mut = Type_attr.Immutable;
                                           Type_attr.ta_ref_val = Type_attr.Ref [];
                                         }
          in
          (ty, Env.undef (), Lifetime.LtStatic, meta_level, Sema_lifetime.LifetimeMap.empty)

       | _ -> failwith "unexpected env : multi-set kind"
     end

  (* only builtin classes may be matched *)
  | Env.Class (_) -> single_type_id_node name env

  | Env.Variable (vr) ->
     begin
       assume_env_is_checked env ctx;
       let {
         Env.var_type = var_ty;
         Env.var_lifetime = var_lt;
       } = vr in
       (* TODO: check class variable *)

       (var_ty, env, var_lt, env.Env.meta_level, Sema_lifetime.LifetimeMap.empty)
     end

  (* returns **type** of MetaVariable, NOT value *)
  | Env.MetaVariable (uni_id) ->
     begin
       let (term_uni_id, c) =
         Unification.search_type_until_terminal ctx.sc_unification_ctx uni_id
       in
       match c with
       | (Unification.Val ty) ->
          (ty, env, Lifetime.LtStatic, env.Env.meta_level, Sema_lifetime.LifetimeMap.empty)
       | (Unification.Undef) ->
          let ty =
            Type.Generator.generate_type ctx.sc_tsets.ts_type_gen
                                         (Type_info.NotDetermined uni_id)
                                         template_args
                                         generics_args
                                         Type_attr.undef
          in
          (ty, env, Lifetime.LtStatic, env.Env.meta_level, Sema_lifetime.LifetimeMap.empty)
       | _ -> failwith "[ICE] meta ver"
     end

  | Env.LifetimeVariable _ ->
     let ty = type_ty in (* TODO: change to lifetime_ty *)
     (ty, env, Lifetime.LtStatic, Meta_level.OnlyMeta, Sema_lifetime.LifetimeMap.empty)

  | _ -> failwith "solve_simple_identifier: unexpected env"




and make_not_determined_type uni_id ctx =
  Type.Generator.generate_type ctx.sc_tsets.ts_type_gen
                               (Type_info.NotDetermined uni_id)
                               [] (* TODO: currenyly, meta var has no template args. support template template parameters *)
                               []   (* TODO *)
                               Type_attr.undef

and normalize_mata_var_as_type uni_id ctx =
  let (term_uni_id, c) =
    Unification.search_type_until_terminal ctx.sc_unification_ctx uni_id
  in
  match c with
  | (Unification.Val ty) -> ty
  | (Unification.Undef) -> make_not_determined_type uni_id ctx
  | _ -> failwith "[ICE] meta ver"


and construct_env_with_check ~checker env ctx =
  (*Debug.printf "CHECKED? %s -> %b\n" (Env.get_name env |> Id_string.to_string) (checker env);*)
  if not (checker env) then
    match env.Env.rel_node with
    | Some (node) ->
       begin
         let parent_env = Option.get env.Env.parent_env in
         let lt_env = LifetimeEnv.init () in (* TODO: *)
         ignore @@ construct_env node parent_env lt_env ctx None;
         if not (checker env) then
           failwith "? recursice definition is appeared"; (* TODO: exception *)
         ()
       end
    | None -> failwith "[ICE] try to complete env / there is no rel node"
  else
    ()  (* DO NOTHING *)

and assume_env_is_completed env ctx =
  (*Debug.printf "IS_NOT_COMPLETE? %s -> %b\n" (Env.get_name env |> Id_string.to_string) (Env.is_complete env);*)
  construct_env_with_check ~checker:Env.is_complete env ctx

and assume_env_is_checked env ctx =
  (*Debug.printf "IS_NOT_COMPLETE? %s -> %b\n" (Env.get_name env |> Id_string.to_string) (Env.is_complete env);*)
  construct_env_with_check ~checker:Env.is_checked env ctx

and convert_type trg_ty src_arg ext_env ctx attr =
  let (_, src_aux) = src_arg in
  let {
    Aux.ta_type = src_ty;
    Aux.ta_vcat = src_val_cat;
    Aux.ta_lt = src_lt;
    Aux.ta_ml = src_ml;
  } = src_aux in
  Debug.printf "TRY convert_type from %s to %s\n"
               (Type.to_string src_ty)
               (Type.to_string trg_ty);

  if is_type_convertible_to src_ty trg_ty then begin
    (* same type *)
    let open Type_attr in
    match (trg_ty.Type_info.ti_attr, src_ty.Type_info.ti_attr) with
    (* val <- ref *)
    | ({ta_ref_val = Val}, {ta_ref_val = _}) ->
       begin
         (* copy val/ref to value *)
         let cenv = Type.as_unique trg_ty in
         let (res, _) = solve_basic_identifier ~do_rec_search:false
                                               ctor_id_name [] cenv ctx attr in
         let (ctor_ty, _, _, _, _) = match res with
           | Some v -> v
           | None -> failwith "[ERR] constructor not found"
         in
         let ctor_env = match Type.type_sort ctor_ty with
           | Type_info.FunctionSetTy [e] -> e
           | Type_info.FunctionSetTy _ -> failwith "[ERR] ctor must be defined in same module"
           | _ -> failwith "[ERR] constructor must be function"
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
            * because 'val' will cause infinite loop of to call "convert_type" to treat copy/move ctor
            *)
           let funcs = List.filter_map pred ctor_fenvs in
           Debug.printf "=> number of funcs = %d / %d\n"
                        (List.length funcs)
                        (List.length ctor_fenvs);
           let selected = find_suitable_functions funcs [src_arg] ext_env ctx attr in
           let fns = match selected with
             | (Function.MatchLevel.ExactMatch, fs, _)
             | (Function.MatchLevel.QualConv, fs, _) -> fs
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
                | 1 ->
                   (* the move ctor is found *)
                   List.hd mv_funcs
                | 0 ->
                   (* there is no move ctor. find copy ctor instead. *)
                   begin
                     (* copy ctors *)
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
         let {
             Type_info.ti_generics_args = generics_args;
           } = src_ty in
         Debug.printf "conv ty -> %s\n" (Type.to_string trg_ty);
         (Function.MatchLevel.ExactMatch, Function.ConvFunc (trg_ty, f, generics_args))
       end

    (* ref <- ref *)
    | ({ta_ref_val = Ref _; ta_mut = trg_mut},
       {ta_ref_val = Ref _; ta_mut = src_mut}) ->
       begin
         let level = match (trg_mut, src_mut) with
           | (Immutable, Immutable) -> Function.MatchLevel.ExactMatch
           | (Immutable, Const)
           | (Immutable, Mutable) -> Function.MatchLevel.NoMatch

           | (Const, Immutable) -> Function.MatchLevel.QualConv
           | (Const, Const) -> Function.MatchLevel.ExactMatch
           | (Const, Mutable) -> Function.MatchLevel.QualConv

           | (Mutable, Immutable)
           | (Mutable, Const) -> Function.MatchLevel.NoMatch
           | (Mutable, Mutable) -> Function.MatchLevel.ExactMatch

           | _ -> failwith "conv"
         in
         (level, Function.Trans trg_ty)
       end

    (* ref <- val *)
    | ({ta_ref_val = Ref _; ta_mut = trg_mut},
       {ta_ref_val = Val; ta_mut = src_mut}) ->
       begin
         let level = match (trg_mut, src_mut) with
           | (Immutable, Immutable) -> Function.MatchLevel.ExactMatch
           | (Immutable, Const)
           | (Immutable, Mutable) -> Function.MatchLevel.NoMatch

           | (Const, Immutable) -> Function.MatchLevel.QualConv
           | (Const, Const) -> Function.MatchLevel.ExactMatch
           | (Const, Mutable) -> Function.MatchLevel.QualConv

           | (Mutable, Immutable)
           | (Mutable, Const) -> Function.MatchLevel.NoMatch
           | (Mutable, Mutable) -> Function.MatchLevel.ExactMatch

           | _ -> failwith "conv"
         in
         (level, Function.Trans trg_ty)
       end

    | _ ->
       failwith "[ICE]"

  end else begin
    (* TODO: implement type conversion *)
    (Function.MatchLevel.NoMatch, Function.Trans trg_ty)
  end

(* TODO: implement implicit type convertion *)
and is_type_convertible_to src_ty trg_ty =
  if Type.has_same_class src_ty trg_ty then
    (* check generics args *)
    true
  else
    false

and determine_function_return_type opt_ret_type
                                   explicit_param_lts implicit_lts implicit_aux_lts
                                   env ctx opt_attr =
  match opt_ret_type with
  (* return type is specified explicitly *)
  | Some (TAst.{kind = PrevPassNode ret_ty_expr}) ->
     let candidate_lts =
       (match explicit_param_lts with
        | [] -> (match implicit_lts with
                 | [] -> implicit_aux_lts
                 | ls -> ls)
        | ls -> ls)
     in

     let ret_ty =
       resolve_type ret_ty_expr env ctx opt_attr
     in
     (* aux ref elision *)
     (* TODO: implement *)
     let new_aux_lt_args = ret_ty.Type_info.ti_aux_generics_args in

     (* lt params elision *)
     let new_lt_args =
       match (candidate_lts, ret_ty.Type_info.ti_generics_args) with
       | ([param_lt], [ty_lt]) when Lifetime.is_undef ty_lt ->
          [param_lt]
       | (_, ty_lt_args) ->
          ty_lt_args
     in

     let new_ret_ty =
       Type.Generator.update_attr_r3 ctx.sc_tsets.ts_type_gen ret_ty
                                     new_aux_lt_args
                                     new_lt_args
     in
     raise_error_if_type_is_invalid new_ret_ty;

     (new_ret_ty, false)

  (* return type is not specified *)
  | None ->
     (* needs return type inference *)
     (Type_info.undef_ty, true)

  | _ -> failwith "[ICE]"

and check_and_insert_suitable_return ?(is_special_func=false)
                                     nbody env ctx opt_attr =
  let fr = Env.FunctionOp.get_record env in
  if not env.Env.closed || is_special_func then
    let void_ty = get_builtin_void_type default_ty_attr ctx in
    let ret_ty = fr.Env.fn_return_type in
    if Type.has_same_class ret_ty (void_ty) || is_special_func then
      begin
        (* insert an empty return statement *)
        match TAst.kind_of nbody with
        | TAst.StatementList stmts ->
           let lt_env = LifetimeEnv.init () in
           let empty_ret = TAst.{kind = ReturnStmt None; loc = Loc.dummy} in
           let (n_node, _, _, _) = construct_env empty_ret env lt_env ctx opt_attr in
           let ret_node =
             TAst.{kind = StatementList (stmts @ [n_node]);
                   loc = loc_of nbody}
           in
           Env.overlap_closed_info true env;
           ret_node
        | _ ->
           failwith "[ICE]"
      end
    else
      error (Error_msg.NoReturnStmts {loc = TAst.loc_of nbody; env});
  else
    nbody

and suitable_storage' ~exit_scope
                      ~param_passing
                      trg_ty ctx =
  assert (not (exit_scope && param_passing));
  let cenv = Type.as_unique trg_ty in
  let cr = Env.ClassOp.get_record cenv in

  let {
    Type_attr.ta_mut = mut;
    Type_attr.ta_ref_val = rv;
  } = trg_ty.Type_info.ti_attr in
  let trg_ref_ty =
    Type.Generator.update_attr_r ctx.sc_tsets.ts_type_gen trg_ty
                                 { trg_ty.Type_info.ti_attr with
                                   Type_attr.ta_ref_val = Type_attr.Ref []
                                 }
  in

  if cr.Env.cls_traits.Env.cls_traits_is_primitive then
    begin
      match rv with
      | Type_attr.Ref _ ->
         (*if param_passing then
           TAst.StoStack trg_ref_ty
         else*)
           TAst.StoImm
      | Type_attr.Val when param_passing -> TAst.StoImm
      | _ ->
         begin
           match mut with
           | Type_attr.Immutable
           | Type_attr.Const -> TAst.StoImm
           | Type_attr.Mutable -> TAst.StoStack trg_ref_ty
           | _ -> failwith "[ICE]"
         end
    end
  else
    begin
      match rv with
      | Type_attr.Ref _ -> TAst.StoImm
      | _ ->
         if exit_scope then
           TAst.StoAgg trg_ref_ty
         else
           TAst.StoStack trg_ref_ty
    end

and suitable_storage ?(opt_operation=None)
                     trg_ty ctx =
  match opt_operation with
  | Some operation ->
     Debug.printf "suitable_storage: %s" (string_of_storage_operation operation);
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
       | SoBind ->
          TAst.StoStack trg_ty
       | SoArrayElement index ->
          TAst.StoArrayElem (trg_ty, index)
     end
  | None ->
     Debug.printf "suitable_storage: no_operation";
     suitable_storage' ~exit_scope:false
                       ~param_passing:false
                       trg_ty ctx

and apply_conv_filter ?(opt_operation=None)
                      filter expr ext_env temp_obj_spec ctx =
  let (expr_node, expr_aux) = expr in
  match filter with
  | Function.ConvFunc (trg_ty, f_env, generics_args) ->
     let sto =
       suitable_storage ~opt_operation:opt_operation
                        trg_ty ctx
     in
     let expr_ty = Aux.ty expr_aux in
     let v =
       let recv_ty_cenv = Type.as_unique trg_ty in
       let recv_ty_cenv_er = Env.ClassOp.get_record recv_ty_cenv in
       let recv_ty_cenv_generics_params = recv_ty_cenv_er.Env.cls_generics_vals in
       let cls_generics_map =
         Sema_lifetime.map_generics_args recv_ty_cenv_generics_params generics_args
       in
       let call_inst =
         make_call_instruction ~generics_args:generics_args
                               ~mm:cls_generics_map
                               (f_env, [Function.Trans expr_ty], [expr])
                               None (Some sto) ext_env temp_obj_spec ctx
       in
       match call_inst with
       | Ok v -> v
       | Bad e -> failwith ""
     in
     v

  | Function.Trans (trg_ty) ->
     let sto =
       suitable_storage ~opt_operation:opt_operation
                        trg_ty ctx
     in
     let nexpr = match sto with
       | TAst.StoImm -> expr_node
       | _ -> expr_node |> TAst.map (fun _ -> TAst.StorageWrapperExpr (ref sto, expr_node))
     in
     (nexpr, expr_aux)


and adjust_expr_for_type ?(action=None)
                         trg_ty src_expr src_aux ext_env temp_obj_spec ctx attr =
  Debug.printf "adjust_expr_for_type -> %s\n" (Type.to_string trg_ty);
  let (match_level, m_filter) =
    convert_type trg_ty (src_expr, src_aux) ext_env ctx attr
  in
  if match_level = Function.MatchLevel.NoMatch then
    Sema_error.error (Error_msg.ConvErr (trg_ty, (Aux.ty src_aux, TAst.loc_of src_expr), ext_env));

  apply_conv_filter ~opt_operation:action
                    m_filter (src_expr, src_aux) ext_env temp_obj_spec ctx


and resolve_type_with_qual ?(making_placeholder=false)
                           ty_attr (expr:Ast.t) env ctx attr : type_info_t =
  (* check generics parameters for ref *)
  let aux_lt_spec = match ty_attr.Type_attr.ta_ref_val with
    | Type_attr.Ref generics_params ->
       begin
         match generics_params with
         | [] ->
            Some (Lifetime.LtUndef)
         | [param_name] ->
            Some (lifetime_spec_to_value param_name env ctx attr)
         | _ ->
            failwith "[ERR]"
       end
    | Type_attr.Val ->
       None
    | _ ->
       failwith "[ICE]"
  in
  let aux_lt_specs = [aux_lt_spec] |> List.filter_map identity in

  (**)
  let ty = resolve_type ~making_placeholder:making_placeholder expr env ctx attr in
  Type.Generator.update_attr_r2 ctx.sc_tsets.ts_type_gen ty ty_attr aux_lt_specs

and resolve_type_with_qual_and_generics_placeholder ?(making_placeholder=false)
                                                    ty_attr expr env ctx attr =
  let ty =
    resolve_type_with_qual ~making_placeholder:making_placeholder
                           ty_attr expr env ctx attr
  in
  let f ty_lt (n_ty_lts, lacking_envs) =
    match ty_lt with
    | Lifetime.LtUndef ->
       (* make new generics params *)
       let (lt_env, lt) = create_generics_spec (Id_string.Pure "`_") env in
       (lt :: n_ty_lts, lt_env :: lacking_envs)
    | _ ->
       (ty_lt :: n_ty_lts, lacking_envs)
  in
  let (aux_gs, aux_lacking_envs) =
    List.fold_right f ty.Type_info.ti_aux_generics_args ([], [])
  in
  let (gs, lacking_envs) =
    List.fold_right f ty.Type_info.ti_generics_args ([], [])
  in
  let ty = Type.Generator.update_attr_r3 ctx.sc_tsets.ts_type_gen ty aux_gs gs in
  (ty, aux_lacking_envs, lacking_envs)


and resolve_type ?(making_placeholder=false) (expr:Ast.t) env ctx attr : type_info_t =
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


and eval_earg_as_ctfe ?(making_placeholder=false) earg env ctx attr =
  let (nexpr, naux) = earg in
  let {
    Aux.ta_type = ty;
    Aux.ta_ml = ml;
  } = naux in
  eval_texpr_as_ctfe ~making_placeholder:making_placeholder
                     nexpr ty ml
                     env ctx attr

and eval_expr_as_ctfe ?(making_placeholder=false) expr env ctx attr =
  let lt_env = LifetimeEnv.init () in
  let sub_expr_spec = SubExprSpec.create env lt_env in
  let earg =
    analyze_expr ~making_placeholder:making_placeholder
                 ~sub_nest:0 (* TODO: *)
                 expr env sub_expr_spec ctx attr
  in
  let v =
    eval_earg_as_ctfe ~making_placeholder:making_placeholder
                      earg env ctx attr
  in
  let (nexpr, _naux) = earg in
  (v, nexpr)

and eval_texpr_as_ctfe ?(making_placeholder=false)
                       texpr expr_ty expr_ml
                       env ctx attr =
  let () = match expr_ml with
    | Meta_level.OnlyRuntime
    | Meta_level.Runtime ->
       error (Error_msg.DiffExecLevel {loc = TAst.loc_of texpr;
                                       expect = Meta_level.Meta;
                                       actual = expr_ml})
    | _ -> ()
  in

  let ctfe_val_and_is_addr =
    match Type.type_sort expr_ty with
    | Type_info.UniqueTy _ ->
       Ctfe_engine.execute ctx.sc_ctfe_engine texpr expr_ty ctx.sc_tsets

    | Type_info.NotDetermined _ ->
       Ctfe_value.Type expr_ty

    | _ -> failwith (Printf.sprintf "[ICE] eval_expr_as_ctfe : couldn't resolve / %s"
                                    (Type.to_string expr_ty))
  in

  ctfe_val_and_is_addr


and tnode_of_ctfe_val ctfe_val ctx =
  let loc = Loc.dummy in
  match ctfe_val with
  | Ctfe_value.Int32 v ->
     TAst.{kind = IntLit (Int32.to_int v, 32, true, type_of_ctfe_val ctx ctfe_val); loc}
  | Ctfe_value.Uint32 v ->
     TAst.{kind = IntLit (Uint32.to_int v, 32, false, type_of_ctfe_val ctx ctfe_val); loc}
  | Ctfe_value.Type ty ->
     TAst.{kind = CtxNode ty; loc}
  | _ ->
     failwith (Printf.sprintf "[ICE] tnode_of_ctfe_val: unsupported ctfe value %s"
                              (Type.to_s_ctfe ctfe_val))

and type_of_ctfe_val ctx v =
  match v with
  | Ctfe_value.Type _ ->
     ctx.sc_tsets.ts_type_type
  | Ctfe_value.Int32 _ ->
     Sema_builtin.get_builtin_int32_type default_ty_attr ctx
  | Ctfe_value.Uint32 _ ->
     Sema_builtin.get_builtin_uint32_type default_ty_attr ctx
  | _ ->
     failwith "[ICE]"

and evaluate_invocation_args ~sub_nest args env sss ctx attr =
  args |> List.map (fun n -> evaluate_invocation_arg ~sub_nest:sub_nest n env sss ctx attr)

and evaluate_invocation_arg ~sub_nest expr env sss ctx attr =
  (* TODO: check CTFE-able node *)
  analyze_expr ~sub_nest:sub_nest expr env sss ctx attr


and find_suitable_operator ?(universal_search=false)
                           op_name_id eargs loc env ctx attr =
  let callee_func_info =
    assert (List.length eargs > 0);
    let (_, lhs_arg_aux) = List.hd eargs in
    let lhs_arg_ty = Aux.ty lhs_arg_aux in
    let (opt_callee_function_info, hist) =
      select_member_element ~universal_search:universal_search
                            lhs_arg_aux op_name_id env ctx attr
    in
    match opt_callee_function_info with
    | Some v -> Ok v
    | None -> Bad (Error_msg.MemberNotFound (Type.as_unique lhs_arg_ty, hist, loc))
  in
  let check_type_and_solve_overload (callee_f_ty, callee_f_env, _, ml, _) =
    let {
      Type_info.ti_sort = ty_sort;
      Type_info.ti_template_args = template_args;
    } = callee_f_ty in
    match ty_sort with
    | Type_info.FunctionSetTy m_envs ->
       begin
         match solve_function_overload ml eargs template_args m_envs env loc ctx attr with
         | v -> Ok v
         | exception (Normal_error v) -> Bad v
       end
    | _ -> failwith "[ICE]: operator must be defined as function"
  in
  let open Result.Monad in
  callee_func_info >>= check_type_and_solve_overload


(* returns Env of function,
 * this function raises an exception *)
and solve_function_overload ml eargs template_args m_env_set ext_env loc ctx attr =
  assert (List.length m_env_set > 0);

  (* solve overload for each overload set *)
  let (envs, errs) =
    let solve (envs, errs) m_env =
      try
        let e =
          solve_function_overload' ml eargs template_args m_env ext_env loc ctx attr
        in
        (e :: envs, errs)
      with
      | Normal_error err ->
         (envs, err :: errs)
    in
    let (vs, es) = List.fold_left solve ([], []) m_env_set in
    (List.rev vs, List.rev es)
  in

  match envs with
  | [e] ->
     (* found an only one result in the overload set *)
     e

  | [] ->
     (* ERROR: all overload resolutions in the overload set are failed *)
     error (Error_msg.NoOverloadSet (errs, loc))

  | _ ->
     (* ERROR: there are many results in the overload set. *)
     (* TODO: make suitable errors *)
     failwith "[ERR] there are many results in the overload set"

and solve_function_overload' ml eargs template_args mset_env ext_env loc ctx attr =
  [%Loga.debug "MetaLevel = %s, Name = %s"
               (Meta_level.to_string ml)
               (Id_string.to_string mset_env.Env.name)
  ];
  let mset_record = match mset_env.Env.er with
    | Env.MultiSet r -> r
    | _ -> Env.debug_print mset_env;
           failwith "[ICE] solve_function_overload : Only Multiset is accepted"
  in
  let () =
    match mset_record.Env.ms_kind with
    | Env.Kind.Function _ ->
       ()
    | _ ->
       failwith "[ICE] solve_function_overload : sort of menv must be function."
  in

  let (args, arg_auxs) = List.split eargs in
  let (f_level, fs_and_args, errs) = match template_args with
    (* has no template args *)
    | [] ->
       begin
         (* if candidate contains onlymeta function, try to search functions from templates first *)
         let (pre_f_level, pre_fs_and_args, errs_n) = match ml with
           | Meta_level.OnlyMeta ->
              assert (List.length template_args = 0);
              let ctfeable =
                List.for_all (fun earg ->
                              let (_ast, aux) = earg in
                              (* TODO: fix *)
                              Meta_level.meta_than_or_equal_to (Aux.ml aux) Meta_level.Meta
                             ) eargs
              in
              List.iter (fun earg ->
                         let (ast, aux) = earg in
                         let loc = TAst.loc_of ast in

                         [%Loga.debug "=> %s (%s)"
                                      (Meta_level.to_string (Aux.ml aux))
                                      (Loc.to_string loc)]
                        ) eargs;
              begin
                match ctfeable with
                | true ->
                   let template_args =
                     eargs |> List.map (fun e -> eval_earg_as_ctfe e ext_env ctx attr)
                   in
                   let eargs = [] in
                   let (args, arg_auxs) = List.split eargs in
                   let (instanced_envs, _) =
                     instantiate_function_templates mset_env template_args arg_auxs
                                                    ext_env ctx attr
                   in
                   Debug.printf "%%%%%%%%%%%%%%%% instanced_envs(for only meta) -> %d\n"
                                (List.length instanced_envs);
                   find_suitable_functions ~only_meta:true
                                           instanced_envs eargs ext_env ctx attr
                | false ->
                   (Function.MatchLevel.NoMatch, [], [])
              end
           | _ ->
              (Function.MatchLevel.NoMatch, [], [])
         in
         match pre_f_level with
         | Function.MatchLevel.NoMatch ->
            let (normal_f_level, normal_fs_and_args, errs_n) =
              find_suitable_functions mset_record.Env.ms_normal_instances
                                      eargs
                                      ext_env ctx attr
            in
            Debug.printf "!! normal function candidates = %s / %d\n"
                         (Function.MatchLevel.to_string normal_f_level)
                         (List.length normal_fs_and_args);

            begin
              match normal_f_level with
              | Function.MatchLevel.ExactMatch ->
                 (normal_f_level, normal_fs_and_args, [])

              (* template functions might have more suitable ones than normal ones *)
              | _ ->
                 begin
                   let args_type_value =
                     arg_auxs
                     |> List.map Aux.ty
                     |> List.map (fun ty -> Type.to_string ty)
                     |> String.join "||"
                   in

                   let (instanced_envs, errs_e) =
                     let env_cache =
                       Hashtbl.find_option mset_record.Env.ms_instanced_args_pre_caches
                                           args_type_value
                     in
                     match env_cache with
                     | Some envs -> envs
                     | None ->
                        let instantiate_result =
                          instantiate_function_templates mset_env [] arg_auxs ext_env ctx attr
                        in
                        Hashtbl.add mset_record.Env.ms_instanced_args_pre_caches
                                    args_type_value instantiate_result;
                        instantiate_result
                   in
                   let (instanced_f_level, instanced_fs_and_args, errs_i) =
                     find_suitable_functions instanced_envs eargs ext_env ctx attr
                   in
                   Debug.printf "!! instanced function candidates = %s / (%d)%d"
                                (Function.MatchLevel.to_string instanced_f_level)
                                (List.length instanced_envs)
                                (List.length instanced_fs_and_args);
                   if Function.MatchLevel.is_better instanced_f_level normal_f_level then
                     (instanced_f_level, instanced_fs_and_args, errs_n @ errs_i @ errs_e)
                   else
                     (normal_f_level, normal_fs_and_args, errs_n @ errs_i @ errs_e)
                 end
            end
         | _ ->
            (pre_f_level, pre_fs_and_args, errs_n)
       end

    (* has template args *)
    | _ ->
       begin
         let (instanced_envs, _) =
           instantiate_function_templates mset_env template_args arg_auxs
                                          ext_env ctx attr
         in
         Debug.printf "%%%%%%%%%%%%%%%% instanced_envs -> %d\n" (List.length instanced_envs);
         find_suitable_functions instanced_envs eargs ext_env ctx attr
       end
  in

  if f_level = Function.MatchLevel.NoMatch then
    begin
      error (Error_msg.NoMatch (errs, loc));
    end;

  let fs_and_args =
    assert (List.length fs_and_args <> 0);

    let () =
      let f (e, _, _) =
        [%Loga.debug "SPECIALIED_LEVELS -> %s"
                     (Env.FunctionOp.specialized_levels e
                      |> List.map string_of_int |> String.join ", ")];
      in
      List.iter f fs_and_args
    in

    (* specialzed level filtering *)
    let max_levels =
      fs_and_args
      |> List.fold_left (fun lv (e, _, _) ->
                         max lv (Env.FunctionOp.specialized_levels e)
                        ) []
    in
    let fs_and_args =
      fs_and_args
      |> List.filter (fun (e, _, _) -> (Env.FunctionOp.specialized_levels e) = max_levels)
    in

    (* filter has constraints *)
    let f (e, _, _) =
      Env.FunctionOp.has_constraints e
    in
    match List.filter f fs_and_args with
    | [] -> fs_and_args
    | xs -> xs
  in

  let envs = List.map (fun (e, _, _) -> e) fs_and_args in
  if (List.length fs_and_args) > 1 then
    error (Error_msg.Ambiguous (f_level, envs, loc));

  assert (List.length fs_and_args = 1);
  List.hd fs_and_args


and find_suitable_functions ?(only_meta=false)
                            f_candidates
                            args
                            ext_env
                            ctx attr
    : (TAst.t, type_info_t, env_t, error_msg_t) Function.function_info_err =
  Debug.printf "number of candidates = %d\n" (List.length f_candidates);

  let calc_match_level f_env =
    assume_env_is_checked f_env ctx;
    let f_record = Env.FunctionOp.get_record f_env in

    let opt_param_types = adjust_param_types f_record.Env.fn_param_kinds args in
    match opt_param_types with
    | Some param_types ->
       let params_num = List.length param_types in
       let args_num = List.length args in
       if args_num <> params_num then
         let err = Error_msg.DifferentArgNum (params_num, args_num) in
         (Function.MatchLevel.NoMatch, f_env, [], args, EArgConvMap.empty, Some err)
       else
         (* convert types of args *)
         let (match_levels, conv_funcs, convmap) =
           let conv (match_levels, conv_funcs, idx, convmap) trg_ty src_arg =
             let (match_level, f) = convert_type trg_ty src_arg ext_env ctx attr in
             let convmap = EArgConvMap.add idx (trg_ty, src_arg, match_level) convmap in
             (match_level::match_levels, f::conv_funcs, (idx+1), convmap)
           in
           let (match_levels, conv_funcs, _, convmap) =
             List.fold_left2 conv ([], [], 0, EArgConvMap.empty) param_types args
           in
           (match_levels |> List.rev, conv_funcs |> List.rev, convmap)
         in

         (* most unmatch level of parameters becomes function match level *)
         let total_f_level =
           List.fold_left Function.MatchLevel.bottom Function.MatchLevel.ExactMatch
                          match_levels
         in

         let err =
           match EArgConvMap.to_locmap convmap with
           | loc_m when Error_msg.ArgLocMap.cardinal loc_m = 0 ->
              None
           | loc_m ->
              Some (Error_msg.ArgConvErr (loc_m, f_env))
         in
         (total_f_level, f_env, conv_funcs, args, convmap, err)

    | None ->
       let params_num = List.length f_record.Env.fn_param_kinds in
       let args_num = List.length args in
       let err = Error_msg.DifferentArgNum (params_num, args_num) in
       (Function.MatchLevel.NoMatch, f_env, [], args, EArgConvMap.empty, Some err)
  in

  let collect (cur_order, fs_and_args, errs) candidate
      : (TAst.t, type_info_t, env_t, error_msg_t) Function.function_info_err =
    let (total_f_level, f_env, conv_funcs, args, _, err) =
      calc_match_level candidate
    in
    let errs = match err with
      | Some e -> e :: errs
      | None -> errs
    in
    if Function.MatchLevel.is_better total_f_level cur_order then
      (* if more better function is found, remake candidates and raise level *)
      (total_f_level, [(f_env, conv_funcs, args)], errs)
    else if Function.MatchLevel.is_same total_f_level cur_order then
      (* if this function has same match level, add to candicates *)
      (cur_order, (f_env, conv_funcs, args) :: fs_and_args, errs)
    else
      (* ignore(do NOT append) function which has lower match level *)
      (cur_order, fs_and_args, errs)
  in
  let (level, fs_and_args, errs) =
    List.fold_left collect (Function.MatchLevel.NoMatch, [], []) f_candidates in

  if level = Function.MatchLevel.NoMatch then
    (level, [], errs)
  else
    (level, fs_and_args, errs)


and instantiate_function_templates menv template_args arg_auxs ext_env ctx attr =
  let instantiate (t_env_record: TAst.t Env.template_record) =
    let (temp_env, meta_var_names, uni_ids) =
      prepare_instantiate_template t_env_record template_args
                                   ext_env ctx attr
    in

    (* match valuse by arg types *)
    let inner_node = t_env_record.Env.tl_inner_node in
    let inner_node = match TAst.kind_of inner_node with
      | TAst.NotInstantiatedNode (n, _) -> n
      | _ -> failwith "[ICE] unexpected not instantiated node"
    in
    let (lifetime_specs, parameters, opt_cond, dn) =
      match Ast.kind_of inner_node with
      | Ast.FunctionDefStmt (_, lt_specs, Ast.{kind = ParamsList params}, _, _, c, _, _, _) ->
         (lt_specs, params, c, 0)
      | Ast.ExternFunctionDefStmt (_, lt_specs, Ast.{kind = ParamsList params}, _, _, c, _, _, _) ->
         (lt_specs, params, c, 0)
      | Ast.MemberFunctionDefStmt (name, lt_specs, Ast.{kind = ParamsList params}, _, _, _, _, _) ->
         let is_special = match name with
           | Id_string.Pure s when s = ctor_name -> true
           (* TODO: support destructors *)
           | _ -> false
         in
         (lt_specs, params, None, if is_special then 0 else 1)
      | _ -> failwith "[ICE]"
    in

    (* TODO: implement *)
    let (lt_params, _) = declare_generics_specs lifetime_specs temp_env in
    let _ = lt_params in

    let param_types =
      parameters
      |> List.map (fun decl -> get_template_function_param_type decl temp_env ctx attr)
    in

    (* debug print *)
    let () =
      List.iteri (fun i ty -> Debug.printf "func_param; %d: %s" i (Type.to_string ty)) param_types;
      Debug.printf "REACHED / get_function_param_types";
      arg_auxs
      |> List.map Aux.ty
      |> List.iteri (fun i ty -> Debug.printf "%d: %s" i (Type.to_string ty));
      Debug.printf "REACHED / get_function_arg_types";
    in

    (* *)
    let param_type_values =
      param_types
      |> List.map (fun x -> Ctfe_value.Type x)
    in
    let arg_type_values =
      arg_auxs
      |> List.map Aux.ty
      |> List.map (fun ty -> Ctfe_value.Type ty)
    in

    (* eval specialized levels *)
    let specialized_levels =
      List.map (count_specialized_level_of_param ctx) param_type_values
      |> List.sort compare
    in

    (* unify types in parameters and args *)
    let () =
      let param_type_values' = param_type_values |> List.enum in
      let arg_type_values' = arg_type_values |> List.enum in
      Enum.drop dn arg_type_values';
      Enum.iter2 (unify_arg_value ctx) param_type_values' arg_type_values';
    in

    (* debug *)
    let () =
      List.iter (fun c -> debug_print_meta_var c ctx) uni_ids;
      Debug.printf "       REACHED / unify_arg_value \n";
    in

    let cache_cookie =
      param_types
      |> List.map (normalize_type ctx)
      |> List.map Type.to_string
      |> String.concat "--"
    in

    (**)
    let f_env =
      complete_template_instance menv t_env_record
                                 meta_var_names uni_ids cache_cookie
                                 temp_env opt_cond
                                 ctx attr
    in
    Env.FunctionOp.update_template_specs f_env
                                         ~has_constraints:(Option.is_some opt_cond)
                                         ~specialized_levels:specialized_levels;
    f_env
  in
  try_to_instantiate_candidates menv instantiate


and get_template_function_param_type (var_attr, _, init) env ctx attr =
  match init with
  | (Some ty_node, _) ->
     let (param_ty, _, _) =
       resolve_type_with_qual_and_generics_placeholder ~making_placeholder:true
                                                       var_attr ty_node
                                                       env ctx attr
     in
     Debug.printf "template_function_ty : %s" (Type.to_string param_ty);
     param_ty

  | _ ->
     failwith "[ICE] not implemented / param nodes"


and get_uni_ids_from_type ty =
  let b_uni_ids = match Type.type_sort ty with
    | Type_info.NotDetermined uni_id -> [uni_id]
    | _ -> []
  in
  let t_uni_ids =
    let f ctfe_val =
      match ctfe_val with
      | Ctfe_value.Type ty -> get_uni_ids_from_type ty
      | _ -> []
    in
    ty.Type_info.ti_template_args |> List.map f |> List.flatten
  in
  b_uni_ids @ t_uni_ids


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
  let open Type_info in
  let uni_map = ctx.sc_unification_ctx in
  match (lhs, rhs) with
  | ({ti_sort = NotDetermined lhs_uni_t_id}, {ti_sort = NotDetermined rhs_uni_t_id}) ->
     (* TODO: check template args *)
     Debug.printf "!! unify_type_value(T/T) / %d = %d\n" lhs_uni_t_id rhs_uni_t_id;
     Unification.link_value uni_map lhs_uni_t_id rhs_uni_t_id

  | (({ti_sort = UniqueTy ty_r; ti_template_args = args} as ty),
     ({ti_sort = NotDetermined uni_t_id; ti_template_args = holder_args}))
  | (({ti_sort = NotDetermined uni_t_id; ti_template_args = holder_args}),
     ({ti_sort = UniqueTy ty_r; ti_template_args = args} as ty)) ->
     Debug.printf "!! unify_type_value(T|V) / >>>> %d value [Act: %d, Hld: %d]\n"
                  uni_t_id
                  (List.length args)
                  (List.length holder_args);

     (* TODO: support variadic args *)
     (* use enum to support a type which has template args partially *)
     Enum.iter2 (unify_arg_value ctx) (List.enum holder_args) (List.enum args);

     Debug.printf "!! unify_type_value(T|V) / ---- %d\n" uni_t_id;
     (* TODO: full qualified type is unified. implement template template parameters *)
     let v = Unification.update_value uni_map uni_t_id (Ctfe_value.Type ty) in
     debug_print_meta_var uni_t_id ctx; Debug.printf "\n";
     Debug.printf "!! unify_type_value(T|V) / <<<< %d\n" uni_t_id;
     v

  | (({ti_sort = (UniqueTy _)} as lhs_ty), ({ti_sort = (UniqueTy _)} as rhs_ty)) ->
     (* TODO: check template args *)
     if not (is_type_convertible_to rhs_ty lhs_ty) then
       (*failwith "[ERR] cannot convert type at unify_type_value"*)
       raise Template_type_mismatch


  | (lhs, rhs) ->
     Debug.printf "lhs == %s" (Type.to_string lhs);
     Debug.printf "rhs == %s" (Type.to_string rhs);
     failwith "[ICE] unify_value_type"

and count_specialized_level_of_param ?(pre_uni_id=Unification.dummy_uni_id) ctx tv =
  match tv with
  | Ctfe_value.Undef _ ->
     0

  | Ctfe_value.Type ty ->
     begin
       let open Type_info in
       let uni_map = ctx.sc_unification_ctx in
       let _ = uni_map in
       match ty with
       | {ti_sort = UniqueTy _; ti_template_args = args} ->
          let nn =
            args
            |> List.fold_left (fun n a -> n + count_specialized_level_of_param ctx a) 0
          in
          1 + nn

       | {ti_sort = NotDetermined uni_id} when uni_id = pre_uni_id ->
          0 (* skip *)

       | {ti_sort = NotDetermined uni_id; ti_template_args = holder_args} ->
          let nn =
            holder_args
            |> List.fold_left (fun n a -> n + count_specialized_level_of_param ctx a) 0
          in
          let concrete_n =
            match Unification.search_value_until_terminal uni_map uni_id with
            | (found_uni_id, Unification.Val ctv) ->
               count_specialized_level_of_param ~pre_uni_id:found_uni_id ctx ctv
            | _ ->
               0
          in
          concrete_n + nn

       | {ti_sort = ClassSetTy _; (* ignore template args *)} ->
          1

       | _ ->
          failwith "[ICE]"
     end

  | _ ->
     (* a concrete value is counted as specialized *)
     1

and normalize_type ctx ty =
  match Type.type_sort ty with
  | Type_info.UniqueTy _ -> ty
  | Type_info.NotDetermined uni_id ->
     let ty = match Unification.get_as_value ctx.sc_unification_ctx uni_id with
       | Ctfe_value.Type uty -> uty
       | _ -> ty
       | exception Not_found -> ty
     in
     ty
  | _ -> failwith "[ICE] unexpected type"


and unify_arg_value ctx lhs rhs =
  match (lhs, rhs) with
  | (Ctfe_value.Type lhs_ty, Ctfe_value.Type rhs_ty) ->
     unify_type_value ctx lhs_ty rhs_ty;

  | (Ctfe_value.Undef _, Ctfe_value.Undef _) ->
     failwith "[ERR]"

  | (Ctfe_value.Undef uni_id, v)
  | (v, Ctfe_value.Undef uni_id) ->
     Debug.printf "!! unify_arg_value(T|V) / %d -> value\n" uni_id;
     Unification.update_value ctx.sc_unification_ctx uni_id v

  | _ ->
     let lhs_ty = type_of_ctfe_val ctx lhs in
     let rhs_ty = type_of_ctfe_val ctx rhs in
     match is_type_convertible_to lhs_ty rhs_ty with
     | true ->
        (* TODO: check types of template params and args *)
        failwith (Printf.sprintf "[ICE] not implemented (unify_arg_value) : lhs=%s, rhs=%s"
                                 (Ctfe_util.to_string lhs)
                                 (Ctfe_util.to_string rhs))
     | false ->
        (* TOOD: fix *)
        raise Template_type_mismatch

and prepare_template_params params_node ctx =
  match TAst.kind_of params_node with
  | TAst.PrevPassNode (Ast.{kind = TemplateParamsList params}) ->
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
  : (type_info_t * 'env * Lifetime.t * Meta_level.t * 'v Sema_lifetime.LifetimeMap.t) option * env_t list =
  let recv_ty = Aux.ty recv_aux in
  let recv_cenv = Type.as_unique recv_ty in

  let (opt_ty_ctx, hist) =
    solve_identifier ~do_rec_search:false t_id recv_cenv ctx attr
  in

  match opt_ty_ctx with
  | Some ty_ctx ->
     (* member env named id is found in recv_ty_r! *)
     (Some ty_ctx, hist)

  | None ->
     (* not found *)
     (* first, find the member function like "opDispatch" in recv_ty_r *)
     (* TODO: implement *)

     (* second, do universal_search *)
     if universal_search then
       (* apply ufcs *)
       solve_identifier ~exclude:[Env.Kind.Class] t_id env ctx attr
     else
       (None, hist)


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

and force_change_type_mut ty new_mut ctx =
  let open Type_attr in
  let {
    Type_info.ti_attr = ty_attr;
  } = ty in
  Type.Generator.update_attr_r ctx.sc_tsets.ts_type_gen
                               ty
                               { ty_attr with
                                 ta_mut = new_mut;
                               }

and prepare_instantiate_template (t_env_record: TAst.t Env.template_record)
                                 template_args ext_env ctx attr =
  Debug.printf "\n-----\n&& start instantiation = %s\n-----\n\n"
               (Id_string.to_string t_env_record.Env.tl_name);

  let template_params =
    match TAst.kind_of (t_env_record.Env.tl_params) with
    | TAst.NotInstantiatedNode (Ast.{kind = TemplateParamsList params}, None) -> params
    | _ ->
       failwith "[ICE] unexpected template params"
  in

  if (List.length template_params) < (List.length template_args) then
    raise (Instantiation_failed "length of template parameters is different");

  (* In this context, value of MetaVar is treated as TYPE *)

  (* *)
  let (meta_var_id_names, meta_var_inits) =
    List.split template_params
  in

  (* This is a temporary environment to evaluate meta variables.
   * DO NOT append this env to the parent_env.
   *)
  let temp_env =
    Env.create_scoped_env ext_env (Id_string.Pure "<tmp>")
                          (Env.Scope (Env.empty_lookup_table ()))
                          None
  in

  let (uni_ids, meta_specs) =
    (* generate meta variables which have no value and no type *)
    let generate_meta_var id_name =
      let loc = None in
      let uni_id = Unification.generate_uni_id ctx.sc_unification_ctx in
      let mv_env =
        Env.create_context_env temp_env id_name (Env.MetaVariable uni_id) loc
      in
      let mv_ty = make_not_determined_type uni_id ctx in
      (uni_id, (mv_ty, mv_env))
    in
    List.map generate_meta_var meta_var_id_names |> List.split
  in
  (* declare *)
  List.iter2 (fun id_name (_, e) ->
              Env.add_inner_env temp_env id_name e |> error_if_env_is_dupped Loc.dummy)
             meta_var_id_names meta_specs;

  (* set types of meta var(template variables) *)
  let set_type_to_meta_var (var_ty, env) opt_init =
    match opt_init with
    (* :U *)
    | Some (Some ty_expr, None) ->
       let var_attr = {
         Type_attr.ta_ref_val = Type_attr.Ref [];
         Type_attr.ta_mut = Type_attr.Immutable;
       } in
       let ty = resolve_type_with_qual var_attr ty_expr temp_env ctx None in
       unify_type ctx var_ty ty

    (* = V *)
    | Some (None, Some value) ->
       failwith "= V / not supported"

    (* :U = V *)
    | Some (Some ty, Some value) ->
       failwith ":U = V / not supported"

    (**)
    | Some (None, None) ->
       failwith "[ICE] unexpected"

    (* if not specified, treat as :type is specified *)
    | None ->
       begin
         match Type.type_sort var_ty with
         | Type_info.NotDetermined _ ->
            let type_ty = ctx.sc_tsets.ts_type_type in
            unify_type ctx var_ty type_ty
         | _ ->
            failwith "[ICE] unexpected"
       end
  in
  List.iter2 set_type_to_meta_var meta_specs meta_var_inits;

  Debug.printf "== PRINT META VARIABLES (after type set)\n";
  List.iter (fun c -> debug_print_meta_var c ctx) uni_ids;

  (* set values of meta var(template variables) *)
  let template_params_default_values =
    let set_default_value_to_meta_var uni_id =
      let ty = normalize_mata_var_as_type uni_id ctx in
      let ctfe_val =
        if Type.has_same_class ty ctx.sc_tsets.ts_type_type then
          (* if the value has 'type' type, set 'not_determined_type' value *)
          let ud_ty = make_not_determined_type uni_id ctx in
          let type_val = Ctfe_value.Type ud_ty in
          type_val
        else
          (* otherwise, set 'undef' value *)
          let undef_val = Ctfe_value.Undef uni_id in
          undef_val
      in
      Unification.update_value ctx.sc_unification_ctx uni_id ctfe_val;
      ctfe_val
    in
    List.map set_default_value_to_meta_var uni_ids
  in
  Debug.printf "\nREACHED / set_default_value\n";

  Debug.printf "== PRINT META VARIABLES (after default value set)\n";
  List.iter (fun c -> debug_print_meta_var c ctx) uni_ids;

  Debug.printf "len(template_params_default_values) = %d\n"
               (List.length template_params_default_values);
  Debug.printf "len(template_args) = %d\n"
               (List.length template_args);

  (* match values by template args *)
  Enum.iter2 (unify_arg_value ctx)
             (List.enum template_params_default_values)
             (List.enum template_args);

  Debug.printf "\nREACHED / unify_arg_value\n";

  (* TODO: assign default template parameter values *)
  (temp_env, meta_var_id_names, uni_ids)


and complete_template_instance ?(making_placeholder=false)
                               menv t_env_record meta_var_names
                               uni_ids cache_cookie
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
      | _ ->
         raise (Instantiation_failed "not resolved") (*failwith "[ERR] not resolved"*)
    in

    let normalize_uni_type uni_id =
      normalize_uni_id uni_id
                       Unification.search_type_until_terminal
                       Unification.update_type
    in
    let normalize_uni_value uni_id =
      let update ctx uni_id v =
        match v with
        | Ctfe_value.Undef _ ->
           raise (Instantiation_failed "value is undef")(*failwith "[ERR] not resolved"*)
        | Ctfe_value.Type ty ->
           begin
             if Type.is_unique_ty ty then
               Unification.update_value ctx uni_id v
             else
               let msg = Printf.sprintf "type is undef: %s" (Type.to_string ty) in
               raise (Instantiation_failed msg)
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
  Debug.printf "\n--\ncomplete_template_instance: normalize_meta_var\n--\n";
  List.iter normalize_meta_var uni_ids;

  let (inner_node, inner_attr) =
    match TAst.kind_of t_env_record.Env.tl_inner_node with
    | TAst.NotInstantiatedNode (n, a) -> (n, a)
    | _ -> failwith "[ICE] unexpected not instantiated node"
  in

  (* cond *)
  let instantiable = match opt_cond with
    | Some cond ->
       begin
         (*let scope_env =
                Env.create_scoped_env temp_env
                                      (Env.Scope (Env.empty_lookup_table ()))
              in*)
         let lt_env = LifetimeEnv.init () in
         let temp_obj_spec = SubExprSpec.create temp_env lt_env in
         let scope_env = temp_env in
         let (n_cond_expr, cond_aux) =
           analyze_expr ~sub_nest:0
                        cond scope_env temp_obj_spec ctx attr
         in
         let bool_ty = Sema_builtin.get_builtin_bool_type default_ty_attr ctx in
         let (conved_cond_node, conved_cond_aux) =
           adjust_expr_for_type bool_ty n_cond_expr cond_aux
                                scope_env temp_obj_spec ctx attr
         in
         let conved_cond_ml = Aux.ml conved_cond_aux in
         let ctfe_v =
           eval_texpr_as_ctfe conved_cond_node bool_ty conved_cond_ml
                              scope_env ctx None
         in
         match ctfe_v with
         | Ctfe_value.Bool b ->
            if b then
              Ok ()
            else
              Bad ()
         | _ -> failwith "[ICE]"
       end
    | None ->
       Ok ()
  in
  let () = match instantiable with
    | Bad _ ->
       raise (Instantiation_failed "")
    | _ ->
       ()
  in

  Debug.printf "\n--\ncomplete_template_instance: debug_print_meta_var\n--\n";
  List.iter (fun i -> debug_print_meta_var i ctx) uni_ids;

  let instence_cache_key =
    (*TODO: fix*)
    let cache_cookie =
      match Ast.kind_of inner_node with
      | Ast.FunctionDefStmt (_, _, Ast.{kind = ParamsList ps}, _, _, _, _, _, _) ->
         let (_, pks, _, _) = analyze_parameters ps temp_env ctx attr in
         let tys = pks |> List.map typeinfo_of_paramkind in
         tys |> List.map Type.to_string |> String.concat "--"
      | _ -> cache_cookie
    in

    let sym =
      uni_ids
      |> List.map (Unification.get_as_value ctx.sc_unification_ctx)
      |> (fun x -> Mangle.s_of_template_args x ctx.sc_tsets)
    in
    let sym = Printf.sprintf "%s|%s" sym cache_cookie in
    let sym =
      match opt_cond with
      | Some _ ->
         sym
      | None ->
         Printf.sprintf "%s|NO_CONSTRAINTS" sym
    in
    sym
  in
  Debug.printf "TRY making an instance! -> %s\n" instence_cache_key;

  let mset_record = Env.MultiSetOp.get_record menv in
  let env_cache =
    Hashtbl.find_option mset_record.Env.ms_instanced_args_cache_for_env instence_cache_key
  in
  match env_cache with
  | Some env ->
     Debug.printf "USED ENV CACHE for %s\n" instence_cache_key;
     env (* DO NOTHING, because this template is already generated *)
  | None ->
     begin
       let mvs = List.combine meta_var_names uni_ids in
       let env_parent = Option.get menv.Env.parent_env in

       let snode =
         let node_cache =
           Hashtbl.find_option mset_record.Env.ms_instanced_args_cache_for_node
                               instence_cache_key
         in
         match node_cache with
         | Some n ->
            Debug.printf "USE NODE CACHE\n";
            n
         | None ->
            Debug.printf "MAKE NODE CACHE\n";
            let n =
              solve_forward_refs ~meta_variables:mvs
                                 ~opt_attr:inner_attr
                                 ~execution_level:Meta_level.Meta
                                 inner_node env_parent ctx
            in
            Hashtbl.add mset_record.Env.ms_instanced_args_cache_for_node
                        instence_cache_key n;
            n
       in

       (* instantiate! *)
       let lt_env = LifetimeEnv.init () in
       let (n_ast, _, _, _) = construct_env snode env_parent lt_env ctx inner_attr in

       (* take an environment *)
       let i_env = match TAst.kind_of n_ast with
         | TAst.GenericFuncDef (_, Some e)
         | TAst.FunctionDefStmt (_, _, _, _, _, _, _, _, Some e)
         | TAst.MemberFunctionDefStmt (_, _, _, _, _, _, _, Some e)
         | TAst.ExternFunctionDefStmt (_, _, _, _, _, _, _, _, Some e)
         | TAst.ClassDefStmt (_, _, _, _, Some e)
         | TAst.ExternClassDefStmt (_, _, _, _, _, Some e) ->
            e
         | _ ->
            TAst.print n_ast;
            failwith "[ICE] complete template / cache ..."
       in
       Debug.printf "INSTANTIATE: %s / instence_cache_key: %s / cache_cookie: %s"
                    (Option.default (i_env.Env.name |> Id_string.to_string)
                                    i_env.Env.mangled_name)
                    instence_cache_key
                    cache_cookie;

       (* memoize *)
       Hashtbl.add mset_record.Env.ms_instanced_args_cache_for_env instence_cache_key i_env;

       i_env
     end


(* XXX: should ext_env be parent env of menv...? *)
and instantiate_class_templates menv template_args ext_env ctx attr =
  let instantiate (t_env_record: TAst.t Env.template_record) =
    let (temp_env, meta_var_names, uni_ids) =
      prepare_instantiate_template t_env_record template_args
                                   ext_env ctx attr
    in
    complete_template_instance menv t_env_record
                               meta_var_names uni_ids ""
                               temp_env None
                               ctx attr
  in
  try_to_instantiate_candidates menv instantiate


and try_to_instantiate_candidates menv instantiator =
  let f (ress, errs) tenv =
    try
      let res = instantiator tenv in
      (res :: ress, errs)
    with
    | Instantiation_failed msg ->
       let err = Error_msg.Msg msg in (* TODO: fix *)
       (ress, err :: errs)
    | Template_type_mismatch ->
       let err = Error_msg.Msg "type mismatch" in (* TODO: fix *)
       (ress, err :: errs)
  in
  let mset_record = Env.MultiSetOp.get_record menv in
  let (instances, errs) =
    List.fold_left f ([], []) mset_record.Env.ms_templates
  in
  (instances |> List.rev, errs |> List.rev)

and map_conversions ?(param_passing=false)
                    filters eargs ext_env temp_obj_spec ctx =
  let f filter arg =
    let act = match param_passing with
      | true -> Some SoParamPassing
      | false -> None
    in
    apply_conv_filter ~opt_operation:act filter arg ext_env temp_obj_spec ctx
  in
  List.map2 f filters eargs

and declare_incomple_assign cenv =
  let loc = None in
  let ml = Meta_level.Runtime in
  let (base_env, _) =
    Env.MultiSetOp.find_or_add cenv assign_name (Env.Kind.Function ml)
  in
  let fenv_r = Env.FunctionOp.empty_record assign_name in

  let fenv =
    Env.create_context_env cenv assign_name
                           (Env.Function (
                                Env.empty_lookup_table ~init:0 (),
                                fenv_r))
                           loc
  in
  Env.MultiSetOp.add_normal_instances base_env fenv;
  fenv


and declare_this_variable fenv cenv ctx =
  (* prepare "this" special var *)(* TODO: consider member qual *)
  let this_ty =
    let attr = Type_attr.make (Type_attr.Ref []) Type_attr.Mutable in
    Sema_type.make_class_type cenv attr fenv ctx
  in
  let (this_id_name, this_venv) =
    make_parameter_venv fenv (Id_string.Pure "this") this_ty ctx
  in
  Env.add_inner_env fenv this_id_name this_venv |> error_if_env_is_dupped Loc.dummy;
  (this_ty, this_venv)


(** for default constructors **)
and define_trivial_default_ctor_for_builtin cenv extern_cname ctx =
  let (fenv, ret_ty) = declare_checked_default_ctor cenv ctx in

  let node = TAst.{kind = GenericFuncDef (None, Some fenv); loc = Loc.dummy} in
  let detail =
    Env.FnRecordBuiltin (Env.FnDefDefaulted true,   (* trivial *)
                         Env.FnKindDefaultConstructor None,
                         (Builtin_info.make_builtin_default_ctor_name extern_cname))
  in
  complete_function_env fenv node ctor_id_name detail ctx

and define_trivial_default_ctor cenv ctx =
  let (fenv, ret_ty) = declare_checked_default_ctor cenv ctx in

  let node = TAst.{kind = GenericFuncDef (None, Some fenv); loc = Loc.dummy} in
  let fn_spec = {
      Env.fn_spec_param_envs = [];
      Env.fn_spec_force_inline = true;
  } in
  let detail =
    Env.FnRecordImplicit (Env.FnDefDefaulted true,  (* trivial *)
                          Env.FnKindDefaultConstructor None,
                          fn_spec)
  in
  complete_function_env fenv node ctor_id_name detail ctx

and define_implicit_default_ctor cenv lt_env ctx =
  let (fenv, ret_ty) = declare_checked_default_ctor cenv ctx in
  let (this_ty, this_venv) = declare_this_variable fenv cenv ctx in

  let call_inst =
    let make_call_defctor_inst venv =
      let venv_r = Env.VariableOp.get_record venv in
      let var_ty = venv_r.Env.var_type in
      let var_cenv = Type.as_unique var_ty in
      let var_cenv_r = Env.ClassOp.get_record var_cenv in
      let var_defctor = match var_cenv_r.Env.cls_default_ctor with
        | Some e -> e
        | None -> failwith "[ICE] no ctor for thenmember"
      in
      let var_call_fs_and_args = (var_defctor, [], []) in

      let temp_obj_spec = SubExprSpec.create cenv lt_env in
      let f_sto = TAst.StoMemberVar (var_ty, Some venv, Some fenv) in
      let (t_ast, _) =
        match make_call_instruction var_call_fs_and_args None (Some f_sto) fenv temp_obj_spec ctx with
        | Ok res -> res
        | Bad _ -> failwith ""
      in
      TAst.{kind = ExprStmt t_ast; loc = Loc.dummy}   (* TODO: register dtors *)
    in

    let call_insts_list =
      let cenv_r = Env.ClassOp.get_record cenv in
      cenv_r.Env.cls_member_vars |> List.map make_call_defctor_inst
    in

    let nbody = TAst.{kind = StatementList call_insts_list; loc = Loc.dummy} in
    let _ = post_check_function_return_type fenv ctx in
    check_and_insert_suitable_return ~is_special_func:true nbody fenv ctx None
  in

  let node = TAst.{kind = GenericFuncDef (Some call_inst, Some fenv); loc = Loc.dummy} in
  let fn_spec = {
      Env.fn_spec_param_envs = [];
      Env.fn_spec_force_inline = true;
  } in
  let detail =
    Env.FnRecordImplicit (Env.FnDefDefaulted false,
                          Env.FnKindDefaultConstructor (Some this_venv),
                          fn_spec)
  in

  complete_function_env fenv node ctor_id_name detail ctx

and make_ctor_generics_map ty : 'v Sema_lifetime.LifetimeMap.t=
  let {
      Type_info.ti_generics_args = generics_args;
    } = ty in
  let ty_cenv = Type.as_unique ty in
  let ty_cenv_er = Env.ClassOp.get_record ty_cenv in
  let ty_cenv_generics_params = ty_cenv_er.Env.cls_generics_vals in

  Debug.printf "CLS =>=>=> %d\n" (List.length generics_args);
  let () =
    let f l =
      Debug.printf "generics args of type => %s\n" (Lifetime.to_string l)
    in
    List.iter f generics_args
  in
  Sema_lifetime.map_generics_args ty_cenv_generics_params generics_args

and define_implicit_default_ctor_for_array cenv lt_env elem_ty total_num ctx =
  let (fenv, ret_ty) = declare_checked_default_ctor cenv ctx in
  let (this_ty, this_venv) = declare_this_variable fenv cenv ctx in

  let call_inst =
    let elem_ty_cenv = Type.as_unique elem_ty in
    let elem_ty_cenv_r = Env.ClassOp.get_record elem_ty_cenv in
    let elem_defctor = match elem_ty_cenv_r.Env.cls_default_ctor with
      | Some e -> e
      | None -> failwith "[ICE] no ctor for class of elements"
    in

    let make_call_defctor_inst idx =
      let idx = Uint32.to_int idx in (* TODO: fix *)
      let elem_call_fs_and_args = (elem_defctor, [], []) in

      let temp_obj_spec = SubExprSpec.create cenv lt_env in
      (* this.buffer[idx] is initialized by ctor *)
      let f_sto = TAst.StoArrayElemFromThis (elem_ty, Some this_venv, idx) in
      let (t_ast, _) =
        let cls_generics_map = make_ctor_generics_map elem_ty in
        match make_call_instruction ~mm:cls_generics_map elem_call_fs_and_args None (Some f_sto) fenv temp_obj_spec ctx with
        | Ok res -> res
        | Bad err -> failwith ""
      in
      TAst.{kind = ExprStmt t_ast; loc = Loc.dummy} (* TODO: register dtors *)
    in

    assert(total_num > Uint32.of_int 0);
    let call_insts_list =
      let open Uint32 in
      (* [0, total_num) *)
      Enum.seq zero ((+) one) ((>) total_num) /@ make_call_defctor_inst
      |> List.of_enum
    in

    let nbody = TAst.{kind = StatementList call_insts_list; loc = Loc.dummy} in
    let _ = post_check_function_return_type fenv ctx in
    check_and_insert_suitable_return ~is_special_func:true nbody fenv ctx None
  in

  let node = TAst.{kind = GenericFuncDef (Some call_inst, Some fenv); loc = Loc.dummy} in
  let fn_spec = {
      Env.fn_spec_param_envs = [];
      Env.fn_spec_force_inline = true;
  } in
  let detail =
    Env.FnRecordImplicit (Env.FnDefDefaulted false,  (* not trivial *)
                          Env.FnKindDefaultConstructor (Some this_venv),
                          fn_spec)
  in
  complete_function_env fenv node ctor_id_name detail ctx


(** for copy constructors **)

and define_trivial_copy_ctor cenv ctx =
  let (fenv, rhs_ty, ret_ty) = declare_checked_copy_ctor cenv ctx in

  let node = TAst.{kind = GenericFuncDef (None, Some fenv); loc = Loc.dummy} in
  let fn_spec = {
      Env.fn_spec_param_envs = [];
      Env.fn_spec_force_inline = true;
  } in
  let detail =
    Env.FnRecordImplicit (Env.FnDefDefaulted true,      (* trivial *)
                          Env.FnKindCopyConstructor None,
                          fn_spec)
  in
  complete_function_env fenv node ctor_id_name detail ctx

and define_implicit_copy_ctor cenv lt_env ctx =
  let (fenv, rhs_ty, ret_ty) = declare_checked_copy_ctor cenv ctx in
  let (this_ty, this_venv) = declare_this_variable fenv cenv ctx in

  let (rhs_id_name, rhs_venv) =
    make_parameter_venv fenv (Id_string.Pure "rhs") rhs_ty ctx
  in
  Env.add_inner_env fenv rhs_id_name rhs_venv |> error_if_env_is_dupped Loc.dummy;

  let call_inst =
    let src_node = Ast.{kind = Id (rhs_id_name, [], ()); loc = Loc.dummy} in
    (* this.VARNAME.ctor(rhs.VARNAME); *)
    let make_call_copyctor_inst venv =
      let venv_r = Env.VariableOp.get_record venv in

      let sub_expr_spec = SubExprSpec.create fenv lt_env in

      (* rhs.VAR_NAME *)
      let rhs_elem =
        let trg_node = Ast.{kind = Id (venv_r.Env.var_name, [], ()); loc = Loc.dummy} in
        let rhs_node = Ast.{kind = ElementSelectionExpr (src_node, trg_node, ()); loc = Loc.dummy} in
        analyze_expr ~making_placeholder:false
                     ~sub_nest:0 (* TODO: *)
                     rhs_node fenv sub_expr_spec ctx None
      in

      let venv_r = Env.VariableOp.get_record venv in
      let var_ty = venv_r.Env.var_type in
      let vcenv = Type.as_unique var_ty in
      let vcenv_r = Env.ClassOp.get_record vcenv in
      let vdctor = match vcenv_r.Env.cls_copy_ctor with
        | Some e -> e
        | None -> failwith "[ICE] no ctor for thenmember"
      in
      let (_, fs_and_args, _) =
        find_suitable_functions [vdctor] [rhs_elem] fenv ctx None
      in
      assert (List.length fs_and_args = 1);
      let fs_and_args = List.hd fs_and_args in

      let f_sto =
        TAst.StoMemberVar (var_ty, Some venv, Some fenv)
      in
      let (t_ast, _) =
        let call_inst =
          make_call_instruction fs_and_args None (Some f_sto) fenv sub_expr_spec ctx
        in
        match call_inst with
        | Ok res -> res
        | Bad err -> failwith ""
      in
      TAst.{kind = ExprStmt t_ast; loc = Loc.dummy}   (* TODO: register dtors *)
    in

    let call_insts_list =
      let cenv_r = Env.ClassOp.get_record cenv in
      cenv_r.Env.cls_member_vars |> List.map make_call_copyctor_inst
    in

    let nbody = TAst.{kind = StatementList call_insts_list; loc = Loc.dummy} in
    let _ = post_check_function_return_type fenv ctx in
    check_and_insert_suitable_return ~is_special_func:true nbody fenv ctx None
  in

  let node = TAst.{kind = GenericFuncDef (Some call_inst, Some fenv); loc = Loc.dummy} in
  let fn_spec = {
      Env.fn_spec_param_envs = [Some rhs_venv];
      Env.fn_spec_force_inline = true;
  } in
  let detail =
    Env.FnRecordImplicit (Env.FnDefDefaulted false,    (* not trivial *)
                          Env.FnKindCopyConstructor (Some this_venv),
                          fn_spec)
  in

  complete_function_env fenv node ctor_id_name detail ctx

and define_implicit_copy_ctor_for_array cenv lt_env elem_ty total_num ctx =
  let (fenv, rhs_ty, ret_ty) = declare_checked_copy_ctor cenv ctx in
  let (this_ty, this_venv) = declare_this_variable fenv cenv ctx in

  let (rhs_id_name, rhs_venv) =
    make_parameter_venv fenv (Id_string.Pure "rhs") rhs_ty ctx
  in
  Env.add_inner_env fenv rhs_id_name rhs_venv |> error_if_env_is_dupped Loc.dummy;

  let call_inst =
    let elem_ty_cenv = Type.as_unique elem_ty in
    let elem_ty_cenv_r = Env.ClassOp.get_record elem_ty_cenv in
    let elem_copyctor = match elem_ty_cenv_r.Env.cls_copy_ctor with
      | Some e -> e
      | None -> failwith "[ICE] no ctor for class of elements"
    in

    let src_node = Ast.{kind = Id (rhs_id_name, [], ()); loc = Loc.dummy} in
    (* this[idx].ctor(rhs[idx]) *)
    let make_call_copyctor_inst idx =
      let idx = Uint32.to_int idx in (* TODO: fix *)

      let sub_expr_spec = SubExprSpec.create fenv lt_env in

      let elem_call_fs_and_args =
        let rhs_elem =
          let index_node = Ast.{kind = IntLit (idx, 32, false, ()); loc = Loc.dummy} in
          let rhs_node = Ast.{kind = SubscriptingExpr (src_node, (Some index_node), ()); loc = Loc.dummy} in
          analyze_expr ~making_placeholder:false
                       ~sub_nest:0
                       rhs_node fenv sub_expr_spec ctx None
        in
        let (_, fs_and_args, _) =
          find_suitable_functions [elem_copyctor] [rhs_elem] fenv ctx None
        in
        assert (List.length fs_and_args = 1);
        List.hd fs_and_args
      in

      (* this.buffer[idx] is initialized by using rhs.buffer[idx] *)
      let f_sto = TAst.StoArrayElemFromThis (elem_ty, Some this_venv, idx) in
      let (t_ast, _) =
        let cls_generics_map = make_ctor_generics_map elem_ty in
        match make_call_instruction ~mm:cls_generics_map elem_call_fs_and_args None (Some f_sto) fenv sub_expr_spec ctx with
        | Ok res -> res
        | Bad err -> failwith ""
      in
      TAst.{kind = ExprStmt t_ast; loc = Loc.dummy} (* TODO: register dtors *)
    in

    assert(total_num > Uint32.of_int 0);
    let call_insts_list =
      let open Uint32 in
      (* [0, total_num) *)
      Enum.seq zero ((+) one) ((>) total_num) /@ make_call_copyctor_inst
      |> List.of_enum
    in

    let nbody = TAst.{kind = StatementList call_insts_list; loc = Loc.dummy} in
    let _ = post_check_function_return_type fenv ctx in
    check_and_insert_suitable_return ~is_special_func:true nbody fenv ctx None
  in

  let node = TAst.{kind = GenericFuncDef (Some call_inst, Some fenv); loc = Loc.dummy} in
  let fn_spec = {
      Env.fn_spec_param_envs = [Some rhs_venv];
      Env.fn_spec_force_inline = true;
  } in
  let detail =
    Env.FnRecordImplicit (Env.FnDefDefaulted false,  (* not trivial *)
                          Env.FnKindCopyConstructor (Some this_venv),
                          fn_spec)
  in
  complete_function_env fenv node ctor_id_name detail ctx


(** for copy assignment **)
and define_trivial_copy_assign_for_builtin ?(has_ptr_constraints=false)
                                           cenv extern_cname ctx =

  let (ty, rhs_ty, lts, lt_constraints) =
    match has_ptr_constraints with
    (* ('a: 'r1, 'r1, 'r2) op=('r1 ref :'a ty, 'r2 ref :'a rhs_ty) *)
    | true ->
       let ty =
         let attr = Type_attr.make (Type_attr.Ref []) Type_attr.Mutable in
         Sema_type.make_class_type ~new_instance:true cenv attr cenv ctx (* XXX: *)
       in
       let rhs_ty =
         let attr = Type_attr.make (Type_attr.Ref []) Type_attr.Const in
         Sema_type.make_class_type ~new_instance:true cenv attr cenv ctx (* XXX: *)
       in

       let ty_lt = match ty.Type_info.ti_generics_args with
         | [lt] -> lt
         | _ -> failwith "[ICE]"
       in
       let r1_lt = match ty.Type_info.ti_aux_generics_args with
         | [lt] -> lt
         | _ -> failwith "[ICE]"
       in

       let r2_lt = match rhs_ty.Type_info.ti_aux_generics_args with
         | [lt] -> lt
         | _ -> failwith "[ICE]"
       in

       let new_lts =
         Lifetime.convert [ty_lt; r1_lt; r2_lt] [(ty_lt, r1_lt)] (Env.get_id cenv)
       in
       [ty_lt; r1_lt; r2_lt] |> List.iter (fun lt -> Debug.printf "%s\n" (Lifetime.to_string lt));
       new_lts |> List.iter (fun lt -> Debug.printf "%s\n" (Lifetime.to_string lt));
       let (n_ty_lt, n_r1_lt, n_r2_lt) = match new_lts with
         | [a; b; c] -> (a, b, c)
         | _ -> failwith "[ICE]"
       in
       Debug.printf "TYPE: %s || %s\n" (Type.to_string ty) (Type.to_string rhs_ty);
       let ty =
         Type.Generator.update_attr_r3 ctx.sc_tsets.ts_type_gen ty [n_r1_lt] [n_ty_lt]
       in
       let rhs_ty =
         Type.Generator.update_attr_r3 ctx.sc_tsets.ts_type_gen rhs_ty [n_r2_lt] [n_ty_lt]
       in

       (* TODO: add lifetimes of rest of ty and rhs_ty *)
       Debug.printf "TYPE: %s || %s\n" (Type.to_string ty) (Type.to_string rhs_ty);
       Debug.printf "CONSTRAINTS: %s || %s\n" (Lifetime.to_string n_ty_lt) (Lifetime.to_string n_r1_lt);
       (ty, rhs_ty, [n_ty_lt; n_r1_lt; n_r2_lt], [Lifetime.LtMin (n_ty_lt, n_r1_lt)])

    | false ->
       let ty =
         let attr = Type_attr.make (Type_attr.Ref []) Type_attr.Mutable in
         Sema_type.make_class_type cenv attr cenv ctx  (* XXX: *)
       in
       let rhs_ty =
         Debug.printf "define_trivial_copy_assign_for_builtin: no ptr: %s (%b)"
                      (Id_string.to_string @@ Env.get_name cenv)
                      (Env.ClassOp.is_primitive cenv);
         let attr =
           match Env.ClassOp.is_primitive cenv with
           | true ->
              Type_attr.make (Type_attr.Val) Type_attr.Const
           | false ->
              Type_attr.make (Type_attr.Ref []) Type_attr.Const
         in
         Sema_type.make_class_type cenv attr cenv ctx  (* XXX: *)
       in

       (* TODO: add lifetimes of ty and rhs_ty *)
       (ty, rhs_ty, [], [])
  in

  let fenv = declare_incomple_assign cenv in

  let detail =
    Env.FnRecordBuiltin (Env.FnDefDefaulted true,
                         Env.FnKindMember,
                         (Builtin_info.make_builtin_copy_assign_name extern_cname))
  in

  (* interface of copy assignment: ref mutable(TYPE) -> ref(TYPE) -> ref mutable(TYPE) *)
  check_function_env2 fenv lts lt_constraints
    [Env.FnParamKindType ty; Env.FnParamKindType rhs_ty] Meta_level.Runtime ty false;

  let node = TAst.{kind = GenericFuncDef (None, Some fenv); loc = Loc.dummy} in
  complete_function_env fenv node assign_name detail ctx

and get_builtin_raw_ptr_type elem_ty ptr_attr ctx : 'env type_info =
  let raw_ptr_ty = !(ctx.sc_tsets.ts_raw_ptr_type_holder) in
  assert (not @@ Type.is_undef raw_ptr_ty);

  Debug.printf "========= RawPtr Element\n";
  Type.debug_print elem_ty;

  let ty = match Type.type_sort raw_ptr_ty with
    | Type_info.ClassSetTy menv ->
       begin
         let template_args = [Ctfe_value.Type elem_ty] in
         let ext_env = Option.get menv.Env.parent_env in
         let (instances,_ ) =
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
                                      [Lifetime.LtStatic]
                                      ptr_attr
       end
    | _ -> failwith "[ICE] unexpected"
  in
  Type.debug_print ty;
  ty

and get_builtin_array_type elem_ty len arr_attr ctx : 'env type_info =
  let arr_ty = !(ctx.sc_tsets.ts_array_type_holder) in
  assert (not @@ Type.is_undef arr_ty);

  Debug.printf "========= Array Element\n";
  Type.debug_print elem_ty;

  let ty = match Type.type_sort arr_ty with
    | Type_info.ClassSetTy menv ->
       begin
         let template_args = [Ctfe_value.Type elem_ty;
                              Ctfe_value.Uint32 (Uint32.of_int len)
                             ] in
         let ext_env = Option.get menv.Env.parent_env in
         let (instances, _) =
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
                                      []    (* TODO *)
                                      arr_attr
       end
    | _ -> failwith "[ICE] unexpected"
  in
  Type.debug_print ty;
  ty


(* *)
and cache_builtin_type_info builtin_mod_e preset_ty name ctx =
  match Type.type_sort !preset_ty with
  (* not defined yet *)
  | Type_info.Undef ->
     begin
       Debug.printf "get_builtin_type_info = %s\n" name;

       let (res, hist) =
         solve_basic_identifier ~do_rec_search:true
                                (Id_string.Pure name) []
                                builtin_mod_e ctx None
       in
       match res with
       (* pure type *)
       | Some (ty, c_env, _, _, _) when ty == ctx.sc_tsets.ts_type_type ->
          begin
            let prim_ty =
              Type.Generator.generate_type ctx.sc_tsets.ts_type_gen
                                           (Type_info.UniqueTy c_env)
                                           []
                                           []   (* TODO *)
                                           default_ty_attr
            in
            preset_ty := prim_ty;
          end

       (* template class *)
       | Some (ty, menv, _, _, _) when Type.is_class_set ty ->
          begin
            preset_ty := ty;
          end

       (**)
       | _ ->
          fatal_error (Error_msg.MemberNotFound (builtin_mod_e, hist, None))
     end

  (* already defined *)
  | _ -> ()

and cache_primitive_types builtin_mod_env ctx =
  let open Builtin_info in
  let tsets = ctx.sc_tsets in

  (* cache bool type *)
  cache_builtin_type_info builtin_mod_env
                          tsets.ts_bool_type_holder
                          bool_type_i.external_name
                          ctx;

  (* cache uint8 type *)
  cache_builtin_type_info builtin_mod_env
                          tsets.ts_uint8_type_holder
                          uint8_type_i.external_name
                          ctx;

  (* cache int32 type *)
  cache_builtin_type_info builtin_mod_env
                          tsets.ts_int32_type_holder
                          int32_type_i.external_name
                          ctx;

  (* cache uint32 type *)
  cache_builtin_type_info builtin_mod_env
                          tsets.ts_uint32_type_holder
                          uint32_type_i.external_name
                          ctx;

  (* cache array type *)
  cache_builtin_type_info builtin_mod_env
                          tsets.ts_array_type_holder
                          array_type_i.external_name
                          ctx;

  (* cache untyped pointer types *)
  cache_builtin_type_info builtin_mod_env
                          tsets.ts_untyped_raw_ptr_type_holder
                          untyped_raw_ptr_type_i.external_name
                          ctx;

  (* cache pointer types *)
  cache_builtin_type_info builtin_mod_env
                          tsets.ts_raw_ptr_type_holder
                          raw_ptr_type_i.external_name
                          ctx;

  ()

and create_updated_type ty
                        ?(rv=ty.Type_info.ti_attr.Type_attr.ta_ref_val)
                        ?(mut=ty.Type_info.ti_attr.Type_attr.ta_mut)
                        ?(aux_generics_args=ty.Type_info.ti_aux_generics_args)
                        ?(generics_args=ty.Type_info.ti_generics_args)
                        ctx =
  let nty =
    { ty with
      Type_info.ti_attr =
        {
          Type_attr.ta_ref_val = rv;
          Type_attr.ta_mut = mut;
        };
      Type_info.ti_aux_generics_args = aux_generics_args;
      Type_info.ti_generics_args = generics_args;
    }
  in
  Type.Generator.register_type ctx.sc_tsets.ts_type_gen nty

and make_type_default_form ?(rv=Type_attr.Val)
                           ?(mut=Type_attr.Const)
                           ty ctx =
  let aux_generic_args = match rv with
    | Type_attr.Val -> []
    | Type_attr.Ref _ -> ty.Type_info.ti_aux_generics_args
    | _ -> failwith ""
  in
  let _ = aux_generic_args in
  let trg_ty =
    create_updated_type ~rv:rv
                        ~mut:mut
                        ~aux_generics_args:aux_generic_args
                        ty ctx
  in
  match is_type_convertible_to ty trg_ty with
  | true -> Some trg_ty
  | false -> None

and analyze ?(meta_variables=[]) ?(opt_attr=None) (node: Ast.t) env lt_env ctx =
  let (node, _, _, _) =
    analyze_t ~meta_variables:meta_variables
              ~opt_attr:opt_attr
              node env lt_env ctx
  in
  node

and analyze_t ?(meta_variables=[]) ?(opt_attr=None) (node: Ast.t) env lt_env ctx =
  let snode =
    solve_forward_refs ~meta_variables:meta_variables
                       ~opt_attr:opt_attr
                       node env ctx
  in
  construct_env snode env lt_env ctx opt_attr


and get_void_aux ctx =
  let ty = get_builtin_void_type default_ty_attr ctx in
  let aux = Aux.make ty VCatPrValue Lifetime.LtStatic Meta_level.Meta in
  aux

(* no-effect to parent_env *)
and create_generics_spec lt_name parent_env =
  let loc = None in
  let lt = Sema_type.create_new_lt_var lt_name parent_env in
  let lt_env = Env.create_context_env parent_env lt_name (Env.LifetimeVariable lt) loc in
  (lt_env, lt)

and declare_generics_specs lifetime_sorts parent_env =
  let tmp_env =
    Env.create_scoped_env parent_env (Id_string.Pure "<tmp>")
                          (Env.Scope (Env.empty_lookup_table ()))
                          None
  in

  (* insert lifetimes into the temporary env *)
  let tmp_lts =
    let f lt_s =
      let (lt, lt_id) = Lifetime.make_placeholder lt_s in
      (* TODO: check duplication *)
      let lt_env = Env.create_context_env tmp_env lt_id (Env.LifetimeVariable lt) None in
      Env.add_inner_env tmp_env lt_id lt_env |> error_if_env_is_dupped Loc.dummy;
      lt
    in
    List.map f lifetime_sorts
  in

  let lt_var_id_constraints =
    (* TODO: support unmanaged *)
    let f lt_s =
      match lt_s with
      | Lifetime.LtSingle _ -> None
      | Lifetime.LtLongerThan (lhs_id, rhs_id) ->
         let lhs = match Env.lookup tmp_env lhs_id with
           | ([v], _) -> Env.LifetimeVariableOp.as_lifetime v
           | _ -> failwith ""
         in
         let rhs = match Env.lookup tmp_env rhs_id with
           | ([v], _) -> Env.LifetimeVariableOp.as_lifetime v
           | _ -> failwith ""
         in
         Debug.printf "CONSTRAINTS: %s %s\n" (Lifetime.to_string lhs) (Lifetime.to_string rhs);
         Some (lhs, rhs)
    in
    List.filter_map f lifetime_sorts
  in

  let new_lts = Lifetime.convert tmp_lts lt_var_id_constraints (Env.get_id parent_env) in

  (* insert lifetimes into the actual env *)
  let () =
    let f spec lt =
      let lt_env =
        Env.create_context_env parent_env spec (Env.LifetimeVariable lt) None
      in
      Env.add_inner_env parent_env spec lt_env |> error_if_env_is_dupped Loc.dummy;
    in
    let lifetime_ids =
      lifetime_sorts
      |> List.map Lifetime.get_id_from_sort
      |> List.unique
    in
    List.iter2 f lifetime_ids new_lts
  in

  let xs = lt_var_id_constraints |> List.map (fun (l, r) -> Lifetime.LtMin (l, r)) in

  (new_lts, xs)
