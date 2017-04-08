(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries
open Value_category
open Sema_definitions
open Sema_context
open Sema_env

let ret_val_category ty ctx =
  let open Type_attr in
  match ty.Type_info.ti_attr with
  | { ta_ref_val = Val; } ->
     VCatPrValue

  | _ -> VCatLValue


let check_env env ml =
  Env.update_status env Env.Checking;
  Env.update_meta_level env ml

let complete_env env node =
  Env.update_status env Env.Complete;
  Env.update_rel_ast env node

let check_function_env2 env generics_vals constraints param_kinds ml return_type is_auto_return_type =
  let r = Env.FunctionOp.get_record env in
  r.Env.fn_generics_vals <- generics_vals;
  r.Env.fn_param_kinds <- param_kinds;
  r.Env.fn_return_type <- return_type;
  r.Env.fn_is_auto_return_type <- is_auto_return_type;
  env.Env.generics_constraints <- constraints;
  check_env env ml

let complete_function_env env node id_name f_detail ctx =
  let r = Env.FunctionOp.get_record env in

  r.Env.fn_detail <- f_detail;

  let m = (Env.get_full_module_name env) |> String.concat "." in
  Debug.printf "Complete function -> %s.%s\n" m (Id_string.to_string id_name);
  Type.debug_print r.Env.fn_return_type;

  let _ = match id_name with
    | Id_string.Pure s when s = Builtin_info.entrypoint_name ->
       begin
         (* TODO: check param_types and return_type *)
         env.Env.mangled_name <- Some Builtin_info.entrypoint_export_name
       end
    | _ ->
       begin
         let mangled =
           Mangle.s_of_function (Env.get_full_module_name env) id_name
                                r.Env.fn_template_vals
                                r.Env.fn_param_kinds r.Env.fn_return_type
                                ctx.sc_tsets
         in
         env.Env.mangled_name <- Some mangled
       end
  in
  complete_env env node


let check_class_env env generics_vals ctx =
  let r = Env.ClassOp.get_record env in
  let id_name = r.Env.cls_name in
  let template_args = r.Env.cls_template_vals in
  let mangled =
    Mangle.s_of_class (Env.get_full_module_name env)
                      id_name template_args ctx.sc_tsets
  in
  env.Env.mangled_name <- Some mangled;
  r.Env.cls_generics_vals <- generics_vals;
  check_env env Meta_level.Meta

let complete_class_env env node c_detail opt_layout =
  let r = Env.ClassOp.get_record env in
  r.Env.cls_detail <- c_detail;
  let _ = match opt_layout with
    | Some (size, align) ->
       r.Env.cls_size <- Some size;
       assert(align >= Stdint.Uint32.of_int 1);
       r.Env.cls_align <- Some align;
    | None ->
       r.Env.cls_size <- None;
       r.Env.cls_align <- None;
  in
  complete_env env node

let register_default_ctor_to_class_env cenv fenv =
  let r = Env.ClassOp.get_record cenv in
  match r.Env.cls_default_ctor with
  | None ->
     r.Env.cls_default_ctor <- (Some fenv)
  | Some _ ->
     failwith "[ICE] default ctor is already registered"

let register_copy_ctor_to_class_env cenv fenv =
  let r = Env.ClassOp.get_record cenv in
  match r.Env.cls_copy_ctor with
  | None ->
     r.Env.cls_copy_ctor <- (Some fenv)
  | Some _ ->
     failwith "[ICE] copy ctor is already registered"

let register_dtor_to_class_env cenv fenv =
  let r = Env.ClassOp.get_record cenv in
  match r.Env.cls_dtor with
  | None ->
     r.Env.cls_dtor <- (Some fenv)
  | Some _ ->
     failwith "[ICE] dtor is already registered"

let complete_variable_env env node ty lifetime v_detail ctx =
  let r = Env.VariableOp.get_record env in
  r.Env.var_type <- ty;
  r.Env.var_lifetime <- lifetime;
  r.Env.var_detail <- v_detail


let is_valid_type ty =
  let open Type_attr in
  let open Type_info in
  let {
    ta_ref_val = rv;
    ta_mut = mut;
  } = ty.Type_info.ti_attr in
  let attr_v = (rv <> RefValUndef) && (mut <> MutUndef) in
  let {
    ti_aux_generics_args = aux_gs;
    ti_generics_args = gs;
  } = ty in
  let g_check lt =
    lt <> Lifetime.LtUndef
  in
  let gs_v = (List.for_all g_check aux_gs) && (List.for_all g_check gs) in
  Debug.printf "- attr:%b / lt:%b(aux:%b): %s\n" attr_v (List.for_all g_check aux_gs) (List.for_all g_check gs) (Type.to_string ty);
  attr_v && gs_v

let assert_valid_type ty =
  assert (is_valid_type ty)


let check_is_args_valid ty =
  (* TODO: implement *)
  ()

let raise_error_if_type_is_invalid ty =
  (* TODO: implement *)
  assert_valid_type ty;
  ()

(* TODO: fix them *)
let get_builtin_void_incomplete_type ctx =
  let open Type_sets in
  ctx.sc_tsets.ts_void_type

let get_builtin_void_type attr ctx =
  let open Type_sets in
  let ty = get_builtin_void_incomplete_type ctx in
  Type.Generator.update_attr_r ctx.sc_tsets.ts_type_gen ty attr

let post_check_function_return_type env ctx =
  let fr = Env.FunctionOp.get_record env in
  let ret_ty = fr.Env.fn_return_type in
  let new_ret_ty = match Type.type_sort ret_ty with
    | Type_info.UniqueTy _ ->
       assert_valid_type ret_ty;
       ret_ty
    | Type_info.Undef ->
       (* if type is not determined, it should be void *)
       let void_ty = get_builtin_void_type default_ty_attr ctx in
       fr.Env.fn_return_type <- void_ty;
       void_ty
    | _ -> failwith @@ "[ERR] type couldn't be determined / " ^
                         (Env.get_name env |> Id_string.to_string)
  in
  new_ret_ty

(*
 * helpers for defining ctor/dtor/assignments
 *)
let declare_incomplete_ctor cenv =
  let loc = None in
  let (base_env, _) = Env.MultiSetOp.find_or_add cenv ctor_id_name Env.Kind.Function in
  let fenv_r = Env.FunctionOp.empty_record ctor_id_name in

  let fenv =
    Env.create_context_env cenv ctor_id_name
                           (Env.Function (
                                Env.empty_lookup_table ~init:0 (),
                                fenv_r))
                           loc
  in
  Env.MultiSetOp.add_normal_instances base_env fenv;
  fenv

let declare_checked_default_ctor cenv ctx =
  let fenv = declare_incomplete_ctor cenv in

  (* interface of default constructor: () -> TYPE *)

  let ret_ty =
    let attr = Type_attr.make Type_attr.Val Type_attr.Const in
    Sema_type.make_class_type cenv attr fenv ctx
  in
  let {
    Type_info.ti_aux_generics_args = aux_generics_args;
    Type_info.ti_generics_args = generics_args;
  } = ret_ty in
  assert (aux_generics_args = []);
  check_function_env2 fenv [] [] [] Meta_level.Meta ret_ty false;

  let new_ret_ty = post_check_function_return_type fenv ctx in

  register_default_ctor_to_class_env cenv fenv;

  (fenv, new_ret_ty)

let declare_checked_copy_ctor ?(has_ptr_constraints=false) cenv ctx =
  let open Type_sets in
  let fenv = declare_incomplete_ctor cenv in

  (* interface of copy constructor: (TYPE) -> TYPE *)
  let ret_ty =
    let attr = Type_attr.make Type_attr.Val Type_attr.Const in
    Sema_type.make_class_type cenv attr fenv ctx
  in
  let {
    Type_info.ti_aux_generics_args = ret_aux_generics_args;
    Type_info.ti_generics_args = ret_generics_args;
  } = ret_ty in
  assert (ret_aux_generics_args = []);

  let (rhs_ty, lts, lt_constraints) =
    match has_ptr_constraints with
    | true ->
       let ret_ty_lt = match ret_generics_args with
         | [lt] -> lt
         | _ -> failwith "[ICE]"
       in

       let rhs_ty =
         let attr = Type_attr.make (Type_attr.Ref []) Type_attr.Const in
         Sema_type.make_class_type ~new_instance:true cenv attr fenv ctx
       in
       let ty_lt = match rhs_ty.Type_info.ti_generics_args with
         | [lt] -> lt
         | _ -> failwith "[ICE]"
       in
       let r1_lt = match rhs_ty.Type_info.ti_aux_generics_args with
         | [lt] -> lt
         | _ -> failwith "[ICE]"
       in

       let new_lts =
         Lifetime.convert [ty_lt; r1_lt] [(ty_lt, ret_ty_lt)] (Env.get_id cenv)
       in

       let (n_ty_lt, n_r1_lt) = match new_lts with
         | [a; b] -> (a, b)
         | _ -> failwith "[ICE]"
       in

       let rhs_ty =
         Type.Generator.update_attr_r3 ctx.sc_tsets.ts_type_gen rhs_ty [n_r1_lt] [n_ty_lt]
       in

       (* TODO: add lifetimes of rest of ty and rhs_ty *)
       (rhs_ty, [n_ty_lt; n_r1_lt], [Lifetime.LtMin (n_ty_lt, ret_ty_lt)])

    | false ->
       let rhs_ty =
         let attr = Type_attr.make (Type_attr.Ref []) Type_attr.Const in
         Sema_type.make_class_type cenv attr fenv ctx
       in
       let {
           Type_info.ti_aux_generics_args = rhs_aux_generics_args;
           Type_info.ti_generics_args = rhs_generics_args;
         } = rhs_ty in
       (* TODO: add lifetimes of rest of ty and rhs_ty *)
       (rhs_ty, rhs_generics_args @ rhs_aux_generics_args, [])
  in
  check_function_env2 fenv lts lt_constraints
    [Env.FnParamKindType rhs_ty] Meta_level.Meta ret_ty false;

  let new_ret_ty = post_check_function_return_type fenv ctx in  (* TODO *)

  register_copy_ctor_to_class_env cenv fenv;

  (fenv, rhs_ty, new_ret_ty)

(** for copy constructors **)
let define_trivial_copy_ctor_for_builtin ?(has_ptr_constraints=false)
                                         cenv extern_cname ctx =
  let (fenv, rhs_ty, ret_ty) =
    declare_checked_copy_ctor ~has_ptr_constraints:has_ptr_constraints
                              cenv ctx
  in

  let node = TAst.GenericFuncDef (None, (Loc.dummy, Some fenv)) in
  let detail =
    Env.FnRecordBuiltin (Env.FnDefDefaulted true,
                         Env.FnKindCopyConstructor None,
                         (Builtin_info.make_builtin_copy_ctor_name extern_cname))
  in
  complete_function_env fenv node ctor_id_name detail ctx

(* for type and void *)
let register_builtin_type name inner_name mangled_name
                          meta_level opt_layout
                          root_env type_gen =
  let create_extern_primitive_class name inner_name =
    let env_r = Env.ClassOp.empty_record name in
    let env =
      Env.create_context_env root_env name
                             (Env.Class (Env.empty_lookup_table ~init:0 (),
                                         env_r))
                             None
    in
    env.Env.mangled_name <- Some mangled_name;
    env.Env.meta_level <- meta_level;

    let loc = None in

    let lifetime_spec = [] in
    let node = TAst.ExternClassDefStmt (name, lifetime_spec, inner_name, None, None, (loc, Some env)) in

    let detail_r = Env.ClsRecordExtern {
                       Env.cls_e_name = inner_name;
                     } in
    complete_class_env env node detail_r opt_layout;
    env_r.Env.cls_traits <- {
      env_r.Env.cls_traits with
      Env.cls_traits_is_primitive = true;
      Env.cls_traits_is_always_value = true;
    };
    env
  in

  let loc = None in
  let id_name = Id_string.Pure name in
  let cenv = create_extern_primitive_class id_name inner_name in
  Env.add_inner_env root_env id_name cenv |> error_if_env_is_dupped loc;

  Type.Generator.generate_type type_gen
                               (Type_info.UniqueTy cenv)
                               []
                               [] (* TODO *)
                               {
                                 Type_attr.ta_ref_val = Type_attr.Val;
                                 Type_attr.ta_mut = Type_attr.Immutable;
                               }

let register_builtin_lifetime name lt root_env =
  let loc = None in
  let env = Env.create_context_env root_env name (Env.LifetimeVariable lt) loc in
  Env.add_inner_env root_env name env |> ignore;
  ()


let print_error_msg msg ctx =
  Printf.printf "\nCompilation Error!: %s\n\n" msg


let debug_print_meta_var uni_id ctx =
  let (_, ty_c) =
    Unification.search_type_until_terminal ctx.sc_unification_ctx uni_id
  in
  let (_, val_c) =
    Unification.search_value_until_terminal ctx.sc_unification_ctx uni_id
  in
  Debug.printf "uni_id(%d); type  is => %s\n" uni_id (
                 match ty_c with
                 | Unification.Val ty -> Type.to_string ty
                 | _ -> ">link or undef<"
               );
  Debug.printf "uni_id(%d); value is => %s\n" uni_id (
                 match val_c with
                 | Unification.Val value -> Ctfe_util.to_string value
                 | _ -> ">link or undef<"
               )
