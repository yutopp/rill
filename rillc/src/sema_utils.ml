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


let check_function_env env param_kinds ml return_type is_auto_return_type =
  let r = Env.FunctionOp.get_record env in
  r.Env.fn_param_kinds <- param_kinds;
  r.Env.fn_return_type <- return_type;
  r.Env.fn_is_auto_return_type <- is_auto_return_type;
  check_env env ml

let complete_function_env env node id_name f_detail ctx =
  let r = Env.FunctionOp.get_record env in

  r.Env.fn_detail <- f_detail;

  let m = (Env.get_full_module_name env) |> String.concat "." in
  Debug.printf "Complete function -> %s.%s\n" m (Nodes.string_of_id_string id_name);
  Type.debug_print r.Env.fn_return_type;

  let _ = match id_name with
    | Nodes.Pure s when s = Builtin_info.entrypoint_name ->
       begin
         (* TODO: check param_types and return_type *)
         r.Env.fn_mangled <- Some Builtin_info.entrypoint_name
       end
    | _ ->
       begin
         let mangled =
           Mangle.s_of_function (Env.get_full_module_name env) id_name
                                r.Env.fn_template_vals
                                r.Env.fn_param_kinds r.Env.fn_return_type
                                ctx.sc_tsets
         in
         r.Env.fn_mangled <- Some mangled
       end
  in
  complete_env env node


let check_class_env env ctx =
  let r = Env.ClassOp.get_record env in
  let id_name = r.Env.cls_name in
  let template_args = r.Env.cls_template_vals in
  let mangled =
    Mangle.s_of_class (Env.get_full_module_name env)
                      id_name template_args ctx.sc_tsets
  in
  r.Env.cls_mangled <- Some mangled;
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
     failwith "[ICE] default ctor is already registered"


let is_valid_type ty =
  let open Type_attr in
  let {
    ta_ref_val = rv;
    ta_mut = mut;
  } = ty.Type_info.ti_attr in
  (rv <> RefValUndef) && (mut <> MutUndef)

let assert_valid_type ty =
  assert (is_valid_type ty)


let check_is_args_valid ty =
  (* TODO: implement *)
  ()


let register_builtin_type name inner_name mangled_name
                          meta_level opt_layout
                          root_env type_gen =
  let create_extern_primitive_class name inner_name =
    let env_r = Env.ClassOp.empty_record name in
    env_r.Env.cls_mangled <- Some mangled_name;

    let env = Env.create_context_env root_env (
                                       Env.Class (Env.empty_lookup_table ~init:0 (),
                                                  env_r)
                                     )
                                     None
    in
    env.Env.meta_level <- meta_level;

    let node = TAst.ExternClassDefStmt (name, inner_name, None, Some env) in

    let detail_r = Env.ClsRecordExtern {
                       Env.cls_e_name = inner_name;
                     } in
    complete_class_env env node detail_r opt_layout;
    env_r.Env.cls_traits <- {
      env_r.Env.cls_traits with
      Env.cls_traits_is_primitive = true;
    };
    env
  in

  let id_name = Nodes.Pure name in
  let cenv = create_extern_primitive_class id_name inner_name in
  Env.add_inner_env root_env name cenv;

  Type.Generator.generate_type type_gen
                               (Type_info.UniqueTy cenv)
                               []
                               {
                                 Type_attr.ta_ref_val = Type_attr.Val;
                                 Type_attr.ta_mut = Type_attr.Immutable;
                               }


let rec split_aux auxs = match auxs with
  | [] -> ([], [], [], [], [])
  | (termc, vc, lt, ml, pos) :: xs ->
     let (ts, vs, ls, ms, ps) = split_aux xs in
     (termc::ts, vc::vs, lt::ls, ml::ms, pos::ps)

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
