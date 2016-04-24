(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries
open Type_sets

module TAst = Tagged_ast

type env_t = TAst.t Env.env_t
type type_info_t = env_t Type.info_t
type ctfe_val_t = type_info_t Ctfe_value.t
type type_gen_t = env_t Type.Generator.t

type conv_filter_t = (type_info_t * env_t) option
type earg_t = TAst.ast * TAst.term_aux_t

type 'env ctx_t = {
  sc_root_env       : 'env;
  mutable sc_builtin_m_env  : 'env option;

  sc_module_bag         : 'env Module_info.Bag.t;
  sc_module_search_dirs : string list;

  (* ctfe engine *)
  sc_ctfe_engine    : Ctfe_engine.t;

  (* type sets *)
  sc_tsets          : 'env type_sets_t;

  (* for template *)
  sc_unification_ctx    : (type_info_t, ctfe_val_t) Unification.t;
}


let check_env env =
  Env.update_status env Env.Checking

let complete_env env node =
  Env.update_status env Env.Complete;
  Env.update_rel_ast env node

let check_is_args_valid ty =
  (* TODO: implement *)
  ()


let check_function_env env param_types return_type is_auto_return_type =
  let r = Env.FunctionOp.get_record env in
  r.Env.fn_param_types <- param_types;
  r.Env.fn_return_type <- return_type;
  r.Env.fn_is_auto_return_type <- is_auto_return_type;
  check_env env

let complete_function_env env node id_name f_detail ctx =
  let r = Env.FunctionOp.get_record env in

  r.Env.fn_detail <- f_detail;

  let m = (Env.get_full_module_name env) |> String.concat "." in
  Printf.printf "Complete function -> %s.%s\n" m (Nodes.string_of_id_string id_name);
  Type.print r.Env.fn_return_type;

  let _ = match id_name with
    | s when s = Nodes.Pure Builtin_info.entrypoint_name ->
       begin
         (* TODO: check param_types and return_type *)
         r.Env.fn_mangled <- Some Builtin_info.entrypoint_name
       end
    | _ ->
       begin
         let mangled =
           Mangle.s_of_function (Env.get_full_module_name env) id_name
                                r.Env.fn_template_vals
                                r.Env.fn_param_types r.Env.fn_return_type
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
  check_env env

let complete_class_env env node c_detail traits =
  let r = Env.ClassOp.get_record env in
  r.Env.cls_detail <- c_detail;
  r.Env.cls_traits <- Some traits;
  complete_env env node


let is_valid_type ty =
  let open Type_attr in
  let {
    ta_ref_val = rv;
    ta_mut = mut;
  } = ty.Type_info.ti_attr in
  (rv <> RefValUndef) && (mut <> MutUndef)

let assert_valid_type ty =
  assert (is_valid_type ty)


let rec split_aux auxs = match auxs with
  | [] -> ([], [], [], [])
  | (termc, vc, lt, ml) :: xs ->
     let (ts, vs, ls, ms) = split_aux xs in
     (termc::ts, vc::vs, lt::ls, ml::ms)


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


let ctor_name = Nodes.Pure "ctor"
let assign_name = Nodes.BinaryOp "="

(* default qual *)
let default_ty_attr = {
  Type_attr.ta_ref_val = Type_attr.Val;
  Type_attr.ta_mut = Type_attr.Const;
}
