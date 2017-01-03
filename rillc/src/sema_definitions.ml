(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries
open Stdint
open Sema_context

type type_gen_t = env_t Type.Generator.t


let make_earg ast aux constraints =
  (ast, aux, constraints)

let rec split3' lists (acca,accb,accc) =
  match lists with
  | [] -> (acca,accb,accc)
  | (a, b, c)::rest -> split3' rest (a::acca, b::accb, c::accc)

let split3 lists =
  let (a, b, c) = split3' lists ([], [], []) in
  (a |> List.rev, b |> List.rev, c |> List.rev)




let ctor_name = "ctor"
let ctor_id_name = Id_string.Pure ctor_name

let dtor_name = "dtor"
let dtor_id_name = Id_string.Pure dtor_name

let assign_name = Id_string.BinaryOp "="

(* default qual *)
let default_ty_attr = {
  Type_attr.ta_ref_val = Type_attr.Val;
  Type_attr.ta_mut = Type_attr.Const;
}

exception Instantiation_failed of string
exception Template_type_mismatch

exception Normal_error of error_msg_t
let error err =
  raise (Normal_error err)

let error_msg msg =
  raise (Normal_error (Error_msg.Msg msg))

exception Fatal_error of error_msg_t
let fatal_error err =
  raise (Fatal_error err)

let fatal_error_msg msg =
  raise (Fatal_error (Error_msg.Msg msg))


let calc_member_layouts member_vars : (Uint32.t * Uint32.t) list =
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
  List.map f member_vars

let calc_max_align member_layouts =
  let f max (_, valign) =
    Uint32.(if valign > max then valign else max)
  in
  List.fold_left f (Uint32.zero) member_layouts

let calc_class_layouts (member_layouts: (Uint32.t * Uint32.t) list) =
  let open Uint32 in

  let max_align = calc_max_align member_layouts in

  let f (csize, offsets) (vsize, valign) =
    let pad = match rem csize valign with
      | n when n = zero -> n
      | n -> valign - n
    in
    let offset = csize + pad in
    (offset + vsize, offset :: offsets)
  in
  let (csize, offsets) =
    List.fold_left f (zero, []) member_layouts
  in

  match max_align with
  | n when n = zero ->
     (zero, one, [])
  | _ ->
     let last_pad = match rem csize max_align with
       | n when n = zero -> zero
       | n -> max_align - n
     in
     let aligned_csize = csize + last_pad in
     (aligned_csize, max_align, offsets |> List.rev)
