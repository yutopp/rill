(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Type_info
type 'e info_t = 'e Type_info.t


let type_sort ty =
  let {
    ti_sort = ts;
  } = ty in
  ts

let type_attr ty =
  let {
    ti_attr = ta;
  } = ty in
  ta

let is_unique_ty ty =
  match type_sort ty with
    UniqueTy _ -> true
  | _ -> false

let is_undef ty =
  match type_sort ty with
    Undef -> true
  | _ -> false

let has_same_class lhs rhs =
  match (type_sort lhs, type_sort rhs) with
  | (UniqueTy lhs_e, UniqueTy rhs_e) ->
     lhs_e == rhs_e (* compare envs by reference *)
  | _ -> false


let is_same lhs rhs =
  match (type_sort lhs, type_sort rhs) with
  | (UniqueTy lhs_r, UniqueTy rhs_r) ->
     begin
       (* TODO: implement correctly *)
       has_same_class lhs rhs &&
         Type_attr.is_same lhs.ti_attr rhs.ti_attr
     end
  | _ -> failwith "not supported"

let is_same_class_ref lhs rhs =
  match (type_attr lhs, type_attr rhs) with
  | ({Type_attr.ta_ref_val = Type_attr.Ref}, {Type_attr.ta_ref_val = Type_attr.Ref}) ->
     has_same_class lhs rhs
  | _ -> false

let is_class_set ty =
  let ts = type_sort ty in
  match ts with
  | ClassSetTy _ -> true
  | _ -> false


let as_unique ty =
  match type_sort ty with
    UniqueTy c_env -> c_env
  | _ -> failwith "as_unique: not unique"


let size_of ty =
  let cenv = as_unique ty in
  let cr = Env.ClassOp.get_record cenv in
  match cr.Env.cls_size with
  | Some v -> v
  | None -> failwith "[ICE] cls size is not set"

let align_of ty =
  let cenv = as_unique ty in
  let cr = Env.ClassOp.get_record cenv in
  match cr.Env.cls_align with
  | Some v -> v
  | None -> failwith "[ICE] cls align is not set"

let element_size_of ty =
  let open Stdint.Uint32 in
  let size = size_of ty in
  let align = align_of ty in
  let padding = rem size align in
  size + padding


module Generator =
  struct
    type 'env id_record_table_t =
        (type_id_ref_t, 'env info_t) Hashtbl.t

    type 'env t = {
      mutable gen_fresh_id  : type_id_ref_t;
      cache_table           : 'env id_record_table_t;
    }

    let default () =
      {
        gen_fresh_id = IdType.zero;
        cache_table = Hashtbl.create 10;
      }

    let make_fresh_id gen =
      let new_id = gen.gen_fresh_id in
      if new_id = IdType.max_int then
        failwith "[ICE] Internal type id is reached to max id...";
      gen.gen_fresh_id <- IdType.succ gen.gen_fresh_id; (* update fresh id *)
      new_id

    let generate_type gen type_sort template_args ty_attr =
      (* TODO: implement cache *)
      let tid = make_fresh_id gen in
      let ty = {
        ti_id = Some tid;
        ti_sort = type_sort;
        ti_template_args = template_args;
        ti_attr = ty_attr;
      } in
      Hashtbl.add gen.cache_table tid ty;
      ty

    let register_type gen ty =
      generate_type gen ty.ti_sort ty.ti_template_args ty.ti_attr

    let update_attr_r gen ty attr =
      register_type gen { ty with ti_attr = attr; }

    let update_attr gen ty at_rv at_mut =
      let attr = {
        Type_attr.ta_ref_val = at_rv;
        Type_attr.ta_mut = at_mut;
      } in
      update_attr_r gen ty attr

    let add_reference gen ty =
      let attr = type_attr ty in
      update_attr_r gen ty { attr with
                             Type_attr.ta_ref_val = Type_attr.Ref
                           }

    let find_type_by_cache_id gen t_id =
      try Hashtbl.find gen.cache_table t_id with
      | Not_found ->
         failwith "[ICE] Internal type id is not found"
  end


let rec to_string ty =
  let open Type_attr in
  let rv_s = match ty.ti_attr.ta_ref_val with
      Ref -> "ref"
    | Val -> "val"
    | XRef -> "xref"
    | _ -> "undef"
  in
  let mut_s = match ty.ti_attr.ta_mut with
      Immutable -> "immutable"
    | Const -> "const"
    | Mutable -> "mutable"
    | _ -> "undef"
  in

  let base_s = match type_sort ty with
    | UniqueTy cenv ->
       begin
         let cls_r = Env.ClassOp.get_record cenv in
         let name = Nodes.string_of_id_string cls_r.Env.cls_name in
         name
       end
    | FunctionSetTy _ -> "@function set@"
    | ClassSetTy _ -> "@class set@"
    | Undef -> "@undef@"
    | NotDetermined uni_id -> Printf.sprintf "@not determined(%d)@" uni_id
  in

  let targs_s = ty.ti_template_args |> List.map to_s_ctfe in
  let targs_s = if (List.length targs_s > 0)
                then
                  Printf.sprintf "!(%s)" (String.concat ", " targs_s)
                else ""
  in
  Printf.sprintf "%s(%s(%s%s))" rv_s mut_s base_s targs_s


and to_s_ctfe value =
  match value with
  | Ctfe_value.Type ty -> to_string ty
  | Ctfe_value.Bool b -> string_of_bool b
  | Ctfe_value.Int32 n -> Int32.to_string n
  | Ctfe_value.Uint32 n -> Stdint.Uint32.to_string n
  | Ctfe_value.Int64 n -> Int64.to_string n
  | Ctfe_value.Uint64 n -> Stdint.Uint64.to_string n
  | Ctfe_value.Undef _ -> "%%ctfe_val(undef)"

and debug_print ty =
  Debug.printf "%s\n" (to_string ty)
