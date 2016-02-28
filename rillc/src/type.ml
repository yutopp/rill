(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

type 'env info_t = {
  ti_id                 : type_id_ref_t option;
  ti_sort               : 'env type_sort_t;
  ti_template_args      : 'env ctfe_val_t list;
  ti_attr               : Type_attr.attr_t;
}

 and 'env ctfe_val_t = ('env info_t) Ctfe_value.t

 and 'env type_sort_t =
    UniqueTy of 'env
  | ClassSetTy of 'env
  | FunctionSetTy of 'env
  | Undef
  | NotDetermined of Unification.id_t


and type_id_ref_t = int64   (* type id is represented by int64 *)
module IdType = Int64
let is_type_id_signed = true


let type_sort ty =
  let {
    ti_sort = ts; _
  } = ty in
  ts

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
       has_same_class lhs rhs
     end
  | _ -> failwith "not supported"


let is_class_set ty =
  let ts = type_sort ty in
  match ts with
  | ClassSetTy _ -> true
  | _ -> false


let as_unique ty =
  match type_sort ty with
    UniqueTy c_env -> c_env
  | _ -> failwith "as_unique: not unique"


let undef_ty =
  {
    ti_id = None;
    ti_sort = Undef;
    ti_template_args = [];
    ti_attr = Type_attr.undef;
  }


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
      Printf.printf "debug / typeid = new %s / cur %s\n"
                    (IdType.to_string new_id) (IdType.to_string gen.gen_fresh_id);
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


    let find_type_by_cache_id gen t_id =
      try Hashtbl.find gen.cache_table t_id with
      | Not_found ->
         failwith "[ICE] Internal type id is not found"
  end
