(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

type 'env type_info = 'env Type.info_t

type 'env type_sets_t = {
  ts_type_gen       : 'env Type.Generator.t;

  (* special builtin primitive type *)
  ts_type_type      : 'env type_info;
  ts_void_type      : 'env type_info;

  (* buildin primitive types *)
  ts_bool_type_holder               : 'env type_info ref;
  ts_uint8_type_holder              : 'env type_info ref;
  ts_int32_type_holder              : 'env type_info ref;
  ts_uint32_type_holder             : 'env type_info ref;
  ts_array_type_holder              : 'env type_info ref;
  ts_untyped_raw_ptr_type_holder    : 'env type_info ref;
  ts_raw_ptr_type_holder            : 'env type_info ref;
}

let make gen ttype vtype =
  {
    ts_type_gen = gen;
    ts_type_type = ttype;
    ts_void_type = vtype;

    ts_bool_type_holder = ref Type_info.undef_ty;
    ts_uint8_type_holder = ref Type_info.undef_ty;
    ts_int32_type_holder = ref Type_info.undef_ty;
    ts_uint32_type_holder = ref Type_info.undef_ty;
    ts_array_type_holder = ref Type_info.undef_ty;
    ts_untyped_raw_ptr_type_holder = ref Type_info.undef_ty;
    ts_raw_ptr_type_holder = ref Type_info.undef_ty;
  }


let is_type_type ty tsets =
  Type.has_same_class ty tsets.ts_type_type
