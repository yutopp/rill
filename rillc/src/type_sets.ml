(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

type 'env type_info = 'env Type.info_t

type 'env type_sets_t = {
  ts_type_gen               : 'env Type.Generator.t;

  (* buildin primitive types *)
  mutable ts_type_type      : 'env type_info;
  mutable ts_void_type      : 'env type_info;
  ts_int_type       : 'env type_info ref;
}


let is_type_type ty tsets =
  Type.has_same_class ty tsets.ts_type_type
