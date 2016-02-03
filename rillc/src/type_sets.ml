type 'env type_info = 'env Type.info_t

type 'env type_sets_t = {
  ts_type_gen               : 'env Type.Generator.t;

  (* buildin primitive types *)
  mutable ts_type_type      : 'env type_info;
  mutable ts_void_type      : 'env type_info;
  mutable ts_int_type       : 'env type_info;
}


let is_type_type ty tsets =
  Type.has_same_class ty tsets.ts_type_type
