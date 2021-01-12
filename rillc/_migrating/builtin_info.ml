(*
 *
 *)
type builtin_class_layout = int * int (* bytes of size and align *)

type bulitin_class_info_t = {
  external_name : string;
  internal_name : string;
  mangled_name  : string;
}

(* "type" a type will be hard coded in sema.ml *)
let type_type_i = {
  external_name = "type";
  internal_name = "__builtin_type_type";
  mangled_name = "t";
}

let void_type_i = {
  external_name = "void";
  internal_name = "__builtin_void_type";
  mangled_name = "v";
}

(* these types will be defined in core/builtin.rill *)
let bool_type_i = {
  external_name = "bool";
  internal_name = "__builtin_bool_type";
  mangled_name = "b";
}

let uint8_type_i = {
  external_name = "uint8";
  internal_name = "__builtin_uint8_type";
  mangled_name = "uu";
}

let int32_type_i = {
  external_name = "int32";
  internal_name = "__builtin_int32_type";
  mangled_name = "is";
}

let uint32_type_i = {
  external_name = "uint32";
  internal_name = "__builtin_uint32_type";
  mangled_name = "iu";
}


let array_type_i = {
  external_name = "array";
  internal_name = "__builtin_array_type";
  mangled_name = "";
}

let untyped_raw_ptr_type_i = {
  external_name = "untyped_raw_ptr";
  internal_name = "__builtin_untyped_raw_ptr_type";
  mangled_name = "";
}

let raw_ptr_type_i = {
  external_name = "raw_ptr";
  internal_name = "__builtin_raw_ptr_type";
  mangled_name = "";
}

(*
 *
 *)
let entrypoint_name = "main"
let entrypoint_export_name = "_Rill_main"

(*
 *)
let make_builtin_default_ctor_name base =
  base ^ "_default_ctor"

let make_builtin_copy_ctor_name base =
  base ^ "_copy_ctor"

let make_builtin_move_ctor_name base =
  base ^ "_move_ctor"

let make_builtin_copy_assign_name base =
  base ^ "_copy_assign"

let make_builtin_move_assign_name base =
  base ^ "_move_assign"
