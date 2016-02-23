(*
 *
 *)
type bulitin_class_info_t = {
  external_name : string;
  internal_name : string;
  mangled_name  : string;
}

(* "type" type will be hard coded in sema.ml *)
let type_type_i = {
  external_name = "type";
  internal_name = "__builtin_type_type";
  mangled_name = "";
}

(* these types will be defined in core/builtin.rill *)
let void_type_i = {
  external_name = "void";
  internal_name = "__builtin_void_type";
  mangled_name = "v";
}

let bool_type_i = {
  external_name = "bool";
  internal_name = "__builtin_bool_type";
  mangled_name = "b";
}

let int32_type_i = {
  external_name = "int32";
  internal_name = "__builtin_int32_type";
  mangled_name = "i";
}

let array_type_i = {
  external_name = "array";
  internal_name = "__builtin_array_type";
  mangled_name = "";
}


(*
 *
 *)
let entrypoint_name = "main"
