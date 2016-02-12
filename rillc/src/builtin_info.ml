(*
 *
 *)
type bulitin_function_info_t = {
  external_name  : string;
  internal_name  : string;
}

(* "type" type will be hard coded in sema.ml *)
let type_type_i = {
  external_name = "type";
  internal_name = "__builtin_type_type"
}

(* these types will be defined in core/builtin.rill *)
let void_type_i = {
  external_name = "void";
  internal_name = "__builtin_void_type"
}

let int32_type_i = {
  external_name = "int32";
  internal_name = "__builtin_int32_type"
}

let array_type_i = {
  external_name = "array";
  internal_name = "__builtin_array_type"
}


(*
 *
 *)
let entrypoint_name = "main"
