open Batteries

let rec s_of_ctfe_val value tset =
  let open Type_sets in
  match value with
  | Ctfe_value.Type ty -> s_of_type ty

  | Ctfe_value.Int32 i32 ->
     begin
       Printf.sprintf "V%si%s"
                      (s_of_ctfe_val (Ctfe_value.Type !(tset.ts_int32_type_holder)) tset)
                      (Int32.to_string i32)
     end

  | _ -> failwith "[ICE] s_of_ctfe_val / unknown"


and s_of_type ty =
  let c_env = Type.as_unique ty in

  let cr = Env.ClassOp.get_record c_env in
  let symbol = Option.get cr.Env.cls_mangled in
  Printf.sprintf "T%d%s" (String.length symbol) symbol


and s_of_template_args args tset =
  let ts = args |> List.map @@ fun x -> s_of_ctfe_val x tset in
  let s = String.concat "" ts in
  Printf.sprintf "_T%d%s" (List.length ts) s


and s_of_string s =
  Printf.sprintf "%d%s" (String.length s) s


and s_of_symbol sym =
  let s = Nodes.string_of_id_string sym in
  s_of_string s


and s_of_function full_module_name id_name
                  template_args args_types return_type
                  tset =
  let s_mod_name = full_module_name |> List.map s_of_string |> String.concat "" in
  let s_name = id_name |> s_of_symbol in
  let s_targs = s_of_template_args template_args tset in
  let s_args_tys = args_types |> List.map s_of_type |> String.concat "" in
  let s_ret_ty = return_type |> s_of_type in
  Printf.sprintf "_Rill_%s%s%sF%sZ%s" s_mod_name s_name s_targs s_args_tys s_ret_ty
