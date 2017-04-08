open Batteries

let rec s_of_ctfe_val value tset =
  let open Type_sets in
  let (ty, sv) = match value with
    | Ctfe_value.Type ty ->
       (tset.ts_type_type, s_of_type ty)
    | Ctfe_value.Bool _ ->
       (!(tset.ts_bool_type_holder), (Ctfe_util.to_string value))
    | Ctfe_value.Int32 _ ->
       (!(tset.ts_int32_type_holder), (Ctfe_util.to_string value))
    | Ctfe_value.Uint32 _ ->
       (!(tset.ts_uint32_type_holder), (Ctfe_util.to_string value))
  (*| Ctfe_value.Int64 _ ->
       !(tset.ts_int64_type_holder)*)
    | _ -> failwith "[ICE] s_of_ctfe_val"
  in
  Printf.sprintf "V%s%s" (s_of_type ty) sv

and s_of_type ty =
  let c_env = Type.as_unique ty in
  let symbol = c_env.Env.mangled_name |> Option.get in

  let ta = ty.Type_info.ti_attr in
  let rv_s = match ta.Type_attr.ta_ref_val with
    | Type_attr.Val -> "v"
    | Type_attr.Ref _ -> "r"
    | Type_attr.XRef -> "x"
    | _ -> failwith ("Mangle.s_of_type / has no rv : " ^ symbol)
  in
  let mut_s = match ta.Type_attr.ta_mut with
    | Type_attr.Immutable -> "i"
    | Type_attr.Const -> "c"
    | Type_attr.Mutable -> "m"
    | _ -> failwith ("Mangle.s_of_type / has no mut : " ^ symbol)
  in

  Printf.sprintf "T%s%s%s" rv_s mut_s symbol


and s_of_template_args args tset =
  match List.length args with
  | 0 -> ""
  | n ->
     let ts = args |> List.map (fun x -> s_of_ctfe_val x tset) in
     let s = String.concat "" ts in
     Printf.sprintf "_T%d_%s" n s


and s_of_string s =
  Printf.sprintf "%d%s" (String.length s) s


and s_of_symbol sym =
  let s = Id_string.to_string sym in
  s_of_string s


and s_of_id_string full_module_name id_name =
  let mod_num = List.length full_module_name in
  let s_name = id_name |> s_of_symbol in
  match full_module_name with
  | ["core"; "basic_types"] ->  (* TODO: fix *)
     Printf.sprintf "B%s" s_name
  | _ ->
     let s_mod_name = full_module_name |> List.map s_of_string |> String.concat "" in
     Printf.sprintf "M%d_%s_%s" mod_num s_mod_name s_name


and s_of_class full_module_name id_name template_args tset =
  let s_sym = s_of_id_string full_module_name id_name in
  let s_targs = s_of_template_args template_args tset in
  Printf.sprintf "C%s%s" s_sym s_targs

and s_of_param param =
  match param with
  | Env.FnParamKindType ty -> s_of_type ty

and s_of_function full_module_name id_name
                  template_args param_kinds return_type
                  tset =
  let s_sym = s_of_id_string full_module_name id_name in
  let s_targs = s_of_template_args template_args tset in
  let s_args_tys = param_kinds |> List.map s_of_param |> String.concat "" in
  let s_ret_ty = return_type |> s_of_type in
  Printf.sprintf "_Rill_%s%sF%sZ%s" s_sym s_targs s_args_tys s_ret_ty
