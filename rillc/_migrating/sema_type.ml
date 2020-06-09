(*
 * Copyright yutopp 2017 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

let create_new_lt_var ?(tmp=false) lt_name parent_env =
  let ctx_env = parent_env.Env.context_env in
  let var_id = Lifetime.Var_id.generate () in
  let lt = match tmp with
      false -> Lifetime.LtVar (var_id, lt_name, ctx_env.Env.env_id, Int32.of_int 0, [var_id], Loc.dummy)
    | true -> Lifetime.LtVarPlaceholder (var_id, Loc.dummy)
  in
  lt


let make_class_type ?(new_instance=false) cenv attr env ctx =
  let open Sema_context in
  let open Type_sets in
  let {
      Type_attr.ta_ref_val = rv;
      Type_attr.ta_mut = mut;
    } = attr
  in
  let ts = Type_info.UniqueTy cenv in

  let cr = Env.ClassOp.get_record cenv in
  let template_args = cr.Env.cls_template_vals in

  let aux_generics_args = match rv with
    | Type_attr.Ref [] -> [create_new_lt_var ~tmp:new_instance (Id_string.Pure "`_") env]
    | Type_attr.Val -> []
    | _ -> failwith "[ICE]"
  in
  let generics_args =
    match new_instance with
    | true ->
       let gen_same_name_lt_var lt =
         match lt with
         | Lifetime.LtVar (_, spec, _, _, _, _) ->
            create_new_lt_var ~tmp:true spec cenv
         | _ -> failwith ""
       in
       cr.Env.cls_generics_vals |> List.map gen_same_name_lt_var
    | false ->
       cr.Env.cls_generics_vals
  in

  Type.Generator.generate_type ~aux_generics_args:aux_generics_args
                               ctx.sc_tsets.ts_type_gen
                               ts template_args generics_args attr

(*
 * exclude parameters which are not required to call.
 * (Ex. has a default value)
 * Currently, it is not supported.
 *)
let exclude_optional_params param_kinds =
  let rec exclude_optional_params' param_kinds acc =
    match param_kinds with
    | [] -> acc
    | (k :: ks) ->
       begin
         match k with
         | Env.FnParamKindType ty ->
            exclude_optional_params' ks (ty :: acc)
       end
  in
  exclude_optional_params' param_kinds []
  |> List.rev
