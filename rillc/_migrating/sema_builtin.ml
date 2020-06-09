open Sema_context
open Type_sets

let get_builtin_bool_type attr ctx : type_info_t =
  let ty = !(ctx.sc_tsets.ts_bool_type_holder) in
  assert (not @@ Type.is_undef ty);
  ty

let get_builtin_int_type ~bits ~signed attr ctx : type_info_t =
  let ty = match bits with
    | 8 -> if signed then
             failwith "[ICE] get builtin int type"
           else
             !(ctx.sc_tsets.ts_uint8_type_holder)
    | 32 -> if signed then
              !(ctx.sc_tsets.ts_int32_type_holder)
            else
              !(ctx.sc_tsets.ts_uint32_type_holder)
    | _ -> failwith "[ICE] unsupported bits size"
  in
  assert (not @@ Type.is_undef ty);
  (*Type.Generator.update_attr_r ctx.sc_tsets.ts_type_gen ty attr*)
  ty

let get_builtin_int32_type attr ctx : type_info_t =
  get_builtin_int_type ~bits:32 ~signed:true attr ctx

let get_builtin_uint32_type attr ctx : type_info_t =
  get_builtin_int_type ~bits:32 ~signed:false attr ctx
