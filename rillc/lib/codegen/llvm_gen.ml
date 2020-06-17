(*
 * Copyright yutopp 2019 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module Span = Common.Span
module Diagnostics = Common.Diagnostics
module Builtin = Sema.Builtin
module L = Llvm
module L_bitwriter = Llvm_bitwriter

module Module = struct
  type t = L.llmodule

  let pp ppf m = Caml.Format.fprintf ppf "%s" (L.string_of_llmodule m)
end

type ctx_t = {
  ds : Diagnostics.t;
  subst : Typing.Subst.t;
  ll_ctx : L.llcontext;
  builtin : Builtin.t;
}

let context ~ds ~subst ~builtin =
  let ll_ctx = L.create_context () in
  { ds; subst; builtin; ll_ctx }

module Env = struct
  type t = {
    types : type_t Map.M(Int).t;
    funcs : func_t Map.M(String).t;
    intrinsics : Llvm_gen_intrinsics.t;
    local_vars : var_t Map.M(String).t;
    bb : L.llbasicblock Map.M(String).t;
  }

  and type_t = { ll_ty : L.lltype }

  and func_t = { ty : Typing.Type.t; kind : func_kind_t }

  and func_kind_t =
    | FuncLLVMDecl of L.llvalue
    | FuncLLVMDef of L.llvalue
    | FuncBuiltin of (L.llvalue array -> L.llbuilder -> L.llvalue)

  and var_t = { ll_v : L.llvalue; as_treat : Value_category.as_treat_t }

  let create ~intrinsics =
    {
      types = Map.empty (module Int);
      funcs = Map.empty (module String);
      intrinsics;
      local_vars = Map.empty (module String);
      bb = Map.empty (module String);
    }

  let get_type env name = Map.find_exn env.types name

  let set_type env struct_tag ll_ty =
    { env with types = Map.set ~key:struct_tag ~data:ll_ty env.types }

  let get_func env name = Map.find_exn env.funcs name

  let set_func env name ll_f =
    { env with funcs = Map.set ~key:name ~data:ll_f env.funcs }

  let get_local_var env name = Map.find_exn env.local_vars name

  let set_local_var env name value =
    { env with local_vars = Map.set ~key:name ~data:value env.local_vars }

  let get_bb env name = Map.find_exn env.bb name

  let set_bb env name ll_bb =
    { env with bb = Map.set ~key:name ~data:ll_bb env.bb }
end

let rec to_llty ~ctx ~env ty : L.lltype =
  match Typing.Subst.subst_type ctx.subst ty with
  | Typing.Type.{ ty = Unit; _ } -> L.void_type ctx.ll_ctx
  | Typing.Type.{ ty = Bool; _ } -> L.integer_type ctx.ll_ctx 1
  | Typing.Type.{ ty = Int; _ } -> L.integer_type ctx.ll_ctx 32
  | Typing.Type.{ ty = String; _ } -> L.pointer_type (L.i8_type ctx.ll_ctx)
  | Typing.Type.{ ty = Array { elem; n }; _ } ->
      let elem_ll_ty = to_llty ~ctx ~env elem in
      L.array_type elem_ll_ty n
  | Typing.Type.{ ty = Func { params; ret; _ }; _ } ->
      let params_tys = List.map ~f:(to_llty ~ctx ~env) params in
      let (params_tys, ret_ty) =
        let ret_ty = to_llty ~ctx ~env ret in
        match Value_category.memory_of ~subst:ctx.subst ret with
        | Value_category.MemPrimitive -> (params_tys, ret_ty)
        | Value_category.MemMemory ->
            (* 1st args will be return type storage *)
            let params_tys =
              let ll_ty = L.pointer_type ret_ty in
              ll_ty :: params_tys
            in
            let ret_ty = L.void_type ctx.ll_ctx in
            (params_tys, ret_ty)
      in
      L.function_type ret_ty (Array.of_list params_tys)
  | Typing.Type.{ ty = Pointer { elem; _ }; _ } ->
      let elem_ll_ty = to_llty ~ctx ~env elem in
      L.pointer_type elem_ll_ty
  | Typing.Type.{ ty = Struct { tag }; _ } ->
      let Env.{ ll_ty } = Env.get_type env tag in
      ll_ty
  | _ ->
      failwith
        (Printf.sprintf "[ICE] not supported type: %s"
           (Typing.Type.to_string ty))

let find_builtin builtin_name =
  match builtin_name with
  | "%op_bin_add" ->
      fun args builder ->
        let a = args.(0) in
        let b = args.(1) in
        L.build_add a b "" builder
  | "%op_bin_sub" ->
      fun args builder ->
        let a = args.(0) in
        let b = args.(1) in
        L.build_sub a b "" builder
  | "%op_bin_mul" ->
      fun args builder ->
        let a = args.(0) in
        let b = args.(1) in
        L.build_mul a b "" builder
  | "%op_bin_equals" ->
      fun args builder ->
        let a = args.(0) in
        let b = args.(1) in
        L.build_icmp L.Icmp.Eq a b "" builder
  | _ -> failwith builtin_name

let conv_val_ptr ctx ll_builder ((param_ty, value) : Typing.Type.t * Env.var_t)
    : L.llvalue =
  let target_t = Value_category.should_treat ~subst:ctx.subst param_ty in
  match (target_t, value) with
  | Value_category.(AsVal, Env.{ ll_v; as_treat = AsVal })
  | Value_category.(AsPtr _, Env.{ ll_v; as_treat = AsPtr _ }) ->
      ll_v
  | Value_category.(AsVal, Env.{ ll_v; as_treat = AsPtr _ }) ->
      L.build_load ll_v "" ll_builder
  | Value_category.(AsPtr _, Env.{ ll_v; as_treat = AsVal }) ->
      failwith "[ICE] ?"

let load_if_ref ll_builder v =
  match v with
  | Env.{ ll_v; as_treat = Value_category.AsPtr _ } ->
      L.build_load ll_v "" ll_builder
  | Env.{ ll_v; as_treat = Value_category.AsVal } -> ll_v

let assume_ref v =
  match v with
  | Env.{ ll_v; as_treat = Value_category.AsPtr _ } -> ll_v
  | Env.{ ll_v; as_treat = Value_category.AsVal } ->
      failwith (Printf.sprintf "[ICE] not ref: %s" (L.string_of_llvalue ll_v))

let assume_val v =
  match v with
  | Env.{ ll_v; as_treat = Value_category.AsVal } -> ll_v
  | Env.{ ll_v; as_treat = Value_category.AsPtr _ } ->
      failwith (Printf.sprintf "[ICE] not val: %s" (L.string_of_llvalue ll_v))

let blit_term_opt ~env ll_builder dst_term src_term =
  let module Vc = Value_category in
  match (dst_term, src_term) with
  (* primitive mem <- primitive mem: load/store *)
  | ( Some Env.{ ll_v = dst; as_treat = Vc.(AsPtr MemPrimitive) },
      Env.{ ll_v = src; as_treat = Vc.(AsPtr MemPrimitive) } ) ->
      let loaded = L.build_load src "" ll_builder in
      let _ll_v : L.llvalue = L.build_store loaded dst ll_builder in
      Env.{ ll_v = dst; as_treat = Vc.(AsPtr MemPrimitive) }
  (* primitive mem <- primitive val: store *)
  | ( Some Env.{ ll_v = dst; as_treat = Vc.(AsPtr MemPrimitive) },
      Env.{ ll_v = src; as_treat = Vc.AsVal } ) ->
      let _ll_v : L.llvalue = L.build_store src dst ll_builder in
      Env.{ ll_v = dst; as_treat = Vc.(AsPtr MemPrimitive) }
  (* memory mem <- memory mem: memcpy *)
  | ( Some Env.{ ll_v = dst; as_treat = Vc.(AsPtr MemMemory) },
      Env.{ ll_v = src; as_treat = Vc.(AsPtr MemMemory) } ) ->
      let inst = env.Env.intrinsics.Llvm_gen_intrinsics.memcpy_i32 in
      (* TODO: fix size and align *)
      let ll_v = inst ll_builder dst src 0 4 false in
      Env.{ ll_v = dst; as_treat = Vc.(AsPtr MemPrimitive) }
  (* val <- *: replace *)
  | (Some Env.{ as_treat = Vc.AsVal; _ }, _) ->
      (* re-use generate term *)
      src_term
  | (None, _) ->
      (* passthrough *)
      src_term
  | _ ->
      failwith
        (Printf.sprintf "[ICE] cannot blit: %s <- %s"
           (Option.value_map dst_term ~default:"NONE" ~f:(fun t ->
                Vc.show_as_treat_t t.Env.as_treat))
           (Vc.show_as_treat_t src_term.Env.as_treat))

let construct_value ~ctx ~env ~ll_holder ~local ll_builder v ty =
  let into_ref ll_v =
    match ll_holder with
    | Some storage ->
        let _ll_v : L.llvalue = L.build_store ll_v storage ll_builder in
        let mem = Value_category.memory_of ~subst:ctx.subst ty in
        Env.{ ll_v = storage; as_treat = Value_category.AsPtr mem }
    | None -> Env.{ ll_v; as_treat = Value_category.AsVal }
  in
  match v with
  | Rir.Term.ValueBool v ->
      let t =
        let ll_v = L.const_int (to_llty ~ctx ~env ty) (if v then 1 else 0) in
        Env.{ ll_v; as_treat = Value_category.AsVal }
      in
      blit_term_opt ~env ll_builder local t
  | Rir.Term.ValueInt v ->
      let t =
        let ll_v = L.const_int (to_llty ~ctx ~env ty) v in
        Env.{ ll_v; as_treat = Value_category.AsVal }
      in
      blit_term_opt ~env ll_builder local t
  | Rir.Term.ValueString v ->
      (* L.const_stringz ctx.ll_ctx v *)
      L.build_global_stringptr v "" ll_builder |> into_ref
  | Rir.Term.ValueUnit -> L.const_null (to_llty ~ctx ~env ty) |> into_ref
  | Rir.Term.ValueArrayElem elems ->
      let storage =
        match ll_holder with Some mem -> mem | None -> failwith "[ICE]"
      in
      let zero = L.const_int (L.i32_type ctx.ll_ctx) 0 in
      List.iteri elems ~f:(fun i elem ->
          let ll_index = L.const_int (L.i32_type ctx.ll_ctx) i in
          let ll_sto =
            L.build_in_bounds_gep storage [| zero; ll_index |] "" ll_builder
          in
          let ll_elem = Env.get_local_var env elem in
          match ll_elem with
          | Env.{ ll_v = ll_rhs; as_treat = Value_category.AsPtr _ } ->
              let ll_rhs_v = L.build_load ll_rhs "" ll_builder in
              let _ll_v : L.llvalue =
                L.build_store ll_rhs_v ll_sto ll_builder
              in
              ()
          | Env.{ ll_v = ll_rhs; as_treat = Value_category.AsVal } ->
              let _ll_v : L.llvalue = L.build_store ll_rhs ll_sto ll_builder in
              ());
      let mem = Value_category.memory_of ~subst:ctx.subst ty in
      Env.{ ll_v = storage; as_treat = Value_category.AsPtr mem }

let construct_term_assign_args ~ctx ll_builder params args =
  List.zip params args |> function
  | List.Or_unequal_lengths.Ok xs ->
      List.map xs ~f:(conv_val_ptr ctx ll_builder)
  | List.Or_unequal_lengths.Unequal_lengths ->
      failwith
        (Printf.sprintf "[ICE] param length are different %d != %d"
           (List.length params) (List.length args))

let construct_term ~ctx ~env ~ll_holder ~local ll_f ll_builder term : Env.var_t
    =
  let module Vc = Value_category in
  match term with
  (* *)
  | Rir.Term.{ kind = Call (callee, args); ty; _ } ->
      let Env.{ ty = f_ty; kind = f_kind } = Env.get_func env callee in
      let (f_params_tys, f_ret_ty) = Typing.Type.assume_func_ty f_ty in

      let arg_values = List.map args ~f:(Env.get_local_var env) in
      let ll_args =
        construct_term_assign_args ~ctx ll_builder f_params_tys arg_values
      in

      let ret_val_memory = Vc.memory_of ~subst:ctx.subst f_ret_ty in
      let ll_args =
        match ret_val_memory with
        | Value_category.MemPrimitive -> ll_args
        | Value_category.MemMemory ->
            let ll_v =
              match local with
              | Some Env.{ ll_v; as_treat = Vc.(AsPtr MemMemory) } -> ll_v
              | _ -> failwith "[ICE]"
            in
            ll_v :: ll_args
      in

      let ll_args = List.to_array ll_args in
      let ll_v =
        match f_kind with
        | Env.FuncLLVMDecl ll_callee | Env.FuncLLVMDef ll_callee ->
            L.build_call ll_callee ll_args "" ll_builder
        | Env.FuncBuiltin pass -> pass ll_args ll_builder
      in
      let value =
        match ret_val_memory with
        | Vc.MemPrimitive ->
            let t = Env.{ ll_v; as_treat = Vc.(AsVal) } in
            blit_term_opt ~env ll_builder local t
        | Vc.MemMemory ->
            (* A return value is already stored to local *)
            Option.value_exn local
      in
      value
  (* *)
  | Rir.Term.{ kind = Index (elems, index); ty; _ } ->
      let ll_elems = Env.get_local_var env elems |> assume_ref in
      let ll_index = Env.get_local_var env index |> load_if_ref ll_builder in

      let zero = L.const_int (L.i32_type ctx.ll_ctx) 0 in
      let ll_v =
        L.build_in_bounds_gep ll_elems [| zero; ll_index |] "" ll_builder
      in
      let value =
        let mem = Value_category.memory_of ~subst:ctx.subst ty in
        match ll_holder with
        | Some storage ->
            let _ll_v : L.llvalue = L.build_store ll_v storage ll_builder in
            Env.{ ll_v = storage; as_treat = Value_category.AsPtr mem }
        | None -> Env.{ ll_v; as_treat = Value_category.AsPtr mem }
      in
      value
  (* *)
  | Rir.Term.{ kind = Ref name; ty; _ } ->
      let ll_v = Env.get_local_var env name |> assume_ref in
      let t =
        (* Treat values as VALUES of Pointer *)
        Env.{ ll_v; as_treat = Value_category.AsVal }
      in
      blit_term_opt ~env ll_builder local t
  (* *)
  | Rir.Term.{ kind = Deref name; ty; _ } ->
      let ll_v = Env.get_local_var env name |> assume_val in
      let t =
        (* Treat values as Ptr *)
        let mem = Value_category.memory_of ~subst:ctx.subst ty in
        Env.{ ll_v; as_treat = Value_category.AsPtr mem }
      in
      blit_term_opt ~env ll_builder local t
  (* *)
  | Rir.Term.{ kind = Construct { struct_tag }; ty; _ } ->
      let Env.{ ll_ty } = Env.get_type env struct_tag in
      let value =
        let mem = Value_category.memory_of ~subst:ctx.subst ty in
        match ll_holder with
        | Some storage ->
            Env.{ ll_v = storage; as_treat = Value_category.AsPtr mem }
        | None -> failwith "[ICE] Cannot construct"
      in
      value
  (* *)
  | Rir.Term.{ kind = RVal v; ty; _ } ->
      construct_value ~ctx ~env ~ll_holder ~local ll_builder v ty
  (* *)
  | Rir.Term.{ kind = LVal id; ty; _ } ->
      [%loga.debug "LVal (%s)" id];
      let t = Env.get_local_var env id in
      blit_term_opt ~env ll_builder local t
  (* *)
  | Rir.Term.{ kind = LValParam index; ty; _ } ->
      (* TODO *)
      let ll_v = L.param ll_f index in
      Env.{ ll_v; as_treat = Value_category.AsVal }
  (* *)
  | Rir.Term.{ kind = Undef; ty; _ } ->
      let value =
        let mem = Value_category.memory_of ~subst:ctx.subst ty in
        match ll_holder with
        | Some storage ->
            Env.{ ll_v = storage; as_treat = Value_category.AsPtr mem }
        | None ->
            let ll_v = L.const_null (L.void_type ctx.ll_ctx) in
            Env.{ ll_v; as_treat = Value_category.AsVal }
      in
      value

let construct_inst ~ctx ~env ll_f ll_builder inst : Env.t =
  [%loga.debug "Inst -> %s" (Rir.Term.show_inst_t inst)];
  match inst with
  (* *)
  | Rir.Term.Let (placeholder, term, _) ->
      let env =
        let local = Env.get_local_var env placeholder in
        match local with
        (* mutable value or structs *)
        | Env.{ ll_v; as_treat = Value_category.AsPtr _ } ->
            let ll_holder = Some ll_v in
            let _ =
              construct_term ~ctx ~env ~ll_holder ~local:(Some local) ll_f
                ll_builder term
            in
            env
        (* immutable value(like int, address) *)
        | _ ->
            let ll_holder = None in
            let local =
              construct_term ~ctx ~env ~ll_holder ~local:(Some local) ll_f
                ll_builder term
            in
            Env.set_local_var env placeholder local
      in
      env
  (* *)
  | Rir.Term.Assign { lhs; rhs } ->
      let lhs =
        construct_term ~ctx ~env ~ll_holder:None ~local:None ll_f ll_builder lhs
      in
      let rhs =
        construct_term ~ctx ~env ~ll_holder:None ~local:(Some lhs) ll_f
          ll_builder rhs
      in
      (* let () =
           match (lhs, rhs) with
           | ( Env.{ ll_v = ll_lhs; as_treat = Value_category.AsPtr _ },
               Env.{ ll_v = ll_rhs; as_treat = Value_category.AsPtr _ } ) ->
               let ll_rhs_v = L.build_load ll_rhs "" ll_builder in
               let _ll_v : L.llvalue = L.build_store ll_rhs_v ll_lhs ll_builder in
               ()
           | ( Env.{ ll_v = ll_lhs; as_treat = Value_category.AsPtr _ },
               Env.{ ll_v = ll_rhs; as_treat = Value_category.AsVal } ) ->
               let _ll_v : L.llvalue = L.build_store ll_rhs ll_lhs ll_builder in
               ()
           | ( Env.{ ll_v = ll_lhs; as_treat = t_lhs },
               Env.{ ll_v = ll_rhs; as_treat = t_rhs } ) ->
               failwith
                 (Printf.sprintf "[ICE] cannot assign: %s <- %s"
                    (Value_category.show_as_treat_t t_lhs)
                    (Value_category.show_as_treat_t t_rhs))
         in*)
      env
  (* *)
  | Rir.Term.TerminatorPoint _ -> (* Ignore *) env

let construct_terminator ~ctx ~env ~ret_term ll_f ll_builder termi =
  [%loga.debug "Termi -> %s" (Rir.Term.show_terminator_t termi)];
  match termi with
  (* *)
  | Rir.Term.Cond (cond, t, e) ->
      let c = Env.get_local_var env cond in
      let ll_bb_t = Env.get_bb env t in
      let ll_bb_e = Env.get_bb env e in
      let ll_c =
        match c with
        | Env.{ ll_v; as_treat = Value_category.AsPtr _ } ->
            L.build_load ll_v "" ll_builder
        | Env.{ ll_v; as_treat = Value_category.AsVal } -> ll_v
      in
      L.build_cond_br ll_c ll_bb_t ll_bb_e ll_builder
  (* *)
  | Rir.Term.Jump label ->
      let ll_bb = Env.get_bb env label in
      L.build_br ll_bb ll_builder
  (* *)
  | Rir.Term.Ret ->
      let ll_v =
        match ret_term with
        | Some ret_term ->
            let ll_ret =
              match ret_term with
              | Env.{ ll_v; as_treat = Value_category.(AsPtr MemPrimitive) } ->
                  let v = L.build_load ll_v "" ll_builder in
                  L.build_ret v ll_builder
              | Env.{ ll_v; as_treat = Value_category.(AsPtr MemMemory) } ->
                  L.build_ret_void ll_builder
              | Env.{ ll_v; as_treat = Value_category.AsVal } ->
                  L.build_ret ll_v ll_builder
            in
            ll_ret
        | None -> L.build_ret_void ll_builder
      in
      ll_v

let construct_bb ~ctx ~env ll_f ll_builder func bb =
  let env =
    let insts = Rir.Term.BB.get_insts bb in
    List.fold_left insts ~init:env ~f:(fun env inst ->
        construct_inst ~ctx ~env ll_f ll_builder inst)
  in

  Rir.Term.BB.get_terminator_opt bb
  |> Option.iter ~f:(fun termi ->
         (* return value storage *)
         let ret_term =
           Rir.Func.get_ret_term func
           |> Option.map ~f:(fun ret_term ->
                  match ret_term with
                  | Rir.Term.{ kind = LVal name; _ } ->
                      Env.get_local_var env name
                  | _ -> failwith "[ICE]")
         in
         let _ =
           construct_terminator ~ctx ~env ~ret_term ll_f ll_builder termi
         in
         ());
  env

let construct_pre_alloc ~ctx ~env ll_f ll_builder pre_alloc =
  let Rir.Func.{ p_bb_name = bb_name; p_insts } = pre_alloc in
  let ll_undef = L.undef (L.void_type ctx.ll_ctx) in
  let f env alloc_inst =
    let (inst, addressable) = alloc_inst in
    [%loga.debug "Inst(pre-alloc) -> %s" (Rir.Term.show_inst_t inst)];
    let (placeholder, value) =
      match inst with
      (* *)
      | Rir.Term.Let (placeholder, (Rir.Term.{ ty; _ } as term), _) ->
          let v =
            match addressable with
            (* *)
            | Rir.Func.AddressableT
                Rir.Func.{ addressable_e_kind = Rir.Func.AddrKindStandard; _ }
              ->
                let storage = Value_category.memory_of ~subst:ctx.subst ty in
                let v =
                  let ll_ty = to_llty ~ctx ~env term.Rir.Term.ty in
                  let ll_v = L.build_alloca ll_ty "" ll_builder in
                  Env.{ ll_v; as_treat = Value_category.AsPtr storage }
                in
                v
            (* *)
            | Rir.Func.AddressableT
                Rir.Func.{ addressable_e_kind = Rir.Func.AddrKindRet; _ } ->
                let storage = Value_category.memory_of ~subst:ctx.subst ty in
                let v =
                  match storage with
                  | Value_category.MemPrimitive ->
                      let ll_ty = to_llty ~ctx ~env term.Rir.Term.ty in
                      let ll_v = L.build_alloca ll_ty "" ll_builder in
                      Env.{ ll_v; as_treat = Value_category.AsPtr storage }
                  | Value_category.MemMemory ->
                      let ll_v = L.param ll_f 0 in
                      Env.{ ll_v; as_treat = Value_category.AsPtr storage }
                in
                v
            (* *)
            | Rir.Func.AddressableF ->
                let ll_v = ll_undef in
                let v = Env.{ ll_v; as_treat = Value_category.AsVal } in
                v
          in
          (placeholder, v)
      (* *)
      | _ -> failwith "[ICE] not let"
    in
    Env.set_local_var env placeholder value
  in
  List.fold_left p_insts ~init:env ~f

let construct_func ~ctx ~env ll_mod ll_f (name, func) =
  (* *)
  let ll_entry_bb = L.entry_block ll_f in
  let ll_builder = L.builder_at_end ctx.ll_ctx ll_entry_bb in

  let pre_allocs = Rir.Func.get_pre_allocs func in
  let env =
    List.fold_left pre_allocs ~init:env ~f:(fun env pre_alloc ->
        construct_pre_alloc ~ctx ~env ll_f ll_builder pre_alloc)
  in

  (* entry -> program_entry *)
  let ll_program_bb = L.append_block ctx.ll_ctx "program_entry" ll_f in
  let _ = L.build_br ll_program_bb ll_builder in

  (* *)
  let bbs = Rir.Func.list_reached_bbs func in
  let (env, _) =
    List.fold_left bbs ~init:(env, ll_program_bb) ~f:(fun (env, ll_bb) bb ->
        let name = bb.Rir.Term.BB.name in
        let ll_bb =
          match name with
          | "entry" -> ll_bb
          | _ -> L.append_block ctx.ll_ctx name ll_f
        in
        let env = Env.set_bb env name ll_bb in
        (env, ll_bb))
  in
  List.fold_left bbs ~init:env ~f:(fun env bb ->
      let name = bb.Rir.Term.BB.name in

      [%loga.debug "BB -> %s" name];
      let ll_bb = Env.get_bb env name in
      let ll_builder = L.builder_at_end ctx.ll_ctx ll_bb in
      construct_bb ~ctx ~env ll_f ll_builder func bb)

(* declare types *)
let pre_construct_type ~ctx ~env ll_mod t : Env.type_t * Env.t =
  let Rir.Module.{ ty_name; ty_struct_tag; ty_rir } = t in

  let mangled_name = (*TODO*) ty_name in
  let ll_ty = L.named_struct_type ctx.ll_ctx mangled_name in

  let tt = Env.{ ll_ty } in
  let env = Env.set_type env ty_struct_tag tt in
  (tt, env)

(* define types *)
let construct_type ~ctx ~env ll_mod ll_ty t : Env.t =
  let Rir.Module.{ ty_rir; _ } = t in

  let packed = false in
  L.struct_set_body ll_ty [||] packed;

  env

(* declare functions *)
let pre_construct_func ~ctx ~env ll_mod f : Env.func_t * Env.t =
  let (name, func) = f in

  let ty = func.Rir.Func.ty in
  let ll_ty = to_llty ~ctx ~env ty in
  let kind =
    match func.Rir.Func.extern_name with
    | Some builtin_name when String.is_prefix builtin_name ~prefix:"%" ->
        Env.FuncBuiltin (find_builtin builtin_name)
    | Some extern_name ->
        Env.FuncLLVMDecl (L.declare_function extern_name ll_ty ll_mod)
    | None ->
        [%loga.debug "def func -> %s" (L.string_of_lltype ll_ty)];
        let mangled_name = Mangling.mangle name ty in
        Env.FuncLLVMDef (L.define_function mangled_name ll_ty ll_mod)
  in
  let ff = Env.{ kind; ty } in
  let env = Env.set_func env name ff in
  (ff, env)

let generate_module ~ctx rir_mod : Module.t =
  let module_name = rir_mod.Rir.Module.module_name in
  let ll_mod = L.create_module ctx.ll_ctx module_name in

  let intrinsics = Llvm_gen_intrinsics.load_intrinsics ctx.ll_ctx ll_mod in

  let env = Env.create ~intrinsics in

  (* types *)
  let (env, ts_rev) =
    let types = Rir.Module.types rir_mod in
    List.fold_left types ~init:(env, []) ~f:(fun (env, ts) t ->
        let (tt, env) = pre_construct_type ~ctx ~env ll_mod t in
        (env, (t, tt) :: ts))
  in
  let env =
    List.fold_left ts_rev ~init:env ~f:(fun env (t, tt) ->
        let Env.{ ll_ty; _ } = tt in
        let env = construct_type ~ctx ~env ll_mod ll_ty t in
        env)
  in

  (* functions *)
  let (env, fs_rev) =
    let funcs = Rir.Module.funcs rir_mod in
    List.fold_left funcs ~init:(env, []) ~f:(fun (env, fs) f ->
        let (ff, env) = pre_construct_func ~ctx ~env ll_mod f in
        (env, (f, ff) :: fs))
  in
  let () =
    fs_rev
    |> List.iter ~f:(fun (f, ff) ->
           match ff with
           | Env.{ kind = FuncLLVMDef ll_f; _ } ->
               let _env = construct_func ~ctx ~env ll_mod ll_f f in
               ()
           | _ -> ())
  in
  ll_mod

let write_to ~ch ~bitcode llvm =
  match bitcode with
  | true ->
      let successful = L_bitwriter.output_bitcode ~unbuffered:true ch llvm in
      if successful then Ok () else Error ""
  | false ->
      Stdio.Out_channel.fprintf ch "%s" (L.string_of_llmodule llvm);
      Ok ()
