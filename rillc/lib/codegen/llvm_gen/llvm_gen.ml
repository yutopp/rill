(*
 * Copyright yutopp 2019 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module Span = Common.Span
module Builtin = Sema.Builtin
module Value_category = Codegen.Value_category
module Mangling = Codegen.Mangling
module L = Llvm
module L_bitwriter = Llvm_bitwriter
module L_linker = Llvm_linker

(* exports *)
module Backend = Llvm_gen_backend

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
  module Func = struct
    type t = { ll_f : L.llvalue; ty : Typing.Type.t; kind : kind_t }

    and kind_t = LLVMDecl | LLVMDef
  end

  module Var = struct
    type t = {
      ll_v : L.llvalue;
      ty : Typing.Type.t;
      as_treat : Value_category.as_treat_t;
    }
  end

  type t = {
    intrinsics : Llvm_gen_intrinsics.t;
    structs : type_t Map.M(String).t;
    global_vars : Var.t Map.M(String).t;
    local_args : Var.t Map.M(Int).t;
    local_vars : Var.t Map.M(String).t;
    bb : L.llbasicblock Map.M(String).t;
  }

  and type_t = { ll_ty : L.lltype }

  let create ~intrinsics =
    {
      intrinsics;
      structs = Map.empty (module String);
      global_vars = Map.empty (module String);
      local_args = Map.empty (module Int);
      local_vars = Map.empty (module String);
      bb = Map.empty (module String);
    }

  let get_struct env name =
    match Map.find env.structs name with
    | Some t -> t
    | None -> failwith (Printf.sprintf "[ICE] Type not found: name=%s" name)

  let set_struct env name ll_ty =
    { env with structs = Map.set ~key:name ~data:ll_ty env.structs }

  let set_local_arg env index value =
    { env with local_args = Map.set ~key:index ~data:value env.local_args }

  let set_local_var env name value =
    { env with local_vars = Map.set ~key:name ~data:value env.local_vars }

  let get_local_var env name = Map.find_exn env.local_vars name

  let set_global_var env name value =
    [%loga.debug "set_global_var = %s" name];
    { env with global_vars = Map.set ~key:name ~data:value env.global_vars }

  let get_var_place env placeholder =
    match placeholder with
    | Rir.Term.PlaceholderVar { name } -> get_local_var env name
    | Rir.Term.PlaceholderParam { index; _ } ->
        Map.find_exn env.local_args index
    | Rir.Term.PlaceholderGlobal { name } -> Map.find_exn env.global_vars name
    | Rir.Term.PlaceholderGlobal2 { name; _ } ->
        let name = Mangling.mangle2 name in
        Map.find_exn env.global_vars name

  let get_bb env name = Map.find_exn env.bb name

  let set_bb env name ll_bb =
    { env with bb = Map.set ~key:name ~data:ll_bb env.bb }
end

let rec to_llty ~ctx ~env ty : L.lltype =
  match ty with
  | Typing.Type.{ ty = Unit; _ } -> L.void_type ctx.ll_ctx
  | Typing.Type.{ ty = Num { bits; _ }; _ } -> L.integer_type ctx.ll_ctx bits
  | Typing.Type.{ ty = Size _; _ } ->
      let bytes = Typing.Mem.size_of ty in
      L.integer_type ctx.ll_ctx (bytes * 8)
  | Typing.Type.{ ty = String; _ } -> L.pointer_type (L.i8_type ctx.ll_ctx)
  | Typing.Type.{ ty = Array { elem; n }; _ } ->
      let elem_ll_ty = to_llty ~ctx ~env elem in
      L.array_type elem_ll_ty n
  | Typing.Type.{ ty = Func _; _ } as ty -> func_to_llty ~ctx ~env ty
  | Typing.Type.{ ty = Pointer { elem; _ }; _ } ->
      let elem_ll_ty = to_llty ~ctx ~env elem in
      L.pointer_type elem_ll_ty
  | Typing.Type.{ ty = Struct { name }; _ } ->
      let mangled_name = Mangling.mangle2 name in
      let Env.{ ll_ty } = Env.get_struct env mangled_name in
      ll_ty
  | _ ->
      failwith
        (Printf.sprintf "[ICE] not supported type: %s"
           (Typing.Type.to_string ty))

and func_to_llty ~ctx ~env ty : L.lltype =
  match Typing.Subst.subst_type ctx.subst ty with
  | Typing.Type.{ ty = Func { params; ret; _ }; _ } ->
      let params_tys = List.map ~f:(func_param_to_llty ~ctx ~env) params in
      let (params_tys, ret_ty) =
        let ret_ty = to_llty ~ctx ~env ret in
        match Value_category.memory_of ret with
        | Value_category.MemPrimitive -> (params_tys, ret_ty)
        | Value_category.MemMemory _ ->
            (* 1st args will be return type storage *)
            let params_tys =
              let ll_ty = L.pointer_type ret_ty in
              ll_ty :: params_tys
            in
            let ret_ty = L.void_type ctx.ll_ctx in
            (params_tys, ret_ty)
      in
      L.function_type ret_ty (Array.of_list params_tys)
  | _ ->
      failwith
        (Printf.sprintf "[ICE] not supported type (func): %s"
           (Typing.Type.to_string ty))

and func_param_to_llty ~ctx ~env ty : L.lltype =
  let ll_ty = to_llty ~ctx ~env ty in
  match Value_category.memory_of ty with
  | Value_category.MemPrimitive -> ll_ty
  | Value_category.MemMemory _ -> L.pointer_type ll_ty

let func_param_to_term ~ctx param_ty ll_param =
  match Value_category.memory_of param_ty with
  | Value_category.MemPrimitive ->
      Env.Var.
        { ll_v = ll_param; ty = param_ty; as_treat = Value_category.AsVal }
  | Value_category.MemMemory _ as mem ->
      Env.Var.
        { ll_v = ll_param; ty = param_ty; as_treat = Value_category.AsPtr mem }

let load_func_params_into_env ~ctx ~env ll_f ty =
  let (params_tys, ret_ty) = Typing.Type.assume_func_ty ty in
  let ll_params = L.params ll_f |> Array.to_list in
  match Value_category.memory_of ret_ty with
  (* *)
  | Value_category.MemPrimitive ->
      List.zip_exn params_tys ll_params
      |> List.foldi ~init:env ~f:(fun index env (param_ty, ll_param) ->
             let var = func_param_to_term ~ctx param_ty ll_param in
             Env.set_local_arg env index var)
  (* *)
  | Value_category.MemMemory _ ->
      let ll_params =
        (* Drop the 1st param which is used as a return value *)
        List.tl_exn ll_params
      in
      List.zip_exn params_tys ll_params
      |> List.foldi ~init:env ~f:(fun index env (param_ty, ll_param) ->
             let var = func_param_to_term ~ctx param_ty ll_param in
             Env.set_local_arg env index var)

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

let conv_val_ptr ctx ll_builder ((param_ty, value) : Typing.Type.t * Env.Var.t)
    : L.llvalue =
  let target_t = Value_category.should_treat param_ty in
  match (target_t, value) with
  | Value_category.(AsVal, Env.Var.{ ll_v; as_treat = AsVal; _ })
  | Value_category.(AsPtr _, Env.Var.{ ll_v; as_treat = AsPtr _; _ }) ->
      ll_v
  | Value_category.(AsVal, Env.Var.{ ll_v; as_treat = AsPtr _; _ }) ->
      L.build_load ll_v "" ll_builder
  | Value_category.(AsPtr _, Env.Var.{ ll_v; as_treat = AsVal; _ }) ->
      failwith "[ICE] ?"

let load_if_ref ll_builder v =
  match v with
  | Env.Var.{ ll_v; as_treat = Value_category.AsPtr _; _ } ->
      L.build_load ll_v "" ll_builder
  | Env.Var.{ ll_v; as_treat = Value_category.AsVal; _ } -> ll_v

let assume_ref v =
  match v with
  | Env.Var.{ ll_v; as_treat = Value_category.AsPtr _; _ } -> ll_v
  | Env.Var.{ ll_v; as_treat = Value_category.AsVal; _ } ->
      failwith (Printf.sprintf "[ICE] not ref: %s" (L.string_of_llvalue ll_v))

let assume_val v =
  match v with
  | Env.Var.{ ll_v; as_treat = Value_category.AsVal; _ } -> ll_v
  | Env.Var.{ ll_v; as_treat = Value_category.AsPtr _; _ } ->
      failwith (Printf.sprintf "[ICE] not val: %s" (L.string_of_llvalue ll_v))

let blit_term_opt ~env ll_builder (dst_term : Env.Var.t option)
    (src_term : Env.Var.t) : Env.Var.t =
  let module Vc = Value_category in
  match (dst_term, src_term) with
  (* primitive mem <- primitive mem: load/store *)
  | ( Some Env.Var.{ ll_v = dst; as_treat = Vc.(AsPtr MemPrimitive); ty = ty_d },
      Env.Var.{ ll_v = src; as_treat = Vc.(AsPtr MemPrimitive); ty = ty_s } ) ->
      let loaded = L.build_load src "" ll_builder in
      let _ll_v : L.llvalue = L.build_store loaded dst ll_builder in
      let ty = (* assume ty_d = ty_s *) ty_d in
      Env.Var.{ ll_v = dst; ty; as_treat = Vc.(AsPtr MemPrimitive) }
  (* primitive mem <- primitive val: store *)
  | ( Some Env.Var.{ ll_v = dst; as_treat = Vc.(AsPtr MemPrimitive); ty = ty_d },
      Env.Var.{ ll_v = src; as_treat = Vc.AsVal; ty = ty_s } ) ->
      let _ll_v : L.llvalue = L.build_store src dst ll_builder in
      let ty = (* assume ty_d = ty_s *) ty_d in
      Env.Var.{ ll_v = dst; ty; as_treat = Vc.(AsPtr MemPrimitive) }
  (* memory mem <- memory mem: memcpy *)
  | ( Some
        Env.Var.
          {
            ll_v = dst;
            as_treat = Vc.(AsPtr (MemMemory { size = s_dst }));
            ty = ty_d;
          },
      Env.Var.
        {
          ll_v = src;
          as_treat = Vc.(AsPtr (MemMemory { size = s_src }));
          ty = ty_s;
        } )
    when s_dst = s_src ->
      let inst = env.Env.intrinsics.Llvm_gen_intrinsics.memcpy_i32 in
      (* TODO: fix size and align *)
      let size = s_src in
      let align = 4 in
      let is_volatile = false in
      let ll_v = inst ll_builder dst src size align is_volatile in
      let ty = (* assume ty_d = ty_s *) ty_d in
      Env.Var.{ ll_v = dst; ty; as_treat = Vc.(AsPtr MemPrimitive) }
  (* val <- *: replace *)
  | (Some Env.Var.{ as_treat = Vc.AsVal; _ }, _) ->
      (* re-use generate term *)
      src_term
  | (None, _) ->
      (* passthrough *)
      src_term
  | _ ->
      failwith
        (Printf.sprintf "[ICE] cannot blit: %s <- %s"
           (Option.value_map dst_term ~default:"NONE" ~f:(fun t ->
                Vc.show_as_treat_t t.Env.Var.as_treat))
           (Vc.show_as_treat_t src_term.Env.Var.as_treat))

let construct_value ~ctx ~env ~ll_holder ~local ll_builder v ty : Env.Var.t =
  let into_ref ll_v =
    match ll_holder with
    | Some storage ->
        let _ll_v : L.llvalue = L.build_store ll_v storage ll_builder in
        let mem = Value_category.memory_of ty in
        Env.Var.{ ll_v = storage; ty; as_treat = Value_category.AsPtr mem }
    | None -> Env.Var.{ ll_v; ty; as_treat = Value_category.AsVal }
  in
  match v with
  | Rir.Term.ValueBool v ->
      let t =
        let ll_v = L.const_int (to_llty ~ctx ~env ty) (if v then 1 else 0) in
        Env.Var.{ ll_v; ty; as_treat = Value_category.AsVal }
      in
      blit_term_opt ~env ll_builder local t
  | Rir.Term.ValueInt v ->
      let t =
        let ll_v = L.const_int (to_llty ~ctx ~env ty) v in
        Env.Var.{ ll_v; ty; as_treat = Value_category.AsVal }
      in
      blit_term_opt ~env ll_builder local t
  | Rir.Term.ValueString v ->
      (* L.const_stringz ctx.ll_ctx v *)
      L.build_global_stringptr v "" ll_builder |> into_ref
  | Rir.Term.ValueUnit -> L.undef (to_llty ~ctx ~env ty) |> into_ref
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
          let ll_elem = Env.get_var_place env elem in
          match ll_elem with
          | Env.Var.{ ll_v = ll_rhs; as_treat = Value_category.AsPtr _; _ } ->
              let ll_rhs_v = L.build_load ll_rhs "" ll_builder in
              let _ll_v : L.llvalue =
                L.build_store ll_rhs_v ll_sto ll_builder
              in
              ()
          | Env.Var.{ ll_v = ll_rhs; as_treat = Value_category.AsVal; _ } ->
              let _ll_v : L.llvalue = L.build_store ll_rhs ll_sto ll_builder in
              ());
      let mem = Value_category.memory_of ty in
      Env.Var.{ ll_v = storage; ty; as_treat = Value_category.AsPtr mem }

let construct_term_assign_args ~ctx ll_builder params args =
  List.zip params args |> function
  | List.Or_unequal_lengths.Ok xs ->
      List.map xs ~f:(conv_val_ptr ctx ll_builder)
  | List.Or_unequal_lengths.Unequal_lengths ->
      failwith
        (Printf.sprintf "[ICE] param length are different %d != %d"
           (List.length params) (List.length args))

let construct_term ~ctx ~env ~ll_holder ~local ll_f ll_builder term : Env.Var.t
    =
  let module Vc = Value_category in
  match term with
  (* *)
  | Rir.Term.{ kind = Call (callee, args); ty; _ } ->
      let Env.Var.{ ll_v = ll_callee; ty = f_ty; _ } =
        Env.get_var_place env callee
      in
      let (f_params_tys, f_ret_ty) = Typing.Type.assume_func_ty f_ty in

      let arg_values = List.map args ~f:(Env.get_var_place env) in
      let ll_args =
        construct_term_assign_args ~ctx ll_builder f_params_tys arg_values
      in

      let ret_val_memory = Vc.memory_of f_ret_ty in
      let ll_args =
        match ret_val_memory with
        | Value_category.MemPrimitive -> ll_args
        | Value_category.MemMemory _ ->
            let ll_v =
              match local with
              | Some Env.Var.{ ll_v; as_treat = Vc.(AsPtr (MemMemory _)); _ } ->
                  ll_v
              | _ -> failwith "[ICE]"
            in
            ll_v :: ll_args
      in

      let ll_args = List.to_array ll_args in
      let ll_v = L.build_call ll_callee ll_args "" ll_builder in
      let value =
        match ret_val_memory with
        | Vc.MemPrimitive ->
            let t = Env.Var.{ ll_v; ty; as_treat = Vc.(AsVal) } in
            blit_term_opt ~env ll_builder local t
        | Vc.MemMemory _ ->
            (* A return value is already stored to local *)
            Option.value_exn local
      in
      value
  (* *)
  | Rir.Term.{ kind = Cast elem; ty; _ } ->
      let ll_elem = Env.get_var_place env elem |> load_if_ref ll_builder in
      let ll_ty = to_llty ~ctx ~env ty in
      let t =
        (* Treat values as Value *)
        let ll_v = L.build_inttoptr ll_elem ll_ty "" ll_builder in
        Env.Var.{ ll_v; ty; as_treat = Value_category.AsVal }
      in
      blit_term_opt ~env ll_builder local t
  (* *)
  | Rir.Term.{ kind = Index (elems, index); ty; _ } ->
      let ll_elems = Env.get_var_place env elems |> assume_ref in
      let ll_index = Env.get_var_place env index |> load_if_ref ll_builder in

      let zero = L.const_int (L.i32_type ctx.ll_ctx) 0 in
      let ll_v =
        L.build_in_bounds_gep ll_elems [| zero; ll_index |] "" ll_builder
      in
      let value =
        let mem = Value_category.memory_of ty in
        match ll_holder with
        | Some storage ->
            let _ll_v : L.llvalue = L.build_store ll_v storage ll_builder in
            Env.Var.{ ll_v = storage; ty; as_treat = Value_category.AsPtr mem }
        | None -> Env.Var.{ ll_v; ty; as_treat = Value_category.AsPtr mem }
      in
      value
  (* *)
  | Rir.Term.{ kind = Ref name; ty; _ } ->
      let ll_v = Env.get_var_place env name |> assume_ref in
      let t =
        (* Treat values as VALUES of Pointer *)
        Env.Var.{ ll_v; ty; as_treat = Value_category.AsVal }
      in
      blit_term_opt ~env ll_builder local t
  (* *)
  | Rir.Term.{ kind = Deref name; ty; _ } ->
      let ll_v = Env.get_var_place env name |> assume_val in
      let t =
        (* Treat values as Ptr *)
        let mem = Value_category.memory_of ty in
        Env.Var.{ ll_v; ty; as_treat = Value_category.AsPtr mem }
      in
      blit_term_opt ~env ll_builder local t
  (* *)
  | Rir.Term.{ kind = Construct; ty; _ } ->
      let ll_ty = to_llty ~ctx ~env ty in
      let value =
        let mem = Value_category.memory_of ty in
        match ll_holder with
        | Some storage ->
            Env.Var.{ ll_v = storage; ty; as_treat = Value_category.AsPtr mem }
        | None ->
            (* TODO: encode a struct into another primitive type *)
            let ll_v = L.undef ll_ty in
            Env.Var.{ ll_v; ty; as_treat = Value_category.AsVal }
      in
      value
  (* *)
  | Rir.Term.{ kind = RVal v; ty; _ } ->
      construct_value ~ctx ~env ~ll_holder ~local ll_builder v ty
  (* *)
  | Rir.Term.{ kind = LVal placeholder; ty; _ } ->
      [%loga.debug "LVal -> %s" (Rir.Term.show_placeholder_t placeholder)];
      let t = Env.get_var_place env placeholder in
      blit_term_opt ~env ll_builder local t
  (* *)
  | Rir.Term.{ kind = Undef; ty; _ } ->
      let value =
        let mem = Value_category.memory_of ty in
        match ll_holder with
        | Some storage ->
            Env.Var.{ ll_v = storage; ty; as_treat = Value_category.AsPtr mem }
        | None ->
            let ll_v = L.undef (L.void_type ctx.ll_ctx) in
            Env.Var.{ ll_v; ty; as_treat = Value_category.AsVal }
      in
      value

let construct_inst ~ctx ~env ll_f ll_builder inst : Env.t =
  [%loga.debug "Inst -> %s" (Rir.Term.show_inst_t inst)];
  match inst with
  (* *)
  | Rir.Term.Let (name, term, _) ->
      let env =
        let local = Env.get_local_var env name in
        match local with
        (* mutable value or structs *)
        | Env.Var.{ ll_v; as_treat = Value_category.AsPtr _; _ } ->
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
            Env.set_local_var env name local
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
      env
  (* *)
  | Rir.Term.TerminatorPoint _ -> (* Ignore *) env

let construct_terminator ~ctx ~env ~ret_term ll_f ll_builder termi =
  [%loga.debug "Termi -> %s" (Rir.Term.show_terminator_t termi)];
  match termi with
  (* *)
  | Rir.Term.Cond (cond, t, e) ->
      let c = Env.get_var_place env cond in
      let ll_bb_t = Env.get_bb env t in
      let ll_bb_e = Env.get_bb env e in
      let ll_c =
        match c with
        | Env.Var.{ ll_v; as_treat = Value_category.AsPtr _; _ } ->
            L.build_load ll_v "" ll_builder
        | Env.Var.{ ll_v; as_treat = Value_category.AsVal; _ } -> ll_v
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
              | Env.Var.
                  { ll_v; as_treat = Value_category.(AsPtr MemPrimitive); _ } ->
                  let v = L.build_load ll_v "" ll_builder in
                  L.build_ret v ll_builder
              | Env.Var.
                  { ll_v; as_treat = Value_category.(AsPtr (MemMemory _)); _ }
                ->
                  L.build_ret_void ll_builder
              | Env.Var.{ ll_v; as_treat = Value_category.AsVal; _ } ->
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
                      Env.get_var_place env name
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
                let storage = Value_category.memory_of ty in
                let v =
                  let ll_ty = to_llty ~ctx ~env term.Rir.Term.ty in
                  let ll_v = L.build_alloca ll_ty "" ll_builder in
                  Env.Var.{ ll_v; ty; as_treat = Value_category.AsPtr storage }
                in
                v
            (* *)
            | Rir.Func.AddressableT
                Rir.Func.{ addressable_e_kind = Rir.Func.AddrKindRet; _ } ->
                let storage = Value_category.memory_of ty in
                let v =
                  match storage with
                  | Value_category.MemPrimitive ->
                      let ll_ty = to_llty ~ctx ~env term.Rir.Term.ty in
                      let ll_v = L.build_alloca ll_ty "" ll_builder in
                      Env.Var.
                        { ll_v; ty; as_treat = Value_category.AsPtr storage }
                  | Value_category.MemMemory _ ->
                      let ll_v = L.param ll_f 0 in
                      Env.Var.
                        { ll_v; ty; as_treat = Value_category.AsPtr storage }
                in
                v
            (* *)
            | Rir.Func.AddressableF ->
                let ll_v = ll_undef in
                let v = Env.Var.{ ll_v; ty; as_treat = Value_category.AsVal } in
                v
          in
          (placeholder, v)
      (* *)
      | _ -> failwith "[ICE] not let"
    in
    Env.set_local_var env placeholder value
  in
  List.fold_left p_insts ~init:env ~f

let construct_func ~ctx ~env ll_mod ll_f func =
  (* *)
  let ll_entry_bb = L.entry_block ll_f in
  let ll_builder = L.builder_at_end ctx.ll_ctx ll_entry_bb in

  (* stack allocations *)
  let pre_allocs = Rir.Func.get_pre_allocs func in
  let env =
    List.fold_left pre_allocs ~init:env ~f:(fun env pre_alloc ->
        construct_pre_alloc ~ctx ~env ll_f ll_builder pre_alloc)
  in

  (* entry -> program_entry *)
  let ll_program_bb = L.append_block ctx.ll_ctx "program_entry" ll_f in
  let _ = L.build_br ll_program_bb ll_builder in

  (* body: make layouts of basic blokcs -> body insts *)
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

let declare_type ~ctx ~env ll_mod type_ : Env.type_t * Env.t =
  let Rir.Type.{ name; ty_sc } = type_ in

  let mangled_name = Mangling.mangle2 name in

  (* Currently, generics is not supported *)
  let ty_pred = Typing.Scheme.assume_has_no_generics ty_sc in
  let (Typing.Pred.Pred { ty; _ }) = ty_pred in

  let ll_ty =
    match ty with
    | Typing.Type.{ ty = Struct _; _ } ->
        let ll_ty = L.named_struct_type ctx.ll_ctx mangled_name in
        ll_ty
    | _ -> failwith (Printf.sprintf "[ICE] %s" (Typing.Type.to_string ty))
  in
  let tt = Env.{ ll_ty } in
  let env = Env.set_struct env mangled_name tt in
  (tt, env)

(* define types *)
let define_type ~ctx ~env ll_mod ll_ty type_ : Env.t =
  (* TODO: implement *)
  let packed = false in
  L.struct_set_body ll_ty [||] packed;

  env

(* define global variable *)
let define_global ~ctx ~env ll_mod g : Env.t =
  let Rir.Global.{ name; ty_sc; _ } = g in

  (* Currently, generics is not supported *)
  let ty = Typing.Scheme.assume_has_no_generics ty_sc in
  let (Typing.Pred.Pred { ty; _ }) = ty in

  let ll_ty = to_llty ~ctx ~env ty in
  let ll_v =
    match g.Rir.Global.body with
    | Some (Rir.Global.BodyExtern extern_name) ->
        L.declare_global ll_ty extern_name ll_mod
    | _ -> failwith "[ICE]"
  in
  let mem = Value_category.memory_of ty in
  let var = Env.Var.{ ll_v; ty; as_treat = Value_category.(AsPtr mem) } in

  let mangled_name = Mangling.mangle2 name in
  let env = Env.set_global_var env mangled_name var in
  env

(* declare functions *)
let pre_construct_func ~ctx ~env ll_mod func : Env.Func.t * Env.t =
  let Rir.Func.{ name; ty_sc; _ } = func in

  let mangled_name = Mangling.mangle2 name in

  (* Currently, generics is not supported *)
  let ty = Typing.Scheme.assume_has_no_generics ty_sc in
  let (Typing.Pred.Pred { ty; _ }) = ty in

  let ll_ty = to_llty ~ctx ~env ty in
  let f =
    match func.Rir.Func.body with
    (* *)
    | Some (Rir.Func.BodyExtern builtin_name)
      when String.is_prefix builtin_name ~prefix:"%" ->
        let ll_f = L.define_function builtin_name ll_ty ll_mod in
        let () =
          let ll_bb = L.entry_block ll_f in
          let ll_builder = L.builder_at_end ctx.ll_ctx ll_bb in
          let generator = find_builtin builtin_name in
          let ll_ret = generator (L.params ll_f) ll_builder in
          let (_ : L.llvalue) = L.build_ret ll_ret ll_builder in
          ()
        in
        L.set_visibility L.Visibility.Hidden ll_f;
        L.set_linkage L.Linkage.Private ll_f;
        let attr = L.create_enum_attr ctx.ll_ctx "alwaysinline" 0L in
        L.add_function_attr ll_f attr L.AttrIndex.Function;
        Env.Func.{ ll_f; ty; kind = LLVMDecl }
    (* *)
    | Some (Rir.Func.BodyExtern extern_name) ->
        let ll_f = L.declare_function extern_name ll_ty ll_mod in
        Env.Func.{ ll_f; ty; kind = LLVMDecl }
    (* *)
    | Some (Rir.Func.BodyFunc _) ->
        [%loga.debug "def func -> %s" (L.string_of_lltype ll_ty)];
        let ll_f = L.define_function mangled_name ll_ty ll_mod in
        Env.Func.{ ll_f; ty; kind = LLVMDef }
    (* *)
    | None ->
        let ll_f = L.declare_function mangled_name ll_ty ll_mod in
        Env.Func.{ ll_f; ty; kind = LLVMDecl }
  in
  let var =
    let Env.Func.{ ll_f; _ } = f in
    let as_treat = Value_category.(AsVal) in
    Env.Var.{ ll_v = ll_f; ty; as_treat }
  in
  let env = Env.set_global_var env mangled_name var in
  (f, env)

(* module *)
let generate_module ~ctx rir_mod : Module.t =
  let module_name = rir_mod.Rir.Module.module_name in
  let ll_mod = L.create_module ctx.ll_ctx module_name in

  let intrinsics = Llvm_gen_intrinsics.load_intrinsics ctx.ll_ctx ll_mod in

  let env = Env.create ~intrinsics in

  (* types *)
  let (env, ts_rev) =
    let types = Rir.Module.all_types rir_mod in
    List.fold_left types ~init:(env, []) ~f:(fun (env, ts) type_ ->
        let (tt, env) = declare_type ~ctx ~env ll_mod type_ in
        (env, (type_, tt) :: ts))
  in
  let env =
    List.fold_left ts_rev ~init:env ~f:(fun env (t, tt) ->
        let Env.{ ll_ty; _ } = tt in
        let env = define_type ~ctx ~env ll_mod ll_ty t in
        env)
  in

  (* globals *)
  let env =
    let vars = Rir.Module.all_global_vars rir_mod in
    List.fold_left vars ~init:env ~f:(fun env g ->
        let env = define_global ~ctx ~env ll_mod g in
        env)
  in

  (* functions *)
  let (env, fs_rev) =
    let funcs = Rir.Module.defined_funcs rir_mod in
    List.fold_left funcs ~init:(env, []) ~f:(fun (env, fs) func ->
        let (ff, env) = pre_construct_func ~ctx ~env ll_mod func in
        (env, (func, ff) :: fs))
  in
  let () =
    fs_rev
    |> List.iter ~f:(fun (func, ff) ->
           match ff with
           | Env.Func.{ kind = LLVMDef; ll_f; ty } ->
               let env = load_func_params_into_env ~ctx ~env ll_f ty in
               let _env : Env.t = construct_func ~ctx ~env ll_mod ll_f func in
               ()
           | _ -> ())
  in
  ll_mod

let merge_modules mods =
  let ll_ctx = L.create_context () in
  let ll_mod = L.create_module ll_ctx "" in
  List.iter mods ~f:(L_linker.link_modules' ll_mod);
  ll_mod

let write_to ~ch ~bitcode llvm =
  match bitcode with
  | true ->
      let successful = L_bitwriter.output_bitcode ~unbuffered:true ch llvm in
      if successful then Ok () else Error ""
  | false ->
      Stdio.Out_channel.fprintf ch "%s" (L.string_of_llmodule llvm);
      Ok ()
