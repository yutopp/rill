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

type as_treat_t = AsVal | AsPtr [@@deriving show]

let rec to_llty ~ctx ty : L.lltype =
  let subst = ctx.subst in
  match Typing.Subst.subst_type subst ty with
  | Typing.Type.{ ty = Unit; _ } -> L.void_type ctx.ll_ctx
  | Typing.Type.{ ty = Bool; _ } -> L.integer_type ctx.ll_ctx 1
  | Typing.Type.{ ty = Int; _ } -> L.integer_type ctx.ll_ctx 32
  | Typing.Type.{ ty = String; _ } -> L.pointer_type (L.i8_type ctx.ll_ctx)
  | Typing.Type.{ ty = Func { params; ret; _ }; _ } ->
      let params_tys = List.map ~f:(to_llty ~ctx) params in
      let ret_ty = to_llty ~ctx ret in
      L.function_type ret_ty (Array.of_list params_tys)
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

module Env = struct
  type t = {
    local_vars : var_t Map.M(String).t;
    funcs : func_t Map.M(String).t;
    bb : L.llbasicblock Map.M(String).t;
  }

  and func_t = { ty : Typing.Type.t; kind : func_kind_t }

  and func_kind_t =
    | FuncLLVMDecl of L.llvalue
    | FuncLLVMDef of L.llvalue
    | FuncBuiltin of (L.llvalue array -> L.llbuilder -> L.llvalue)

  and var_t = { ll_v : L.llvalue; as_treat : as_treat_t }

  let create () =
    {
      local_vars = Map.empty (module String);
      funcs = Map.empty (module String);
      bb = Map.empty (module String);
    }

  let get_local_var env name = Map.find_exn env.local_vars name

  let set_local_var env name value =
    { env with local_vars = Map.add_exn ~key:name ~data:value env.local_vars }

  let get_func env name = Map.find_exn env.funcs name

  let set_func env name ll_f =
    { env with funcs = Map.add_exn ~key:name ~data:ll_f env.funcs }

  let get_bb env name = Map.find_exn env.bb name

  let set_bb env name ll_bb =
    { env with bb = Map.add_exn ~key:name ~data:ll_bb env.bb }
end

let should_treat ty =
  (* TODO: impl *)
  AsVal

let conv_val_ptr ll_builder ((param_ty, value) : Typing.Type.t * Env.var_t) :
    L.llvalue =
  let target_t = should_treat param_ty in
  match (target_t, value) with
  | (AsVal, Env.{ ll_v; as_treat = AsVal })
  | (AsPtr, Env.{ ll_v; as_treat = AsPtr }) ->
      ll_v
  | (AsVal, Env.{ ll_v; as_treat = AsPtr }) -> L.build_load ll_v "" ll_builder
  | (AsPtr, Env.{ ll_v; as_treat = AsVal }) -> failwith "[ICE] ?"

let construct_value ~ctx ll_builder v ty =
  match v with
  | Rir.Term.ValueBool v -> L.const_int (to_llty ~ctx ty) (if v then 1 else 0)
  | Rir.Term.ValueInt v -> L.const_int (to_llty ~ctx ty) v
  | Rir.Term.ValueString v ->
      (* L.const_stringz ctx.ll_ctx v *)
      L.build_global_stringptr v "" ll_builder
  | Rir.Term.ValueUnit -> L.const_null (to_llty ~ctx ty)

let construct_term ~ctx ~env ~ll_holder ll_f ll_builder term : Env.var_t =
  match term with
  (* *)
  | Rir.Term.{ kind = Call (callee, args); ty; _ } ->
      let Env.{ ty = f_ty; kind = f_kind } = Env.get_func env callee in
      let (f_params_tys, f_ret_ty) = Typing.Type.assume_func_ty f_ty in
      let arg_values = List.map args ~f:(Env.get_local_var env) in
      let ll_args =
        List.zip f_params_tys arg_values |> function
        | List.Or_unequal_lengths.Ok xs ->
            List.map xs ~f:(conv_val_ptr ll_builder) |> Array.of_list
        | List.Or_unequal_lengths.Unequal_lengths ->
            failwith
              (Printf.sprintf "[ICE] param length are different %d != %d"
                 (List.length f_params_tys) (List.length arg_values))
      in
      let ll_v =
        match f_kind with
        | Env.FuncLLVMDecl ll_callee | Env.FuncLLVMDef ll_callee ->
            L.build_call ll_callee ll_args "" ll_builder
        | Env.FuncBuiltin pass -> pass ll_args ll_builder
      in
      let value =
        match ll_holder with
        | Some mem ->
            let _ll_v : L.llvalue = L.build_store ll_v mem ll_builder in
            Env.{ ll_v = mem; as_treat = AsPtr }
        | None -> Env.{ ll_v; as_treat = AsVal }
      in
      value
  (* *)
  | Rir.Term.{ kind = RVal v; ty; _ } ->
      let ll_v = construct_value ~ctx ll_builder v ty in
      let value =
        match ll_holder with
        | Some mem ->
            let _ll_v : L.llvalue = L.build_store ll_v mem ll_builder in
            Env.{ ll_v = mem; as_treat = AsPtr }
        | None -> Env.{ ll_v; as_treat = AsVal }
      in
      value
  (* *)
  | Rir.Term.{ kind = LVal id; ty; _ } ->
      [%loga.debug "LVal (%s)" id];
      let value = Env.get_local_var env id in
      value
  (* *)
  | Rir.Term.{ kind = LValParam index; ty; _ } ->
      (* TODO *)
      let ll_v = L.param ll_f index in
      Env.{ ll_v; as_treat = AsVal }
  (* *)
  | Rir.Term.{ kind = Undef; _ } ->
      let value =
        match ll_holder with
        | Some mem -> Env.{ ll_v = mem; as_treat = AsPtr }
        | None ->
            let ll_v = L.const_null (L.void_type ctx.ll_ctx) in
            Env.{ ll_v; as_treat = AsVal }
      in
      value

let construct_inst ~ctx ~env ll_f ll_builder inst : Env.t =
  [%loga.debug "Inst -> %s" (Rir.Term.show_inst_t inst)];
  match inst with
  (* *)
  | Rir.Term.Let (placeholder, term, alloc) ->
      let env =
        match alloc with
        (* *)
        | Rir.Term.AllocLit ->
            let ll_holder = None in
            let local =
              construct_term ~ctx ~env ~ll_holder ll_f ll_builder term
            in
            Env.set_local_var env placeholder local
        (* *)
        | Rir.Term.AllocStack ->
            let ll_holder =
              let local = Env.get_local_var env placeholder in
              match local with
              | Env.{ ll_v; as_treat = AsPtr } -> Some ll_v
              | _ -> failwith "[ICE]"
            in
            let _ = construct_term ~ctx ~env ~ll_holder ll_f ll_builder term in
            env
      in
      env
  (* *)
  | Rir.Term.Assign { lhs; rhs } ->
      let lhs = construct_term ~ctx ~env ~ll_holder:None ll_f ll_builder lhs in
      let rhs = construct_term ~ctx ~env ~ll_holder:None ll_f ll_builder rhs in
      let () =
        match (lhs, rhs) with
        | ( Env.{ ll_v = ll_lhs; as_treat = AsPtr },
            Env.{ ll_v = ll_rhs; as_treat = AsPtr } ) ->
            let ll_rhs_v = L.build_load ll_rhs "" ll_builder in
            let _ll_v : L.llvalue = L.build_store ll_rhs_v ll_lhs ll_builder in
            ()
        | ( Env.{ ll_v = ll_lhs; as_treat = AsPtr },
            Env.{ ll_v = ll_rhs; as_treat = AsVal } ) ->
            let _ll_v : L.llvalue = L.build_store ll_rhs ll_lhs ll_builder in
            ()
        | ( Env.{ ll_v = ll_lhs; as_treat = t_lhs },
            Env.{ ll_v = ll_rhs; as_treat = t_rhs } ) ->
            failwith
              (Printf.sprintf "[ICE] cannot assign: %s <- %s"
                 (show_as_treat_t t_lhs) (show_as_treat_t t_rhs))
      in
      env
  (* *)
  | Rir.Term.TerminatorPoint _ -> (* Ignore *) env

let construct_terminator ~ctx ~env ll_f ll_builder termi =
  [%loga.debug "Termi -> %s" (Rir.Term.show_terminator_t termi)];
  match termi with
  (* *)
  | Rir.Term.Cond (cond, t, e) ->
      let c = Env.get_local_var env cond in
      let ll_bb_t = Env.get_bb env t in
      let ll_bb_e = Env.get_bb env e in
      let ll_c =
        match c with
        | Env.{ ll_v; as_treat = AsPtr } -> L.build_load ll_v "" ll_builder
        | Env.{ ll_v; as_treat = AsVal } -> ll_v
      in
      L.build_cond_br ll_c ll_bb_t ll_bb_e ll_builder
  (* *)
  | Rir.Term.Jump label ->
      let ll_bb = Env.get_bb env label in
      L.build_br ll_bb ll_builder
  (* *)
  | Rir.Term.Ret term ->
      let v = construct_term ~ctx ~env ~ll_holder:None ll_f ll_builder term in
      let ll_v =
        match v with
        | Env.{ ll_v; as_treat = AsPtr } -> L.build_load ll_v "" ll_builder
        | Env.{ ll_v; as_treat = AsVal } -> ll_v
      in
      L.build_ret ll_v ll_builder
  (* *)
  | Rir.Term.RetVoid -> L.build_ret_void ll_builder

let construct_bb ~ctx ~env ll_f ll_builder bb =
  let insts = Rir.Term.BB.get_insts bb in
  let env =
    List.fold_left insts ~init:env ~f:(fun env inst ->
        construct_inst ~ctx ~env ll_f ll_builder inst)
  in
  Rir.Term.BB.get_terminator_opt bb
  |> Option.iter ~f:(fun termi ->
         let _ = construct_terminator ~ctx ~env ll_f ll_builder termi in
         ());
  env

let construct_pre_alloc ~ctx ~env ll_f ll_builder (bb_name, alloc_insts) =
  let f env inst =
    [%loga.debug "Inst(pre-alloc) -> %s" (Rir.Term.show_inst_t inst)];
    match inst with
    (* *)
    | Rir.Term.Let (placeholder, term, Rir.Term.AllocStack) ->
        let value =
          let ll_ty = to_llty ~ctx term.Rir.Term.ty in
          let ll_v = L.build_alloca ll_ty "" ll_builder in
          Env.{ ll_v; as_treat = AsPtr }
        in
        Env.set_local_var env placeholder value
    (* Ignore *)
    | _ -> env
  in
  List.fold_left alloc_insts ~init:env ~f

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
  let bbs = Rir.Func.list_bbs func in
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
      construct_bb ~ctx ~env ll_f ll_builder bb)

(* declare functions *)
let pre_construct_func ~ctx ll_mod (name, func) : Env.func_t =
  let ty = func.Rir.Func.ty in
  let ll_ty = to_llty ~ctx ty in
  let kind =
    match func.Rir.Func.extern_name with
    | Some builtin_name when String.is_prefix builtin_name ~prefix:"%" ->
        Env.FuncBuiltin (find_builtin builtin_name)
    | Some extern_name ->
        Env.FuncLLVMDecl (L.declare_function extern_name ll_ty ll_mod)
    | None ->
        let mangled_name = Mangling.mangle name ty in
        Env.FuncLLVMDef (L.define_function mangled_name ll_ty ll_mod)
  in
  Env.{ kind; ty }

let generate_module ~ctx rir_mod : Module.t =
  let module_name = rir_mod.Rir.Module.module_name in
  let ll_mod = L.create_module ctx.ll_ctx module_name in

  let env = Env.create () in

  let (env, fs_rev) =
    let funcs = Rir.Module.funcs rir_mod in
    List.fold_left funcs ~init:(env, []) ~f:(fun (env, fs) f ->
        let (name, func) = f in
        let d_f = pre_construct_func ~ctx ll_mod (name, func) in
        let env = Env.set_func env name d_f in
        (env, (f, d_f) :: fs))
  in

  let () =
    fs_rev
    |> List.iter ~f:(fun (f, d_f) ->
           match d_f with
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
