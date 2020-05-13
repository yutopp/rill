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

let rec to_llty ~ctx ty : L.lltype =
  let subst = ctx.subst in
  match Typing.Subst.subst_type subst ty with
  | Typing.Type.{ ty = Unit; _ } -> L.void_type ctx.ll_ctx
  | Typing.Type.{ ty = Int; _ } -> L.integer_type ctx.ll_ctx 32
  | Typing.Type.{ ty = String; _ } -> L.pointer_type (L.i8_type ctx.ll_ctx)
  | Typing.Type.{ ty = Func (params, ret); _ } ->
      let params_tys = List.map ~f:(to_llty ~ctx) params in
      let ret_ty = to_llty ~ctx ret in
      L.function_type ret_ty (Array.of_list params_tys)
  | _ -> failwith "[ICE] not supported type"

module Env = struct
  type t = {
    local_vars : L.llvalue Map.M(String).t;
    funcs : L.llvalue Map.M(String).t;
    bb : L.llbasicblock Map.M(String).t;
  }

  let create () =
    {
      local_vars = Map.empty (module String);
      funcs = Map.empty (module String);
      bb = Map.empty (module String);
    }

  let get_local_var env name = Map.find_exn env.local_vars name

  let set_local_var env name ll_v =
    { env with local_vars = Map.add_exn ~key:name ~data:ll_v env.local_vars }

  let get_func env name = Map.find_exn env.funcs name

  let set_func env name ll_f =
    { env with funcs = Map.add_exn ~key:name ~data:ll_f env.funcs }

  let get_bb env name = Map.find_exn env.bb name

  let set_bb env name ll_bb =
    { env with bb = Map.add_exn ~key:name ~data:ll_bb env.bb }
end

let construct_value ~ctx ll_builder v ty =
  match v with
  | Rir.Term.ValueInt v -> L.const_int (to_llty ~ctx ty) v
  | Rir.Term.ValueString v ->
      (* L.const_stringz ctx.ll_ctx v *)
      L.build_global_stringptr v "" ll_builder
  | Rir.Term.ValueUnit -> L.const_null (to_llty ~ctx ty)

let construct_term ~ctx ~env ll_f ll_builder term =
  match term with
  | Rir.Term.{ kind = Call (callee, args); ty; _ } ->
      let callee_val = Env.get_func env callee in
      let args_vals =
        List.map args ~f:(Env.get_local_var env) |> Array.of_list
      in
      let llval = L.build_call callee_val args_vals "" ll_builder in
      llval
  | Rir.Term.{ kind = RVal v; ty; _ } ->
      let ll_v = construct_value ~ctx ll_builder v ty in
      ll_v
  | _ ->
      (* *)
      L.const_null (L.void_type ctx.ll_ctx)

let construct_inst ~ctx ~env ll_f ll_builder inst : Env.t =
  [%Loga.debug "Inst -> %s" (Rir.Term.show_inst_t inst)];
  match inst with
  | Rir.Term.Let (placeholder, term) ->
      let ll_v = construct_term ~ctx ~env ll_f ll_builder term in
      Env.set_local_var env placeholder ll_v
  | _ -> env

let construct_terminator ~ctx ~env ll_f ll_builder termi =
  let _ = L.build_ret_void ll_builder in
  ()

let construct_bb ~ctx ~env ll_f ll_builder bb =
  let insts = Rir.Term.BB.get_insts bb in
  let env =
    List.fold_left insts ~init:env ~f:(fun env inst ->
        construct_inst ~ctx ~env ll_f ll_builder inst)
  in
  Rir.Term.BB.get_terminator_opt bb
  |> Option.iter ~f:(fun termi ->
         construct_terminator ~ctx ~env ll_f ll_builder termi)

let construct_func ~ctx ~env ll_mod ll_f (name, func) =
  let (env, _) =
    let ll_entry_bb = L.entry_block ll_f in
    Hashtbl.fold func.Rir.Func.bbs ~init:(env, ll_entry_bb)
      ~f:(fun ~key ~data (env, ll_bb) ->
        let ll_bb =
          match key with
          | "entry" -> ll_bb
          | name -> L.insert_block ctx.ll_ctx name ll_bb
        in
        let env = Env.set_bb env key ll_bb in
        (env, ll_bb))
  in

  Hashtbl.iteri func.Rir.Func.bbs ~f:(fun ~key:name ~data:bb ->
      let ll_bb = Env.get_bb env name in
      let ll_builder = L.builder_at_end ctx.ll_ctx ll_bb in
      construct_bb ~ctx ~env ll_f ll_builder bb)

(* declare functions *)
let pre_construct_func ~ctx ll_mod (name, func) : L.llvalue * bool =
  let ty = func.Rir.Func.ty in
  let ll_ty = to_llty ~ctx ty in
  match func.Rir.Func.extern_name with
  | Some extern_name -> (L.declare_function extern_name ll_ty ll_mod, false)
  | None -> (L.define_function name ll_ty ll_mod, true)

let generate_module ~ctx rir_mod : Module.t =
  let module_name = rir_mod.Rir.Module.module_name in
  let ll_mod = L.create_module ctx.ll_ctx module_name in

  let env = Env.create () in
  let (env, fs_rev) =
    let funcs = Rir.Module.funcs rir_mod in
    List.fold_left funcs ~init:(env, []) ~f:(fun (env, fs) f ->
        let (name, func) = f in
        let (ll_f, def) = pre_construct_func ~ctx ll_mod (name, func) in
        let env = Env.set_func env name ll_f in
        (env, (f, ll_f, def) :: fs))
  in

  let () =
    fs_rev
    |> List.iter ~f:(fun (f, ll_f, def) ->
           if def then construct_func ~ctx ~env ll_mod ll_f f else ())
  in
  ll_mod
