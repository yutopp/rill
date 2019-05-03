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
module L = Llvm

type t = L.llmodule

type context_t = L.llcontext

let rec to_llty ctx ty : L.lltype =
  match ty with
  | Type.Unit ->
     L.void_type ctx

  | Type.Int ->
     L.integer_type ctx 32

  | Type.String ->
     L.pointer_type (L.i8_type ctx)

  | Type.Func (params, ret) ->
     let params_tys = List.map ~f:(to_llty ctx) params in
     let ret_ty = to_llty ctx ret in
     L.function_type ret_ty (Array.of_list params_tys)

  | _ ->
     failwith "[ICE] not supported type"

let rec construct_bb m (ir_fun : Rir.Func.t) lvenv ir_bb bb : (unit, Diagnostics.t) Result.t =
  let ctx = L.module_context m in
  let builder = L.builder_at_end ctx bb in

  let rec construct_insts ir_insts lvenv =
    match ir_insts with
    | ir_inst :: rest ->
       let open Result.Let_syntax in
       let%bind lvenv = construct_inst ir_inst lvenv in
       construct_insts rest lvenv
    | [] ->
       Ok ()

  and construct_inst ir_inst lvenv =
    Stdio.printf "Inst -> %s\n"
                 (Rir.Term.sexp_of_inst_t ir_inst |> Sexp.to_string_hum);

    match ir_inst with
    | Rir.Term.Let (placeholder, v) ->
       let open Result.Let_syntax in
       let%bind body = construct_value ~name:(Some placeholder) v lvenv in
       let lvenv = Map.add_exn lvenv ~key:placeholder ~data:body in
       Ok lvenv

    | Rir.Term.Nop ->
       Ok lvenv

  and construct_value ?name ir_value lvenv =
    match ir_value with
    | Rir.Term.{kind = Call (callee, args); _} ->
       let callee_val = Option.value_exn ~message:callee (Map.find lvenv callee) in
       let args_vals = List.map args ~f:(Map.find_exn lvenv) |> Array.of_list in
       let llval = L.build_call callee_val args_vals "" builder in
       Ok llval

    | Rir.Term.{kind = RVal ir_rvalue; ty; _} ->
       begin match ir_rvalue with
       | Rir.Term.ValueInt v ->
          Ok (L.const_int (to_llty ctx ty) v)

       | Rir.Term.ValueString v ->
          Ok (L.const_stringz ctx v)

       | Rir.Term.ValueUnit ->
          Ok (L.const_null (to_llty ctx ty))
       end

    | Rir.Term.{kind = LVal var_name; _} ->
       Ok (Map.find_exn lvenv var_name)

    | Rir.Term.{kind = Undef; _} ->
       Ok (L.const_null (L.void_type ctx))

  and construct_terminator ir_terminator =
    Ok ()
  in
  let result =
    let open Result.Let_syntax in
    let%bind _body = construct_insts (Rir.Term.BB.get_insts ir_bb) lvenv in
    let%bind _ret =
      Rir.Term.BB.get_terminator_opt ir_bb
      |> (fun t -> Option.value_map t ~default:(Ok ()) ~f:construct_terminator)
    in
    let _ = L.build_ret_void builder in
    Ok ()
  in
  result

let pre_construct_func m lvenv (name, ir_fun) =
  let ctx = L.module_context m in

  let fty = match ir_fun.Rir.Func.tysc with
    | Type.Scheme.Scheme ([], ty) -> ty
    | _ -> failwith "(TODO) generics is not supported"
  in
  let ll_fty = to_llty ctx fty in
  let f =
    match ir_fun.Rir.Func.extern_name with
    | Some extern_name ->
       L.declare_function extern_name ll_fty m
    | None ->
       L.define_function name ll_fty m
  in

  Map.add_exn lvenv ~key:name ~data:f |> Result.return

let construct_func m lvenv _ (name, ir_fun) : (unit, Diagnostics.t) Result.t =
  let ctx = L.module_context m in

  match ir_fun.Rir.Func.extern_name with
  | Some _ ->
     Ok ()
  | None ->
     let f = Map.find_exn lvenv name in

     let ir_entry_bb = Rir.Func.get_entry_bb ir_fun in
     let entry_bb = L.entry_block f in

     let builder = L.builder_at_end ctx entry_bb in

     construct_bb m ir_fun lvenv ir_entry_bb entry_bb

let create_context () : context_t =
  let llctx = L.create_context () in
  llctx

let create_module ctx rir : (t, Diagnostics.t) Result.t =
  let module_name = "mod_name" in (* TODO: fix *)
  let llmod = L.create_module ctx module_name in
  let rec build ctor top_levels v =
    List.fold_result ~f:ctor
                     ~init:v
                     top_levels
  in
  let open Result.Let_syntax in
  let lvenv = Map.empty (module String) in
  let%bind lvenv = build (pre_construct_func llmod) (Rir.Module.funcs rir) lvenv in
  let%bind _ = build (construct_func llmod lvenv) (Rir.Module.funcs rir) () in
  Ok llmod

let debug_string_of m =
  L.string_of_llmodule m
