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

let rec type_of ctx ty : L.lltype =
  L.void_type ctx

let rec construct_bb m (ir_fun : Rir_term.Func.t) lvenv ir_bb bb : (unit, Diagnostics.t) Result.t =
  let ctx = L.module_context m in
  let builder = L.builder_at_end ctx bb in

  let rec construct_insts ir_insts =
    match ir_insts with
    | ir_inst :: rest ->
       let open Result.Let_syntax in
       let%bind _ = construct_inst ir_inst in
       construct_insts rest
    | [] ->
       Ok ()

  and construct_inst ir_inst =
    match ir_inst with
    | Rir_term.Let (placeholder, v) ->
       let open Result.Let_syntax in
       let%bind body = construct_value ~name:(Some placeholder) v in
       let storage = Map.find_exn lvenv placeholder in
       let _ = L.build_store storage body builder in
       Ok ()

    | Rir_term.Nop ->
       Ok ()

  and construct_value ?name ir_value =
    match ir_value with
    | Rir_term.{kind = Call (callee, args); _} ->
       let callee_val = Map.find_exn lvenv callee in
       let args_vals = List.map args ~f:(Map.find_exn lvenv) |> Array.of_list in
       let llval = L.build_call callee_val args_vals "" builder in
       Ok llval

    | Rir_term.{kind = RVal ir_rvalue; ty; _} ->
       begin match ir_rvalue with
       | Rir_term.ValueInt v ->
          Ok (L.const_int (type_of ctx ty) v)

       | Rir_term.ValueString v ->
          Ok (L.const_stringz ctx v)

       | Rir_term.ValueUnit ->
          Ok (L.const_null (type_of ctx ty))
       end

    | Rir_term.{kind = LVal var_name; _} ->
       Ok (Map.find_exn lvenv var_name)

    | Rir_term.{kind = Unit; _} ->
       Ok (L.const_null (L.void_type ctx))

  and construct_terminator ir_terminator =
    Ok ()
  in
  let result =
    let open Result.Let_syntax in
    let%bind _body = construct_insts (Rir_term.BB.get_insts ir_bb) in
    let%bind _ret =
      Rir_term.BB.get_terminator_opt ir_bb
      |> (fun t -> Option.value_map t ~default:(Ok ()) ~f:construct_terminator)
    in
    Ok ()
  in
  result

let construct_func m ir_fun : (unit, Diagnostics.t) Result.t =
  let ctx = L.module_context m in

  let fty = L.function_type (L.void_type ctx) [||] in
  let f = L.define_function ir_fun.Rir_term.Func.name fty m in

  let ir_entry_bb = Rir_term.Func.get_entry_bb ir_fun in
  let entry_bb = L.entry_block f in

  let lvenv = Map.empty (module String) in
  let builder = L.builder_at_end ctx entry_bb in

  (* TODO: fix *)
  let v = L.build_alloca (L.void_type ctx) "0" builder in
  let lvenv = Map.add_exn lvenv ~key:"0" ~data:v in

  let v = L.build_alloca (L.void_type ctx) "1" builder in
  let lvenv = Map.add_exn lvenv ~key:"1" ~data:v in

  let println_ty = L.function_type (L.void_type ctx) [|L.pointer_type (L.i8_type ctx)|] in
  let v = L.declare_function "println" println_ty m in
  let lvenv = Map.add_exn lvenv ~key:"println" ~data:v in

  construct_bb m ir_fun lvenv ir_entry_bb entry_bb

let create_context () : context_t =
  let llctx = L.create_context () in
  llctx

let create_module ctx rir : (t, Diagnostics.t) Result.t =
  let module_name = "mod_name" in (* TODO: fix *)
  let llmod = L.create_module ctx module_name in
  let rec build ctor top_levels =
    match top_levels with
    | t :: ts ->
       begin match ctor t with
       | Ok _ -> build ctor ts
       | Error _ as e -> e
       end
    | [] ->
       Ok ()
  in
  let open Result.Let_syntax in
  let%bind _ = build (construct_func llmod) (List.rev rir.Rir_term.funcs) in
  Ok llmod

let debug_string_of m =
  L.string_of_llmodule m
