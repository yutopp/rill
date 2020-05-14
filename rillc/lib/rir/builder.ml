(*
 * Copyright yutopp 2018 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

type t = {
  module_ : Module.t;
  current_func : Func.t option;
  current_bb : Term.BB.t option;
}

let create ~m = { module_ = m; current_func = None; current_bb = None }

let get_module ctx = ctx.module_

let with_current_func ctx f = { ctx with current_func = Some f }

let get_current_func ctx = Option.value_exn ctx.current_func

let with_current_bb ctx bb = { ctx with current_bb = Some bb }

let get_current_bb ctx = Option.value_exn ctx.current_bb

let get_current_state ctx = (ctx.current_func, ctx.current_bb)

let register_func_def ctx name f =
  (* TODO: support name *)
  Module.append_func ctx.module_ name f

let build_let ctx name v =
  let inst = Term.Let (name, v) in
  let bb = get_current_bb ctx in
  Term.BB.append_inst bb inst

let build_assign ctx lhs rhs =
  let inst = Term.Assign (lhs, rhs) in
  let bb = get_current_bb ctx in
  Term.BB.append_inst bb inst

let build_jump ctx bb =
  let term = Term.Jump bb.Term.BB.name in
  let bb = get_current_bb ctx in
  Term.BB.set_terminator bb term

let build_cond ctx cond t e =
  let term = Term.Cond (cond, t.Term.BB.name, e.Term.BB.name) in
  let bb = get_current_bb ctx in
  Term.BB.set_terminator bb term

let build_ret ctx name =
  let term = Term.Ret name in
  let bb = get_current_bb ctx in
  Term.BB.set_terminator bb term

let build_ret_void ctx =
  let term = Term.RetVoid in
  let bb = get_current_bb ctx in
  Term.BB.set_terminator bb term
