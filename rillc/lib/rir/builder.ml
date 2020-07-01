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

let declare_func b name ty = Module.declare_func b.module_ name ty

let find_func b name = Module.find_func b.module_ name

let mark_func_as_defined b f = Module.mark_func_as_defined b.module_ f

let declare_global_var b name ty = Module.declare_global_var b.module_ name ty

let define_type_def b name inner_ty = Module.define_type b.module_ name inner_ty

let build_bb b name =
  let f = get_current_func b in
  let name = Func.prepare_bb_name f name in
  let bb = Term.BB.create name in
  Func.insert_bb f bb;
  bb

let build_let b name v mut =
  let name =
    match name with
    | "" ->
        let f = get_current_func b in
        Func.gen_local_var f
    | _ -> name
  in
  let inst = Term.Let (name, v, mut) in
  let bb = get_current_bb b in
  Term.BB.append_inst bb inst;

  let place = Term.PlaceholderVar { name } in
  Term.{ kind = LVal place; ty = v.ty; span = v.span }

let build_assign b lhs rhs =
  let inst = Term.Assign { lhs; rhs } in
  let bb = get_current_bb b in
  Term.BB.append_inst bb inst

let build_jump b bb =
  let termi = Term.Jump bb.Term.BB.name in
  let bb = get_current_bb b in
  Term.BB.append_inst bb (Term.TerminatorPoint termi)

let build_cond b cond t e =
  let termi = Term.Cond (cond, t.Term.BB.name, e.Term.BB.name) in
  let bb = get_current_bb b in
  Term.BB.append_inst bb (Term.TerminatorPoint termi)

let build_return b =
  let termi = Term.Ret in
  let bb = get_current_bb b in
  Term.BB.append_inst bb (Term.TerminatorPoint termi)
