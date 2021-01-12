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

(* function *)
let with_current_func ctx f = { ctx with current_func = Some f }

let get_current_func ctx = Option.value_exn ctx.current_func

let with_current_bb ctx bb = { ctx with current_bb = Some bb }

let get_current_bb ctx = Option.value_exn ctx.current_bb

let get_current_state ctx = (ctx.current_func, ctx.current_bb)

let declare_func ~subst b name ty_sc =
  Module.declare_func ~subst b.module_ name ty_sc

let define_impl ~subst b trait_name for_name mapping =
  Module.define_impl ~subst b.module_ trait_name for_name mapping

let declare_instance_func b name ty_sc =
  Module.declare_instance_func b.module_ name ty_sc

(* static *)
let declare_global_var b name ty_sc =
  Module.declare_global_var b.module_ name ty_sc

(* type *)
let define_type_def b name ty_sc = Module.define_type b.module_ name ty_sc

(* generics *)
let register_monomorphization_candidate b name =
  Module.register_monomorphization_candidate b.module_ name

let register_monomorphization_candidate b ph =
  let open Term in
  match ph with
  | PlaceholderVar _ -> ()
  | PlaceholderParam _ -> ()
  | PlaceholderGlobal _ -> ()
  | PlaceholderGlobal2 { name; dispatch = false } ->
      register_monomorphization_candidate b name
  | PlaceholderGlobal2 { name; _ } -> ()

(* *)
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
