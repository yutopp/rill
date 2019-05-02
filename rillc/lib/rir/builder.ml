(*
 * Copyright yutopp 2018 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

type t = {
  module_: Module.t;
  mutable current_func: Term.Func.t option;
  mutable current_bb: Term.BB.t option;
}

let create ~m =
  {
    module_ = m;
    current_func = None;
    current_bb = None;
  }

let get_module ctx =
  ctx.module_

let set_current_func ctx f =
  ctx.current_func <- Some f

let get_current_func ctx =
  Option.value_exn ctx.current_func

let set_current_bb ctx bb =
  ctx.current_bb <- Some bb

let get_current_bb ctx =
  Option.value_exn ctx.current_bb

let get_current_state ctx =
  (ctx.current_func, ctx.current_bb)

let set_current_state ctx s =
  let (cf_opt, cbb_opt) = s in
  cf_opt |> Option.iter ~f:(set_current_func ctx);
  cbb_opt |> Option.iter ~f:(set_current_bb ctx)

let register_func_def ctx _name f =
  (* TODO: support name *)
  Module.append_func ctx.module_ f

let build_let ctx name v =
  let inst = Term.Let (name, v) in
  let bb = get_current_bb ctx in
  Term.BB.append bb inst
