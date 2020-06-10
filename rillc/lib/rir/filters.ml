(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

module Collect_stack_vars_and_pass_opts_pass = struct
  let apply m =
    let funcs = Module.funcs m in
    List.iter funcs ~f:(fun (func_name, func) ->
        let (reached_bbs, pre_allocs) =
          Func.fold_bbs func ~init:([], [])
            ~f:(fun (reached_bbs, pre_allocs) bb ->
              let bb_name = bb.Term.BB.name in
              [%loga.debug "func %s / bb!: %s" func_name bb_name];

              let insts = Term.BB.get_insts bb in
              let insts_let_stack =
                List.filter insts ~f:(fun inst ->
                    match inst with
                    | Term.Let (_, _, Term.AllocStack) -> true
                    | _ -> false)
              in

              let reached_bbs = bb_name :: reached_bbs in
              let pre_allocs = (bb_name, insts_let_stack) :: pre_allocs in
              (reached_bbs, pre_allocs))
        in

        (* Save pre_allocs *)
        Func.set_pre_allocs func pre_allocs;

        (* Remove unuses bbs *)
        (* TODO *)
        ());
    m
end

let finish m =
  let m = Collect_stack_vars_and_pass_opts_pass.apply m in
  m
