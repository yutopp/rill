(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

module Collect_stack_vars_in_func_pass = struct
  module Module = Rir.Module
  module Func = Rir.Func
  module Term = Rir.Term

  let collect vars term =
    match term with
    | Term.{ kind = Ref name; _ } -> Set.add vars name
    | _ -> vars

  let collect_inst vars inst =
    match inst with
    | Term.Let (name, term, mut, ty) ->
        let vars = collect vars term in
        let storage =
          match (mut, Value_category.should_treat ty) with
          | (Typing.Type.MutImm, Value_category.AsVal) -> Term.AllocLit
          | (Typing.Type.MutImm, Value_category.AsPtr) -> Term.AllocStack
          | (Typing.Type.MutMut, _) -> Term.AllocStack
          | (_, _) -> failwith "[ICE] mut is not determined"
        in
        let vars =
          match storage with Term.AllocStack -> Set.add vars name | _ -> vars
        in
        vars
    | Term.Assign { lhs; rhs } ->
        let vars = collect vars rhs in
        let vars = collect vars lhs in
        vars
    | _ -> vars

  let apply _func_name func =
    Func.fold_bbs func
      ~init:(Set.empty (module String))
      ~f:(fun vars bb ->
        let insts = Term.BB.get_insts bb in
        List.fold_left insts ~init:vars ~f:collect_inst)
end

module Collect_and_set_local_vars_in_func_pass = struct
  module Module = Rir.Module
  module Func = Rir.Func
  module Term = Rir.Term

  let apply ~addressables func_name func =
    let pre_allocs =
      Func.fold_bbs func ~init:[] ~f:(fun pre_allocs bb ->
          let bb_name = bb.Term.BB.name in
          [%loga.debug "func %s / bb!: %s" func_name bb_name];

          let insts = Term.BB.get_insts bb in
          let insts_let_with_addr =
            let open Option.Let_syntax in
            List.filter_map insts ~f:(fun inst ->
                let%bind storage =
                  match inst with
                  | Term.Let (name, _, _, _) when Set.mem addressables name ->
                      Some Func.AddressableT
                  | Term.Let (_, _, _, _) -> Some Func.AddressableF
                  | _ -> None
                in
                Some (inst, storage))
          in

          let pre_alloc =
            Func.{ p_bb_name = bb_name; p_insts = insts_let_with_addr }
          in
          let pre_allocs = pre_alloc :: pre_allocs in
          pre_allocs)
    in

    (* Save pre_allocs *)
    Func.set_pre_allocs func pre_allocs
end

module Modify_funcs_in_module_pass = struct
  module Module = Rir.Module

  let apply m =
    let funcs = Module.funcs m in
    List.iter funcs ~f:(fun (func_name, func) ->
        let addressables =
          Collect_stack_vars_in_func_pass.apply func_name func
        in
        Collect_and_set_local_vars_in_func_pass.apply ~addressables func_name
          func;
        ());
    m
end

let finish m =
  let m = Modify_funcs_in_module_pass.apply m in
  m
