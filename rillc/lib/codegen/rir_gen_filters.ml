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

  let collect ~extra vars term =
    match term with
    | Term.{ kind = Ref name; _ } -> Map.set vars ~key:name ~data:extra
    | _ -> vars

  let collect_inst ~subst ~extra vars inst =
    match inst with
    (* *)
    | Term.Let (name, (Term.{ ty; _ } as term), mut) ->
        let vars = collect ~extra vars term in
        let storage =
          match (mut, Value_category.should_treat ~subst ty) with
          | (Typing.Type.MutImm, Value_category.AsVal) -> Term.AllocLit
          | (Typing.Type.MutImm, Value_category.AsPtr _) -> Term.AllocStack
          | (Typing.Type.MutMut, _) -> Term.AllocStack
          | (_, _) -> failwith "[ICE] mut is not determined"
        in
        let vars =
          match storage with
          | Term.AllocStack -> Map.set vars ~key:name ~data:extra
          | _ -> vars
        in
        vars
    (* *)
    | Term.Assign { lhs; rhs } ->
        let vars = collect ~extra vars rhs in
        let vars = collect ~extra vars lhs in
        vars
    (* *)
    | _ -> vars

  let apply ~subst _func_name func =
    let extra = Func.{ addressable_e_kind = AddrKindStandard } in
    let vars = Map.empty (module String) in
    let vars =
      Func.fold_bbs func ~init:vars ~f:(fun vars bb ->
          let insts = Term.BB.get_insts bb in
          List.fold_left insts ~init:vars ~f:(collect_inst ~subst ~extra))
    in
    (* Override a ret var if exists *)
    let vars =
      match Func.get_ret_term func with
      | Some Term.{ kind = LVal name; _ } ->
          let extra = Func.{ addressable_e_kind = AddrKindRet } in
          Map.set vars ~key:name ~data:extra
      | Some _ -> failwith "[ICE] unexpected ret val"
      | None -> vars
    in
    vars
end

module Collect_and_set_local_vars_in_func_pass = struct
  module Module = Rir.Module
  module Func = Rir.Func
  module Term = Rir.Term

  let apply ~subst ~addressables func_name func =
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
                  | Term.Let (name, _, _) ->
                      let s =
                        match Map.find addressables name with
                        | Some extra -> Func.AddressableT extra
                        | None -> Func.AddressableF
                      in
                      Some s
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

  let apply ~subst m =
    let funcs = Module.funcs m in
    List.iter funcs ~f:(fun (func_name, func) ->
        let addressables =
          Collect_stack_vars_in_func_pass.apply ~subst func_name func
        in
        Collect_and_set_local_vars_in_func_pass.apply ~subst ~addressables
          func_name func;
        ());
    m
end

let finish ~subst m =
  let m = Modify_funcs_in_module_pass.apply ~subst m in
  m
