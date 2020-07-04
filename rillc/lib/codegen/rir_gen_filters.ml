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
    | Term.{ kind = Ref (PlaceholderVar { name }); _ } ->
        Map.set vars ~key:name ~data:extra
    | Term.{ kind = Ref (PlaceholderParam { name; _ }); _ } ->
        Map.set vars ~key:name ~data:extra
    | _ -> vars

  let collect_inst ~extra vars inst =
    match inst with
    (* *)
    | Term.Let (name, (Term.{ ty; _ } as term), mut) ->
        let vars = collect ~extra vars term in
        let storage =
          match (mut, Value_category.should_treat ty) with
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

  let apply func =
    let extra = Func.{ addressable_e_kind = AddrKindStandard } in
    let vars = Map.empty (module String) in
    let vars =
      Func.fold_bbs func ~init:vars ~f:(fun vars bb ->
          let insts = Term.BB.get_insts bb in
          List.fold_left insts ~init:vars ~f:(collect_inst ~extra))
    in
    (* Override a ret var if exists *)
    let vars =
      match Func.get_ret_term func with
      | Some Term.{ kind = LVal (PlaceholderVar { name }); _ } ->
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

  let apply ~addressables func =
    let pre_allocs =
      Func.fold_bbs func ~init:[] ~f:(fun pre_allocs bb ->
          let bb_name = bb.Term.BB.name in
          let Rir.Func.{ name = func_name; _ } = func in
          [%loga.debug
            "func %s / bb!: %s"
              (Common.Chain.Nest.to_string ~to_s:Typing.Type.to_string
                 func_name)
              bb_name];

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

  let apply m =
    let funcs = Module.defined_funcs m in
    List.iter funcs ~f:(fun func ->
        let Rir.Func.{ body; _ } = func in
        match body with
        | Some (Rir.Func.BodyFunc _) ->
            let addressables = Collect_stack_vars_in_func_pass.apply func in
            Collect_and_set_local_vars_in_func_pass.apply ~addressables func;
            ()
        | _ -> ());
    m
end

let finish m =
  let m = Modify_funcs_in_module_pass.apply m in
  m

module Env = struct
  module IntMap = Map.M (Int)

  type t = { ty_subst : Typing.Type.t IntMap.t }

  let create () =
    let ty_subst = Map.empty (module Int) in
    { ty_subst }

  let bind subst var ty =
    match (var, ty) with
    | ( Typing.Type.{ ty = Var { var = a; _ }; _ },
        Typing.Type.{ ty = Var { var = b; _ }; _ } )
      when a = b ->
        subst
    | (Typing.Type.{ ty = Var { var = id; bound = BoundForall }; _ }, _) ->
        let { ty_subst } = subst in
        let ty_subst = Map.add_exn ty_subst ~key:id ~data:ty in
        { subst with ty_subst }
    | _ -> failwith "[ICE]"

  let rec subst_type subst ty =
    match ty with
    | Typing.Type.{ ty = Var { var = uni_id; _ }; binding_mut; span } -> (
        let { ty_subst; _ } = subst in
        match Map.find ty_subst uni_id with
        | Some ty' ->
            let ty = subst_type subst ty' in
            Typing.Type.{ ty with binding_mut }
        | None -> failwith "[ICE] cannot determined" )
    | Typing.Type.{ ty = Array { elem; n }; _ } as tty ->
        let elem = subst_type subst elem in
        Typing.Type.{ tty with ty = Array { elem; n } }
    | Typing.Type.{ ty = Func { params; ret; linkage }; _ } as tty ->
        let params = List.map ~f:(subst_type subst) params in
        let ret = subst_type subst ret in
        Typing.Type.{ tty with ty = Func { params; ret; linkage } }
    | Typing.Type.{ ty = Pointer { mut; elem }; _ } as tty ->
        let elem = subst_type subst elem in
        Typing.Type.{ tty with ty = Pointer { mut; elem } }
    (* meta *)
    | Typing.Type.{ ty = Type inner_ty; _ } as tty ->
        let inner_ty = subst_type subst inner_ty in
        Typing.Type.{ tty with ty = Type inner_ty }
    | alt -> alt
end

let ma base special =
  List.zip_exn base special
  |> List.fold ~init:(Env.create ()) ~f:(fun env (l_b, l_s) ->
         let Common.Chain.Layer.{ generics_vars = gvs_b; _ } = l_b in
         let Common.Chain.Layer.{ generics_vars = gvs_s; _ } = l_s in
         List.zip_exn gvs_b gvs_s
         |> List.fold ~init:env ~f:(fun env (v_b, v_s) ->
                [%loga.debug "B -> %s" (Typing.Type.to_string v_b)];
                [%loga.debug "S -> %s" (Typing.Type.to_string v_s)];
                Env.bind env v_b v_s))

module Instantiate_pass = struct
  module Module = Rir.Module

  let clone_term ~subst term =
    let Rir.Term.{ ty; _ } = term in
    let ty = Env.subst_type subst ty in
    Rir.Term.{ term with ty }

  let clone_inst ~subst inst =
    match inst with
    | Rir.Term.Let (name, term, mut) ->
        let term = clone_term ~subst term in
        Rir.Term.Let (name, term, mut)
    | Rir.Term.Assign { lhs; rhs } ->
        let lhs = clone_term ~subst lhs in
        let rhs = clone_term ~subst rhs in
        Rir.Term.Assign { lhs; rhs }
    | Rir.Term.TerminatorPoint termi -> Rir.Term.TerminatorPoint termi

  let clone_ir ~subst ~base_f f =
    let base_bbs = Rir.Func.list_reached_bbs base_f in
    (* close bbs *)
    List.iter base_bbs ~f:(fun base_bb ->
        let bb = Rir.Term.BB.create base_bb.Rir.Term.BB.name in

        (* clone insts *)
        List.iter (Rir.Term.BB.get_insts base_bb) ~f:(fun base_inst ->
            let inst = clone_inst ~subst base_inst in
            Rir.Term.BB.append_inst bb inst);

        Rir.Func.insert_bb f bb;
        ())

  let apply m =
    let hints = Module.hints m in
    Map.iter hints ~f:(fun name ->
        (* TODO: support other kind of definition *)
        let base_f = Rir.Module.find_generic_func m name in

        let s = Rir.Func.to_string ~indent:0 base_f in

        let subst = ma base_f.Rir.Func.name name in

        let (Typing.Scheme.ForAll (vars, ty)) = base_f.Rir.Func.ty_sc in
        let ty = Env.subst_type subst ty in
        let ty_sc = Typing.Scheme.of_ty ty in

        let builder = Rir.Builder.create ~m in
        let f = Rir.Builder.declare_instance_func builder name ty_sc in
        Rir.Func.set_body_form f;

        clone_ir ~subst ~base_f f;
        ());
    m
end

let instantiate m =
  let m = Instantiate_pass.apply m in
  m
