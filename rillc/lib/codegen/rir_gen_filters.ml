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
  module FreshMap = Map.M (Common.Fresh)

  type t = { ty_subst : Typing.Type.t FreshMap.t }

  let create () =
    let ty_subst = Map.empty (module Common.Fresh) in
    { ty_subst }

  let bind subst var ty =
    match (var, ty) with
    | ( Typing.Type.{ ty = Var { var = a; _ }; _ },
        Typing.Type.{ ty = Var { var = b; _ }; _ } )
      when Common.Type_var.equal a b ->
        subst
    | (Typing.Type.{ ty = Var { var = id; bound = BoundForall; _ }; _ }, _) ->
        let { ty_subst } = subst in
        let ty_subst = Map.set ty_subst ~key:id ~data:ty in
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
  let base = Common.Chain.Nest.to_list base in
  let special = Common.Chain.Nest.to_list special in
  List.zip_exn base special
  |> List.fold ~init:(Env.create ()) ~f:(fun env (l_b, l_s) ->
         let Common.Chain.Layer.{ generics_vars = gvs_b; _ } = l_b in
         let Common.Chain.Layer.{ generics_vars = gvs_s; _ } = l_s in
         List.zip_exn gvs_b gvs_s
         |> List.fold ~init:env ~f:(fun env (v_b, v_s) ->
                [%loga.debug "B -> %s" (Typing.Type.to_string v_b)];
                [%loga.debug "S -> %s" (Typing.Type.to_string v_s)];
                Env.bind env v_b v_s))

let to_impl name ~env ty =
  let rec convert layers subst rev_layers =
    match layers with
    | [] ->
        let name = Common.Chain.Nest.from_list (rev_layers |> List.rev) in
        name
    | l :: rest ->
        (* TODO: fix *)
        let Common.Chain.Layer.{ kind; generics_vars; _ } = l in
        let env =
          match generics_vars with [ v ] -> Env.bind env v ty | _ -> env
        in
        let generics_vars =
          List.map generics_vars ~f:(fun v -> Env.subst_type env v)
        in
        let l = Common.Chain.Layer.{ l with generics_vars } in
        let rev_layers = l :: rev_layers in

        convert rest env rev_layers
  in
  let layers = Common.Chain.Nest.to_list name in
  convert layers env []

module Instantiate_pass = struct
  module Module = Rir.Module

  let subst_placeholder ~builder ~subst ph =
    let open Rir.Term in
    match ph with
    | PlaceholderVar _ -> ph
    | PlaceholderParam _ -> ph
    | PlaceholderGlobal _ -> ph
    | PlaceholderGlobal2 { name; dispatch } when not dispatch ->
        let name =
          Common.Chain.Nest.to_list name
          |> List.map ~f:(fun layer ->
                 let Common.Chain.Layer.{ generics_vars; kind; _ } = layer in
                 let generics_vars =
                   List.map ~f:(Env.subst_type subst) generics_vars
                 in
                 let kind =
                   match kind with
                   (*| Common.Chain.Layer.Var ty ->
                       let ty = Env.subst_type subst ty in
                       Common.Chain.Layer.Var ty*)
                   | _ -> kind
                 in
                 Common.Chain.Layer.{ layer with generics_vars })
          |> Common.Chain.Nest.from_list
        in
        [%loga.debug
          "global2: %s"
            (Common.Chain.Nest.to_string ~to_s:Typing.Type.to_string name)];
        PlaceholderGlobal2 { name; dispatch }
    | PlaceholderGlobal2 { name; dispatch } ->
        PlaceholderGlobal2 { name; dispatch }

  let subst_kind ~builder ~subst kind =
    let open Rir.Term in
    let sp ph =
      let ph = subst_placeholder ~builder ~subst ph in
      Rir.Builder.register_monomorphization_candidate builder ph;
      ph
    in
    match kind with
    | Call (r, args) -> Call (r |> sp, args |> List.map ~f:sp)
    | Cast v -> Cast (v |> sp)
    | Index (v, index) -> Index (v |> sp, index |> sp)
    | Ref v -> Ref (v |> sp)
    | Deref v -> Deref (v |> sp)
    | Construct -> kind
    | RVal _ -> kind
    | LVal v -> LVal (v |> sp)
    | Undef -> kind

  let clone_term ~builder ~subst term =
    let Rir.Term.{ ty; kind; _ } = term in
    let ty = Env.subst_type subst ty in
    let kind = subst_kind ~builder ~subst kind in
    let term' = Rir.Term.{ term with kind; ty } in
    [%loga.debug
      "-> %s :: %s" (Rir.Term.to_string_term term') (Typing.Type.to_string ty)];
    term'

  let clone_inst ~builder ~subst inst =
    match inst with
    | Rir.Term.Let (name, term, mut) ->
        let term = clone_term ~builder ~subst term in
        Rir.Term.Let (name, term, mut)
    | Rir.Term.Assign { lhs; rhs } ->
        let lhs = clone_term ~builder ~subst lhs in
        let rhs = clone_term ~builder ~subst rhs in
        Rir.Term.Assign { lhs; rhs }
    | Rir.Term.TerminatorPoint termi -> Rir.Term.TerminatorPoint termi

  let clone_ir ~builder ~subst ~base_f f =
    let base_bbs = Rir.Func.list_reached_bbs base_f in
    (* clone bbs *)
    List.iter base_bbs ~f:(fun base_bb ->
        let bb = Rir.Term.BB.create base_bb.Rir.Term.BB.name in

        (* clone insts *)
        List.iter (Rir.Term.BB.get_insts base_bb) ~f:(fun base_inst ->
            let inst = clone_inst ~builder ~subst base_inst in
            Rir.Term.BB.append_inst bb inst);

        Rir.Func.insert_bb f bb;
        ());

    Option.iter (Rir.Func.get_ret_term base_f) ~f:(fun base_ret_term ->
        let ret_term = clone_term ~builder ~subst base_ret_term in
        Rir.Func.set_ret_term f ret_term)

  let apply m =
    let rec f () =
      match Module.drain_monomorphization_candidates m with
      | [] -> ()
      | candidates ->
          List.iter candidates ~f:(fun name ->
              [%loga.debug
                "cloning...: %s"
                  (Common.Chain.Nest.to_string ~to_s:Typing.Type.to_string name)];

              (* TODO: support other kind of definition *)
              let base_f = Rir.Module.find_generic_func m name in

              let subst = ma base_f.Rir.Func.name name in

              let (Typing.Scheme.ForAll { vars; ty; _ }) =
                base_f.Rir.Func.ty_sc
              in
              let (Typing.Pred.Pred { conds; ty }) = ty in
              let ty = Env.subst_type subst ty in
              let ty = Typing.Pred.Pred { conds; ty } in
              let ty_sc = Typing.Scheme.of_ty ty in

              let builder = Rir.Builder.create ~m in
              let f = Rir.Builder.declare_instance_func builder name ty_sc in
              Rir.Func.set_body_form f;

              clone_ir ~builder ~subst ~base_f f;
              ());
          f ()
    in
    f ();
    m
end

module Impl_pass = struct
  module Module = Rir.Module

  let iter_placeholder ~m ph =
    let open Rir.Term in
    match ph with
    | PlaceholderVar _ -> ()
    | PlaceholderParam _ -> ()
    | PlaceholderGlobal _ -> ()
    | PlaceholderGlobal2 ({ name; dispatch } as r) when dispatch ->
        [%loga.debug
          "dispatch -> %s"
            (Common.Chain.Nest.to_string ~to_s:Typing.Type.to_string name)];
        let Common.Chain.Nest.{ paths; last } = name in

        let trait = Rir.Module.find_trait m paths in
        let impl = Rir.Module.Trait.find trait paths in
        let func_name = Rir.Module.Impl.find impl last in

        r.name <- func_name;
        r.dispatch <- false
    | PlaceholderGlobal2 _ -> ()

  let iter_kind ~m kind =
    let open Rir.Term in
    let sp ph =
      let () = iter_placeholder ~m ph in
      (*Rir.Builder.register_monomorphization_candidate builder ph;*)
      ()
    in
    match kind with
    | Call (r, args) ->
        r |> sp;
        args |> List.iter ~f:sp
    | Cast v -> v |> sp
    | Index (v, index) ->
        v |> sp;
        index |> sp
    | Ref v -> v |> sp
    | Deref v -> v |> sp
    | Construct -> ()
    | RVal _ -> ()
    | LVal v -> v |> sp
    | Undef -> ()

  let iter_term ~m term =
    let Rir.Term.{ kind; _ } = term in
    iter_kind ~m kind

  let iter_inst ~m inst =
    match inst with
    | Rir.Term.Let (name, term, mut) -> iter_term ~m term
    | Rir.Term.Assign { lhs; rhs } ->
        iter_term ~m lhs;
        iter_term ~m rhs
    | Rir.Term.TerminatorPoint termi -> ()

  let iter_ir ~m base_f =
    let base_bbs = Rir.Func.list_reached_bbs base_f in
    (* iter bbs *)
    List.iter base_bbs ~f:(fun base_bb ->
        List.iter (Rir.Term.BB.get_insts base_bb) ~f:(fun base_inst ->
            iter_inst ~m base_inst));

    Option.iter (Rir.Func.get_ret_term base_f) ~f:(fun base_ret_term ->
        iter_term ~m base_ret_term)

  let apply m =
    let funcs = Module.defined_funcs m in
    List.iter funcs ~f:(fun func ->
        let Rir.Func.{ body; _ } = func in
        match body with
        | Some (Rir.Func.BodyFunc _) -> iter_ir ~m func
        | _ -> ());
    m
end
