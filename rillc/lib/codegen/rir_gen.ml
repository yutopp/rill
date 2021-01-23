(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module Span = Common.Span
module NAst = Sema.Phase3.NAst
module Builtin = Sema.Builtin

type ctx_t = {
  m : Sema.Mod.t;
  ds : Diagnostics.t;
  subst : Typing.Subst.t;
  rir_ctx : Rir.Context.t;
  builtin : Builtin.t;
  root_mod_env : Sema.Env.t;
  return_storage : Rir.Term.t option;
  outer_bbs : outer_bbs_t option;
}

and outer_bbs_t = {
  outer_bb_begin : Rir.Term.BB.t;
  outer_bb_end : Rir.Term.BB.t;
}

let context ~m ~builtin ~root_mod_env =
  let Sema.Mod.{ ds; _ } = m in
  let subst = Sema.Mod.subst_of m in
  let rir_ctx = Rir.Context.create () in
  let return_storage = None in
  let outer_bbs = None in
  { m; ds; subst; rir_ctx; builtin; root_mod_env; return_storage; outer_bbs }

let has_no_value ty =
  match ty with Typing.Type.{ ty = Unit; _ } -> true | _ -> false

let rec generate_expr ~ctx ~builder ast =
  match ast with
  (* *)
  | NAst.{ kind; _ } ->
      let s = NAst.show_kind_t kind in
      failwith
        (Printf.sprintf "Not supported node (Rir_gen.generate_expr): %s" s)

let rec generate_stmt ~ctx ~builder ast =
  let module B = Rir.Builder in
  let ast =
    let NAst.{ ty; _ } = ast in
    let (Typing.Pred.Pred { conds; ty }) = ty in
    let ty = Typing.Subst.subst_type ctx.subst ty in
    let ty = Typing.Pred.Pred { conds; ty } in
    NAst.{ ast with ty }
  in
  match ast with
  (* *)
  | NAst.{ kind = Let { mut; name; expr }; span; _ } ->
      let (t_expr, builder) = generate_stmt ~ctx ~builder expr in
      let term = Rir.Builder.build_let builder name t_expr mut in
      (term, builder)
  (* *)
  | NAst.{ kind = Assign { lhs; rhs }; span; _ } ->
      let (rhs, builder) = generate_stmt ~ctx ~builder rhs in
      let (lhs, builder) = generate_stmt ~ctx ~builder lhs in
      Rir.Builder.build_assign builder lhs rhs;
      (lhs, builder)
  (* *)
  | NAst.{ kind = Seq (node :: nodes); span; _ } ->
      (* TODO: support scope *)
      let ret = generate_stmt ~ctx ~builder node in
      List.fold_left nodes ~init:ret ~f:(fun (_, builder) node ->
          generate_stmt ~ctx ~builder node)
  (* *)
  | NAst.{ kind = If { cond; t; e_opt }; ty; span; _ } ->
      let (Typing.Pred.Pred { ty; _ }) = ty in
      let f = Rir.Builder.get_current_func builder in

      let has_no_value = has_no_value ty in
      (* receiver *)
      let recv =
        match has_no_value with
        | true -> Rir.Term.{ kind = RVal ValueUnit; ty; span }
        | false ->
            let mut = Typing.Type.MutMut in
            B.build_let builder "" Rir.Term.{ kind = Undef; ty; span } mut
      in

      let bb_then = B.build_bb builder "if_then" in

      let (bb_else, bb_end) =
        match Option.is_some e_opt with
        | true ->
            let bb_else = B.build_bb builder "if_else" in
            let bb_end = B.build_bb builder "if_end" in
            (bb_else, bb_end)
        | false ->
            let bb_end = B.build_bb builder "if_end" in
            (bb_end (* To jump to end clause directly *), bb_end)
      in
      let conv_place = to_placeholder ~ctx ~builder cond in
      Rir.Builder.build_cond builder conv_place bb_then bb_else;

      (* then *)
      let () =
        let builder = Rir.Builder.with_current_bb builder bb_then in
        let (term, builder) = generate_stmt ~ctx ~builder t in
        if not has_no_value then B.build_assign builder recv term;

        let bb = B.get_current_bb builder in
        match Rir.Term.BB.get_terminator_opt bb with
        | Some _ -> ()
        | _ -> Rir.Builder.build_jump builder bb_end
      in

      (* else *)
      let () =
        match e_opt with
        | Some e ->
            let builder = Rir.Builder.with_current_bb builder bb_else in
            let (term, builder) = generate_stmt ~ctx ~builder e in
            if not has_no_value then B.build_assign builder recv term;

            let bb = B.get_current_bb builder in
            let () =
              match Rir.Term.BB.get_terminator_opt bb with
              | Some _ -> ()
              | _ -> Rir.Builder.build_jump builder bb_end
            in
            ()
        | None -> ()
      in

      let builder = Rir.Builder.with_current_bb builder bb_end in
      let node = recv in
      (node, builder)
  (* *)
  | NAst.{ kind = Loop inner; ty; span; _ } ->
      let (Typing.Pred.Pred { ty; _ }) = ty in
      (* receiver (unit always) *)
      let recv =
        let mut = Typing.Type.MutImm in
        B.build_let builder "" Rir.Term.{ kind = Undef; ty; span } mut
      in

      let bb_start = B.build_bb builder "loop_start" in
      Rir.Builder.build_jump builder bb_start;

      let bb_end = B.build_bb builder "loop_end" in

      let () =
        let builder = Rir.Builder.with_current_bb builder bb_start in
        let (term, builder) =
          let outer_bbs =
            Some { outer_bb_begin = bb_start; outer_bb_end = bb_end }
          in
          let ctx = { ctx with outer_bbs } in
          generate_stmt ~ctx ~builder inner
        in

        let bb = B.get_current_bb builder in
        match Rir.Term.BB.get_terminator_opt bb with
        | Some _ -> ()
        | _ -> Rir.Builder.build_jump builder bb_start
      in

      let builder = Rir.Builder.with_current_bb builder bb_end in
      let node = recv in
      (node, builder)
  (* *)
  | NAst.{ kind = Break; span; ty } ->
      let (Typing.Pred.Pred { ty; _ }) = ty in
      let bb_next = B.build_bb builder "break_next" in

      let outer_bbs =
        match ctx.outer_bbs with Some bbs -> bbs | None -> failwith "[ICE]"
      in
      let bb = outer_bbs.outer_bb_end in
      Rir.Builder.build_jump builder bb;

      let builder = Rir.Builder.with_current_bb builder bb_next in
      let node = Rir.Term.{ kind = Undef; ty; span } in
      (node, builder)
  (* *)
  | NAst.{ kind = Cast { name }; span; ty } ->
      let (Typing.Pred.Pred { ty; _ }) = ty in
      let place = to_placeholder ~ctx ~builder name in
      let node = Rir.Term.{ kind = Cast place; ty; span } in
      (node, builder)
  (* *)
  | NAst.{ kind = Call { name; args }; span; ty } ->
      let (Typing.Pred.Pred { ty; _ }) = ty in
      let place = to_placeholder ~ctx ~builder name in
      let args = List.map args ~f:(to_placeholder ~ctx ~builder) in
      let node = Rir.Term.{ kind = Call (place, args); ty; span } in
      (node, builder)
  (* *)
  | NAst.{ kind = Index { name; index }; span; ty } ->
      let (Typing.Pred.Pred { ty; _ }) = ty in
      let place = to_placeholder ~ctx ~builder name in
      let index_place = to_placeholder ~ctx ~builder index in
      let node = Rir.Term.{ kind = Index (place, index_place); ty; span } in
      (node, builder)
  (* *)
  | NAst.{ kind = Ref { name }; span; ty } ->
      let (Typing.Pred.Pred { ty; _ }) = ty in
      let place = to_placeholder ~ctx ~builder name in
      let node = Rir.Term.{ kind = Ref place; ty; span } in
      (node, builder)
  (* *)
  | NAst.{ kind = Deref { name }; span; ty } ->
      let (Typing.Pred.Pred { ty; _ }) = ty in
      let place = to_placeholder ~ctx ~builder name in
      let node = Rir.Term.{ kind = Deref place; ty; span } in
      (node, builder)
  (* *)
  | NAst.{ kind = Construct; span; ty } ->
      let (Typing.Pred.Pred { ty; _ }) = ty in
      let node = Rir.Term.{ kind = Construct; ty; span } in
      (node, builder)
  (* *)
  | NAst.{ kind = Var r; ty; span } ->
      let () = scan_external_symbol ~ctx ~builder r in

      let (Typing.Pred.Pred { ty; _ }) = ty in
      let place = to_placeholder ~ctx ~builder r in
      let node = Rir.Term.{ kind = LVal place; ty; span } in
      (node, builder)
  (* *)
  | NAst.{ kind = LitBool v; ty; span } ->
      let (Typing.Pred.Pred { ty; _ }) = ty in
      let node = Rir.Term.{ kind = RVal (ValueBool v); ty; span } in
      (node, builder)
  (* *)
  | NAst.{ kind = LitInt v; ty; span } ->
      let (Typing.Pred.Pred { ty; _ }) = ty in
      let node = Rir.Term.{ kind = RVal (ValueInt v); ty; span } in
      (node, builder)
  (* *)
  | NAst.{ kind = LitString s; ty; span } ->
      let (Typing.Pred.Pred { ty; _ }) = ty in
      let node = Rir.Term.{ kind = RVal (ValueString s); ty; span } in
      (node, builder)
  (* *)
  | NAst.{ kind = LitUnit; ty; span } ->
      let (Typing.Pred.Pred { ty; _ }) = ty in
      let node = Rir.Term.{ kind = RVal ValueUnit; ty; span } in
      (node, builder)
  (* *)
  | NAst.{ kind = LitArrayElem elems; ty; span } ->
      let (Typing.Pred.Pred { ty; _ }) = ty in
      let elems = List.map elems ~f:(to_placeholder ~ctx ~builder) in
      let node = Rir.Term.{ kind = RVal (ValueArrayElem elems); ty; span } in
      (node, builder)
  (* *)
  | NAst.{ kind = Undef; ty; span } ->
      let (Typing.Pred.Pred { ty; _ }) = ty in
      let node = Rir.Term.{ kind = Undef; ty; span } in
      (node, builder)
  (* *)
  | NAst.{ kind; _ } ->
      let s = NAst.show_kind_t kind in
      failwith
        (Printf.sprintf "Not supported node (Rir_gen.generate_stmt): %s" s)

and to_placeholder ~ctx ~builder r =
  let ph =
    match r with
    | NAst.VarLocal { name; label } -> Rir.Term.PlaceholderVar { name }
    | NAst.VarParam { index; name } -> Rir.Term.PlaceholderParam { index; name }
    | NAst.VarGlobal { name } -> Rir.Term.PlaceholderGlobal { name }
    | NAst.VarGlobal2 { nest } ->
        Rir.Module.to_placeholder ~subst:ctx.subst nest
  in
  Rir.Builder.register_monomorphization_candidate builder ph;
  ph

and scan_external_symbol ~ctx ~builder r =
  match r with
  | NAst.VarGlobal2 { nest } ->
      let self_tag = ctx.m |> Sema.Mod.tag in
      let symbol_tag = Path.tag nest in

      let p = Rir.Module.to_placeholder ~subst:ctx.subst nest in
      [%loga.debug "externala = %s" (Rir.Term.to_string_place_holder p)];
      [%loga.debug
        " :: = %s :: %s"
          (Group.Mod_tag.show self_tag)
          (Group.Mod_tag.show symbol_tag)];
      let is_external = not (Group.Mod_tag.equal self_tag symbol_tag) in
      [%loga.debug " -> %b" is_external];
      if is_external then Rir.Builder.append_used_external builder ~path:nest;
      ()
  | _ -> ()

let rec generate_toplevel ~ctx ~builder ast =
  match ast with
  (* *)
  | NAst.{ kind = Import { pkg; mods }; ty; span } ->
      (* DO NOTHING *)
      ()
  (* *)
  | NAst.{ kind = Func { name; ty_sc; kind = NAst.FuncKindDef body }; span; _ }
    ->
      let f = define_func ~ctx ~builder ~name ~ty_sc in
      let builder = Rir.Builder.with_current_func builder f in

      let bb = Rir.Builder.build_bb builder Rir.Func.entry_name in
      let builder = Rir.Builder.with_current_bb builder bb in

      let ret_ty = Rir.Func.get_ret_ty f in
      let ret_term =
        match has_no_value ret_ty with
        | true -> None
        | _ ->
            let mut = Typing.Type.MutMut in
            let t_expr = Rir.Term.{ kind = Undef; ty = ret_ty; span } in
            let t = Rir.Builder.build_let builder "ret" t_expr mut in

            Rir.Func.set_ret_term f t;

            Some t
      in

      let ctx = { ctx with return_storage = ret_term } in
      let (term, builder) = generate_stmt ~ctx ~builder body in
      let () =
        match has_no_value ret_ty with
        | true -> Rir.Builder.build_return builder
        | _ ->
            let ret_term = Option.value_exn ret_term in
            Rir.Builder.build_assign builder ret_term term;
            Rir.Builder.build_return builder
      in
      ()
  (* *)
  | NAst.
      {
        kind = Func { name; ty_sc; kind = NAst.FuncKindExtern extern_name };
        span;
        _;
      } ->
      let (_f : Rir.Func.t) =
        declare_extern_func ~ctx ~builder ~name ~ty_sc ~extern_name
      in
      ()
  (* *)
  | NAst.{ kind = Func { name; ty_sc; kind = NAst.FuncKindDecl }; span; _ } ->
      let (_f : Rir.Func.t) = declare_func ~ctx ~builder ~name ~ty_sc in
      ()
  (* *)
  | NAst.
      {
        kind = Static { name; kind = NAst.VarKindExtern extern_name; ty_sc };
        span;
        _;
      } ->
      declare_extern_static ~ctx ~builder ~name ~ty_sc ~extern_name
  (* *)
  | NAst.{ kind = Struct { name; ty_sc }; ty; span } ->
      define_struct ~ctx ~builder ~name ~ty_sc
  (* *)
  | NAst.{ kind = DefSeq nodes; _ } ->
      List.iter nodes ~f:(generate_toplevel ~ctx ~builder)
  (* *)
  | NAst.{ kind = StmtDispatchTable _; ty; span } ->
      (* DO NOTHING *)
      ()
  (* *)
  | NAst.{ kind; _ } ->
      let s = NAst.show_kind_t kind in
      failwith
        (Printf.sprintf "Not supported node (Rir_gen.generate_toplevel): %s" s)

and declare_func ~ctx ~builder ~name ~ty_sc =
  let ty_sc = Typing.Subst.subst_scheme ctx.subst ty_sc in
  Rir.Builder.declare_func ~subst:ctx.subst builder name ty_sc

and define_func ~ctx ~builder ~name ~ty_sc =
  let f = declare_func ~ctx ~builder ~name ~ty_sc in
  Rir.Func.set_body_form f;
  f

and declare_extern_func ~ctx ~builder ~name ~ty_sc ~extern_name =
  let f = declare_func ~ctx ~builder ~name ~ty_sc in
  Rir.Func.set_extern_form f ~extern_name;
  f

and define_struct ~ctx ~builder ~name ~ty_sc =
  let ty_sc = Typing.Subst.subst_scheme ctx.subst ty_sc in
  Rir.Builder.define_type_def builder name ty_sc

and declare_extern_static ~ctx ~builder ~name ~ty_sc ~extern_name =
  let ty_sc = Typing.Subst.subst_scheme ctx.subst ty_sc in
  let (g : Rir.Global.t) = Rir.Builder.declare_global_var builder name ty_sc in
  Rir.Global.set_extern_form g ~extern_name

let rec import_traits ~ctx ~builder ast =
  match ast with
  (* *)
  | NAst.{ kind = DefSeq nodes; _ } ->
      List.iter nodes ~f:(import_traits ~ctx ~builder)
  (* *)
  | NAst.{ kind = StmtDispatchTable { trait_name; for_ty; mapping }; ty; span }
    ->
      let for_name =
        Rir_gen_filters.to_impl trait_name
          ~env:(Rir_gen_filters.Env.create ())
          for_ty
      in

      Rir.Builder.define_impl ~subst:ctx.subst builder trait_name for_name
        mapping;

      ()
  (* *)
  | NAst.{ kind; _ } ->
      (* DO NOTHING *)
      ()

let generate_module ~ctx ast : Rir.Module.t =
  match ast with
  (* *)
  | NAst.{ kind = Module { nodes; _ }; _ } ->
      let rir_mod = Rir.Module.create ~ctx:ctx.rir_ctx in
      let builder = Rir.Builder.create ~m:rir_mod in

      (*import_deps_xref ~ctx ~builder;*)
      import_traits ~ctx ~builder nodes;
      generate_toplevel ~ctx ~builder nodes;

      let rir_mod =
        Rir_gen_filters.Import_externals_pass.apply rir_mod
          ~root_env:ctx.root_mod_env
      in
      let rir_mod = Rir_gen_filters.Instantiate_pass.apply rir_mod in
      let rir_mod = Rir_gen_filters.Impl_pass.apply rir_mod in
      let rir_mod = Rir_gen_filters.finish rir_mod in
      rir_mod
  (* *)
  | NAst.{ kind; _ } ->
      let s = NAst.show_kind_t kind in
      failwith
        (Printf.sprintf "Not supported node (Rir_gen.generate_module): %s" s)

let write_to ~ch rir =
  Stdio.Out_channel.fprintf ch "%s" (Rir.Module.to_string rir);
  Ok ()
