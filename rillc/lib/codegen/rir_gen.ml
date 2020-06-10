(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module Span = Common.Span
module Diagnostics = Common.Diagnostics
module NAst = Sema.Phase3.NAst
module Builtin = Sema.Builtin

type ctx_t = {
  ds : Diagnostics.t;
  subst : Typing.Subst.t;
  rir_ctx : Rir.Context.t;
  builtin : Builtin.t;
  outer_bbs : outer_bbs_t option;
}

and outer_bbs_t = {
  outer_bb_begin : Rir.Term.BB.t;
  outer_bb_end : Rir.Term.BB.t;
}

let context ~ds ~subst ~builtin =
  let rir_ctx = Rir.Context.create () in
  let outer_bbs = None in
  { ds; subst; rir_ctx; builtin; outer_bbs }

let rec generate_expr ~ctx ~builder ast =
  match ast with
  (* *)
  | NAst.{ kind; _ } ->
      let s = NAst.show_kind_t kind in
      failwith
        (Printf.sprintf "Not supported node (Rir_gen.generate_expr): %s" s)

let rec generate_stmt ~ctx ~builder ast =
  let module B = Rir.Builder in
  match ast with
  (* *)
  | NAst.{ kind = Let { mut; name; expr }; ty; span; _ } ->
      let (t_expr, builder) = generate_stmt ~ctx ~builder expr in
      let storage =
        match mut with
        | Typing.Type.MutImm -> Rir.Term.AllocLit
        | Typing.Type.MutMut -> Rir.Term.AllocStack
      in
      let term = Rir.Builder.build_let builder name t_expr storage in
      (term, builder)
  (* *)
  | NAst.{ kind = Assign { lhs; rhs }; ty; span; _ } ->
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
      let f = Rir.Builder.get_current_func builder in

      let has_no_value =
        Typing.Subst.subst_type ctx.subst ty |> Typing.Type.has_no_value
      in
      (* receiver *)
      let recv =
        match has_no_value with
        | true -> Rir.Term.{ kind = RVal ValueUnit; ty; span }
        | false ->
            B.build_let builder ""
              Rir.Term.{ kind = Undef; ty; span }
              Rir.Term.AllocStack
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
      Rir.Builder.build_cond builder cond bb_then bb_else;

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
      (* receiver (unit always) *)
      let recv =
        B.build_let builder ""
          Rir.Term.{ kind = Undef; ty; span }
          Rir.Term.AllocLit
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
  | NAst.{ kind = Call { name; args }; span; ty } ->
      let node = Rir.Term.{ kind = Call (name, args); ty; span } in
      (node, builder)
  (* *)
  | NAst.{ kind = Var id; ty; span } ->
      let node = Rir.Term.{ kind = LVal id; ty; span } in
      (node, builder)
  (* *)
  | NAst.{ kind = VarParam i; ty; span } ->
      let node = Rir.Term.{ kind = LValParam i; ty; span } in
      (node, builder)
  (* *)
  | NAst.{ kind = LitBool v; ty; span } ->
      let node = Rir.Term.{ kind = RVal (ValueBool v); ty; span } in
      (node, builder)
  (* *)
  | NAst.{ kind = LitInt v; ty; span } ->
      let node = Rir.Term.{ kind = RVal (ValueInt v); ty; span } in
      (node, builder)
  (* *)
  | NAst.{ kind = LitString s; ty; span } ->
      let node = Rir.Term.{ kind = RVal (ValueString s); ty; span } in
      (node, builder)
  (* *)
  | NAst.{ kind = LitUnit; ty; span } ->
      let node = Rir.Term.{ kind = RVal ValueUnit; ty; span } in
      (node, builder)
  (* *)
  | NAst.{ kind = Undef; ty; span } ->
      let node = Rir.Term.{ kind = Undef; ty; span } in
      (node, builder)
  (* *)
  | NAst.{ kind; _ } ->
      let s = NAst.show_kind_t kind in
      failwith
        (Printf.sprintf "Not supported node (Rir_gen.generate_stmt): %s" s)

let generate_toplevel ~ctx ~builder ast =
  match ast with
  (* *)
  | NAst.{ kind = Import { pkg; mods }; ty; span } ->
      (* DO NOTHING *)
      ()
  (* *)
  | NAst.{ kind = Func { name; kind = NAst.FuncKindDef body }; ty; span } ->
      let (_, ret_ty) = Typing.Type.assume_func_ty ty in
      let f = Rir.Func.create ~ty ~extern_name:None in
      let builder = Rir.Builder.with_current_func builder f in

      let bb = Option.value_exn (Rir.Func.get_entry_bb f) in
      let builder = Rir.Builder.with_current_bb builder bb in

      let (term, builder) = generate_stmt ~ctx ~builder body in
      let () =
        match ret_ty with
        | Typing.Type.{ ty = Unit; _ } -> Rir.Builder.build_return_void builder
        | _ -> Rir.Builder.build_return builder term
      in

      Rir.Builder.register_func_def builder name f
  (* *)
  | NAst.
      { kind = Func { name; kind = NAst.FuncKindExtern extern_name }; ty; span }
    ->
      let f = Rir.Func.create ~ty ~extern_name:(Some extern_name) in
      Rir.Builder.register_func_def builder name f
  (* *)
  | NAst.{ kind; _ } ->
      let s = NAst.show_kind_t kind in
      failwith
        (Printf.sprintf "Not supported node (Rir_gen.generate_toplevel): %s" s)

let generate_module ~ctx ast =
  match ast with
  (* *)
  | NAst.{ kind = Module nodes; _ } ->
      let rir_mod = Rir.Module.create ~ctx:ctx.rir_ctx in
      let builder = Rir.Builder.create ~m:rir_mod in
      List.iter nodes ~f:(generate_toplevel ~ctx ~builder);
      let rir_mod = Rir.Filters.finish rir_mod in
      rir_mod
  (* *)
  | NAst.{ kind; _ } ->
      let s = NAst.show_kind_t kind in
      failwith
        (Printf.sprintf "Not supported node (Rir_gen.generate_module): %s" s)

let write_to ~ch rir =
  Stdio.Out_channel.fprintf ch "%s" (Rir.Module.show rir);
  Ok ()
