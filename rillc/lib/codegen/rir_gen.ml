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
}

let context ~ds ~subst ~builtin =
  let rir_ctx = Rir.Context.create () in
  { ds; subst; rir_ctx; builtin }

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
  | NAst.{ kind = Let { name; expr }; span; _ } ->
      let (t_expr, builder) = generate_stmt ~ctx ~builder expr in
      Rir.Builder.build_let builder name t_expr;

      let term =
        Rir.Term.{ kind = Undef; ty = ctx.builtin.Builtin.unit_; span }
      in
      (term, builder)
  | NAst.{ kind = Assign { lhs; rhs }; ty; span; _ } ->
      Rir.Builder.build_assign builder lhs rhs;

      let term =
        Rir.Term.{ kind = Undef; ty = ctx.builtin.Builtin.unit_; span }
      in
      (term, builder)
  (* *)
  | NAst.{ kind = Seq (node :: nodes); span; _ } ->
      (* TODO: support scope *)
      let ret = generate_stmt ~ctx ~builder node in
      List.fold_left nodes ~init:ret ~f:(fun (_, builder) node ->
          generate_stmt ~ctx ~builder node)
  (* *)
  | NAst.{ kind = If { cond; t; e_opt = Some e }; ty; span; _ } ->
      let f = Rir.Builder.get_current_func builder in

      (*
      (* receiver *)
      let _ = B.build_let builder "" Rir.Term.{ kind = Undef; ty; span } in
       *)
      let bb_then = Rir.Term.BB.create "if_then" in
      Rir.Func.insert_bb f bb_then;

      let bb_else = Rir.Term.BB.create "if_else" in
      Rir.Func.insert_bb f bb_else;

      Rir.Builder.build_cond builder cond bb_then bb_else;

      let bb_end = Rir.Term.BB.create "if_end" in
      Rir.Func.insert_bb f bb_end;

      (* then *)
      let () =
        let builder = Rir.Builder.with_current_bb builder bb_then in
        let (_, builder) = generate_stmt ~ctx ~builder t in

        match Rir.Term.BB.get_terminator_opt bb_then with
        | Some _ -> ()
        | _ -> Rir.Builder.build_jump builder bb_end
      in

      (* else *)
      let () =
        let builder = Rir.Builder.with_current_bb builder bb_else in
        let (_, builder) = generate_stmt ~ctx ~builder e in

        match Rir.Term.BB.get_terminator_opt bb_else with
        | Some _ -> ()
        | _ -> Rir.Builder.build_jump builder bb_end
      in

      let builder = Rir.Builder.with_current_bb builder bb_end in
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
  | NAst.{ kind = Func { name; kind = NAst.FuncKindDef body }; ty; span } ->
      let (_, ret_ty) = Typing.Type.assume_func_ty ty in
      let f = Rir.Func.create ~ty ~extern_name:None in
      let builder = Rir.Builder.with_current_func builder f in

      let bb = Rir.Term.BB.create_entry () in
      Rir.Func.insert_bb f bb;
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
      rir_mod
  (* *)
  | NAst.{ kind; _ } ->
      let s = NAst.show_kind_t kind in
      failwith
        (Printf.sprintf "Not supported node (Rir_gen.generate_module): %s" s)
