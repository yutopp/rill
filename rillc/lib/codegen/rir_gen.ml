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
  match ast with
  (* *)
  | NAst.{ kind = Let { name; expr }; span; _ } ->
      let (t_expr, builder) = generate_stmt ~ctx ~builder expr in
      Rir.Builder.build_let builder name t_expr;

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
  | NAst.{ kind = Call { name; args }; span; ty } ->
      let node = Rir.Term.{ kind = Call (name, args); ty; span } in
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
  | NAst.{ kind; _ } ->
      let s = NAst.show_kind_t kind in
      failwith
        (Printf.sprintf "Not supported node (Rir_gen.generate_stmt): %s" s)

let generate_toplevel ~ctx ~builder ast =
  match ast with
  (* *)
  | NAst.{ kind = Func { name; kind = NAst.FuncKindDef body }; ty; span } ->
      let f = Rir.Func.create ~ty ~extern_name:None in
      let builder = Rir.Builder.with_current_func builder f in

      let bb = Rir.Term.BB.create "entry" in
      Rir.Func.insert_bb f bb;
      let builder = Rir.Builder.with_current_bb builder bb in

      let (_body, builder) = generate_stmt ~ctx ~builder body in

      (* TODO: fix *)
      Rir.Builder.build_ret builder "";

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