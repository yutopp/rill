(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

(* exports *)
module Syntax = Syntax

module Span = Common.Span
module Diagnostics = Common.Diagnostics

module Sema = Sema
module Hir = Hir
module Rir = Rir
module Codegen_llvm = Codegen_llvm

type t = unit

let create_context () : t =
  ()

module Module = struct
  type t = {
    dm: Diagnostics.Multi.t;
    filename: string;
    state: state_t;
    failed: bool;
  }
  and state_t =
    | Initialized
    | Parsed of Syntax.Ast.t
    | Failed

  let diagnostics m =
    m.dm

  let is_failed m =
    m.failed

  let create ctx filename =
    {
      dm = Diagnostics.Multi.create ();
      filename;
      state = Initialized;
      failed = false;
    }

  let parse m () =
    let (astp, dm) = Syntax.parse_from_file m.dm m.filename in
    let (state, failed) = match astp with
      | Syntax.Complete ast ->
         Stdio.printf "AST = \n%s\n" (Syntax.Ast.sexp_of_t ast |> Sexp.to_string_hum ~indent:2);
         (Parsed ast, false)

      | Syntax.Incomplete ast ->
         Stdio.printf "AST = \n%s\n" (Syntax.Ast.sexp_of_t ast |> Sexp.to_string_hum ~indent:2);
         (Parsed ast, true)

      | Syntax.Failed ->
         (m.state, true)
    in
    (dm, state, failed)

  let parse m =
    match m.state with
    | Initialized ->
       let (dm, state, failed) = parse m () in
       {m with dm; state; failed;}

    | _ ->
       failwith ""

  let analyze m ast =
    (* TODO: create it elsewhare *)
    let package_env = Sema.Env.create "" Sema.Env.Package None in

    let () =
      let ty = Sema.Type.Int in
      let e = Sema.Env.create "i32" (Sema.Env.Type ty) (Some package_env) in
      let _ = Sema.Env.insert package_env e in
      ()
    in

    let () =
      let ty = Sema.Type.Unit in
      let e = Sema.Env.create "unit" (Sema.Env.Type ty) (Some package_env) in
      let _ = Sema.Env.insert package_env e in
      ()
    in

    let (m', dm) = Sema.collect_toplevels m.dm ast package_env in
    let subst = Sema.unify_toplevels m' in
    let () = Sema.show_module m' subst in


    let open Result.Let_syntax in
(*
  let%bind (tnode, ctx) = Sema.sem ast in
    Stdio.eprintf "SEMA = \n%s\n" (Hir.sexp_of_t tnode |> Sexp.to_string_hum ~indent:2);

    let%bind k_form = Rir.KNorm.generate tnode in
    Stdio.eprintf "K form = \n%s\n" (Rir.KNorm.sexp_of_t k_form |> Sexp.to_string_hum ~indent:2);

    let%bind rir = Rir.Trans.transform k_form in
    Stdio.eprintf "RIR = \n%s\n" (Rir.Term.sexp_of_t rir |> Sexp.to_string_hum ~indent:2);
 *)

    m |> return

  let analyze m =
    match m.state with
    | Parsed ast ->
       (* TODO: fix *)
       begin match analyze m ast with
       | Ok m ->
          m
       | Error _ ->
          m
       end
    | _ ->
       m (*failwith ""*)
end

(* TODO: fix *)
let build_module ctx filename =
  let m = Module.create ctx filename in

  let m = Module.parse m in
  let m = Module.analyze m in

  m
