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
module Rir = Rir

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
    | Analyzed of Rir.Module.t
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
       failwith "[ICE]"

  let analyze m ast =
    (* TODO: create it elsewhare *)
    let package_env = Sema.Env.create "" Sema.Env.Kind.Package None in

    let () =
      let ty = Type.Int in
      let e = Sema.Env.create "i32" (Sema.Env.Kind.Type ty) (Some package_env) in
      let _ = Sema.Env.insert package_env e in
      ()
    in

    let () =
      let ty = Type.Unit in
      let e = Sema.Env.create "unit" (Sema.Env.Kind.Type ty) (Some package_env) in
      let _ = Sema.Env.insert package_env e in
      ()
    in

    let (sema_m, dm) = Sema.Initial.collect_toplevels m.dm ast package_env in
    let subst = Sema.Initial.unify_toplevels sema_m in
    let () = Sema.Initial.show_module sema_m subst in

    let (rir_m, dm) = Sema.Intermediate.transform sema_m subst dm in
    (dm, Analyzed rir_m, Diagnostics.Multi.is_failed dm)

  let analyze m =
    match m.state with
    | Parsed ast when not m.failed ->
       (* TODO: fix *)
       let (dm, state, failed) = analyze m ast in
       {m with dm; state; failed;}

    | _ ->
       m

  let codegen rir_m out =
    let ctx = Codegen.Llvm_gen.create_context () in
    let llvm_m = Codegen.Llvm_gen.create_module ctx rir_m in
    match llvm_m with
    | Ok m ->
       Stdio.printf "LLVM = %s\n" (Codegen.Llvm_gen.debug_string_of m);
       Stdio.Out_channel.write_all out ~data:(Codegen.Llvm_gen.debug_string_of m);
       ()
    | _ -> ()

  let codegen m out =
    match m.state with
    | Analyzed rir_m when not m.failed ->
       (* TODO: fix *)
       let () = codegen rir_m out in
       m

    | _ ->
       m
end

(* TODO: fix *)
let build_module ctx filename out_filename =
  let m = Module.create ctx filename in

  let m = Module.parse m in
  let m = Module.analyze m in
  let () =
    let _ = Module.codegen m out_filename in
    ()
  in
  m
