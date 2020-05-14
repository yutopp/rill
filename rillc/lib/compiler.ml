(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module Diagnostics = Common.Diagnostics

type t = { workspace : Common.Workspace.t }

type phase_t =
  | CannotParsed
  | Parsed of Syntax.Ast.t
  | SemaP1 of Sema.Phase1.TopAst.t
  | SemaP2 of Sema.Phase2.TAst.t
[@@deriving sexp_of]

let create workspace : t = { workspace }

type artifact_t =
  | ArtifactRir of Rir.Module.t
  | ArtifactLlvm of Codegen.Llvm_gen.Module.t
[@@deriving show]

let dump_artifact art =
  match art with
  | ArtifactRir m -> ()
  | ArtifactLlvm m ->
      Stdio.printf "%s" (Codegen.Llvm_gen.L.string_of_llmodule m)

type failed_t = phase_t * Diagnostics.Elem.t

type build_result_t = (artifact_t, failed_t) Result.t

let build_from_file compiler ~ds ~path : build_result_t =
  let open Result.Let_syntax in
  let%bind (_stete, parsed) =
    Syntax.parse_from_file ~ds path
    |> Result.map_error ~f:(fun e -> (CannotParsed, e))
  in
  (* TODO: check state and ds to restrict compilaction *)
  let subst = Typing.Subst.create () in
  let builtin = Sema.Builtin.create () in
  let%bind p1ast =
    let ctx = Sema.Phase1.context ~parent:None ~ds ~subst ~builtin in
    Sema.Phase1.collect_toplevels ~ctx parsed
    |> Result.map_error ~f:(fun e -> (Parsed parsed, e))
  in
  let%bind subst =
    let ctx = Sema.Phase1_1.context ~ds ~subst in
    Sema.Phase1_1.declare_toplevels ~ctx p1ast
    |> Result.map ~f:(fun e -> Sema.Phase1_1.(ctx.subst))
    |> Result.map_error ~f:(fun e -> (SemaP1 p1ast, e))
  in

  (* TODO: check all Vars are decidable *)

  (* Now we can build objects per compilation units *)
  let%bind (p2ast, subst) =
    let ctx = Sema.Phase2.context ~ds ~subst ~builtin in
    Sema.Phase2.into_typed_tree ~ctx p1ast
    |> Result.map ~f:(fun n -> (n, Sema.Phase2.(ctx.subst)))
    |> Result.map_error ~f:(fun e -> (SemaP1 p1ast, e))
  in

  let%bind () =
    match Diagnostics.errors ds with
    | [] -> Ok ()
    | _ ->
        let e = new Common.Reasons.compilation_stopped in
        let span = Common.Span.create_path ~path in
        let elm = Diagnostics.Elem.error ~span e in
        Error (SemaP2 p2ast, elm)
  in

  let%bind () =
    match Diagnostics.warnings ds with
    | [] -> Ok ()
    | _ ->
        let e = new Common.Reasons.compilation_stopped in
        let span = Common.Span.create_path ~path in
        let elm = Diagnostics.Elem.error ~span e in
        Error (SemaP2 p2ast, elm)
  in

  (* TODO: check that there are no errors in ds *)
  let p3ast =
    let ctx = Sema.Phase3.context ~ds ~subst in
    let env = Sema.Phase3.Env.create () in
    Sema.Phase3.normalize ~ctx ~env p2ast
  in

  let rir =
    let ctx = Codegen.Rir_gen.context ~ds ~subst ~builtin in
    Codegen.Rir_gen.generate_module ~ctx p3ast
  in

  let llvm =
    let ctx = Codegen.Llvm_gen.context ~ds ~subst ~builtin in
    Codegen.Llvm_gen.generate_module ~ctx rir
  in

  (* TODO: check that there are no errors in ds *)
  Ok (ArtifactLlvm llvm)

type error_t = unit

type completed_t = (artifact_t * Diagnostics.t) list

type incompleted_t = (failed_t * Diagnostics.t) list

type build_results_t = (completed_t * incompleted_t, error_t) Result.t

let build_from_files compiler ~paths : build_results_t =
  let phases_with_ds =
    List.map paths ~f:(fun path ->
        let ds = Diagnostics.create () in
        let res = build_from_file compiler ~ds ~path in
        let () =
          match res with
          | Ok n ->
              let s = show_artifact_t n in
              Stdio.eprintf "%s\n" s
          | _ -> Stdio.eprintf "Failed to build_from_file\n"
        in
        (res, ds))
  in
  let (completed, incompleted) =
    List.partition_map phases_with_ds ~f:(fun (res, ds) ->
        match res with Ok m -> `Fst (m, ds) | Error last -> `Snd (last, ds))
  in
  Ok (completed, incompleted)
