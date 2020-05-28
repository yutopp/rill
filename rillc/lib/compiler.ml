(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module Diagnostics = Common.Diagnostics
module Package = Common.Package

type t = { mutable has_fatal : bool; workspace : Common.Workspace.t }

let create workspace : t = { has_fatal = false; workspace }

type codegen_phase_t = CodegenPhaseRir | CodegenPhaseLLVM

type phase_t =
  | CannotParsed
  | Parsed of Syntax.Ast.t
  | SemaP1 of Sema.Phase1.TopAst.t
  | SemaP2 of Sema.Phase2.TAst.t
[@@deriving sexp_of]

type artifact_t =
  | ArtifactRir of Rir.Module.t
  | ArtifactLlvm of Codegen.Llvm_gen.Module.t
[@@deriving show]

let dump_rir rir = ()

let dump_llvm llvm =
  Stdio.printf "%s" (Codegen.Llvm_gen.L.string_of_llmodule llvm)

type failed_t = phase_t * Diagnostics.Elem.t

module Phases = struct
  let parse ~compiler ~ds ~path =
    Syntax.parse_from_file ~ds path
    |> Result.map_error ~f:(fun e -> (CannotParsed, e))

  let phase1_collect_toplevels ~compiler ~ds ~builtin ast =
    let open Result.Let_syntax in
    (* TODO: fix treatment of subst *)
    let subst = Typing.Subst.create () in
    let%bind p1ast =
      let ctx = Sema.Phase1.context ~parent:None ~ds ~subst ~builtin in
      Sema.Phase1.collect_toplevels ~ctx ast
      |> Result.map_error ~f:(fun e -> (Parsed ast, e))
    in
    return (p1ast, subst)

  let phase1_declare_toplevels ~compiler ~ds ~builtin ~subst p1ast =
    let open Result.Let_syntax in
    let%bind subst =
      let ctx = Sema.Phase1_1.context ~ds ~subst in
      Sema.Phase1_1.declare_toplevels ~ctx p1ast
      |> Result.map ~f:(fun e -> Sema.Phase1_1.(ctx.subst))
      |> Result.map_error ~f:(fun e -> (SemaP1 p1ast, e))
    in
    return (p1ast, subst)

  let phase2 ~compiler ~ds ~builtin ~subst p1ast =
    let open Result.Let_syntax in
    let%bind (p2ast, subst) =
      let ctx = Sema.Phase2.context ~ds ~subst ~builtin in
      Sema.Phase2.into_typed_tree ~ctx p1ast
      |> Result.map ~f:(fun n -> (n, Sema.Phase2.(ctx.subst)))
      |> Result.map_error ~f:(fun e -> (SemaP1 p1ast, e))
    in
    return (p2ast, subst)

  let phase3 ~compiler ~ds ~subst p2ast =
    let ctx = Sema.Phase3.context ~ds ~subst in
    let env = Sema.Phase3.Env.create () in
    Sema.Phase3.normalize ~ctx ~env p2ast
end

type error_t = unit

type completed_t = (artifact_t * Diagnostics.t) list

type incompleted_t = (failed_t * Diagnostics.t) list

type build_results_t = (completed_t * incompleted_t, error_t) Result.t

module Mod = struct
  type t = {
    path : string;
    phase_result : (phase_t, failed_t) Result.t;
    ds : Diagnostics.t;
  }

  and phase_t =
    | Noop
    | Phase1CollectTopLevels of (Sema.Phase1.TopAst.t * Typing.Subst.t)
    | Phase1DeclareTopLevels of (Sema.Phase1.TopAst.t * Typing.Subst.t)
    | Phase2 of (Sema.Phase2.TAst.t * Typing.Subst.t)
    | ArtifactRir of Rir.Module.t
    | ArtifactLlvm of Codegen.Llvm_gen.Module.t

  let create ~path =
    let phase_result = Ok Noop in
    let ds = Diagnostics.create () in
    { path; phase_result; ds }

  let is_failed m = Result.is_error m.phase_result
end

module ModDict = struct
  type t = { rel : (string, Mod.t) Hashtbl.t }

  let create () = { rel = Hashtbl.create (module String) }

  let update dict m =
    let key = m.Mod.path in
    Hashtbl.set dict.rel ~key ~data:m

  let to_alist d = Hashtbl.to_alist d.rel
end

module PkgDict = struct
  type t = { rel : (Package.id_t, ModDict.t) Hashtbl.t }

  let create () = { rel = Hashtbl.create (module Int) }

  let update d ~key ~data : unit =
    Hashtbl.add_exn d.rel ~key:key.Package.id ~data

  let get d ~key = Hashtbl.find_exn d.rel key.Package.id

  let to_alist d = Hashtbl.to_alist d.rel
end

let preload_module compiler builtin ds path =
  let open Result.Let_syntax in
  (* TODO: check state and ds to restrict compilaction *)
  let%bind (_stete, parsed) = Phases.parse ~compiler ~ds ~path in

  Phases.phase1_collect_toplevels ~compiler ~ds ~builtin parsed

let rec preload_pkg compiler dict builtin pkg =
  (* preload deps *)
  let deps = Package.deps pkg in
  List.iter deps ~f:(preload_pkg compiler dict builtin);

  (* preload self *)
  let mod_dict =
    let dict = ModDict.create () in
    let paths = Package.src_paths pkg in
    List.iter paths ~f:(fun path ->
        let m = Mod.create ~path in

        let base_dir = Package.base_dir pkg in
        let path = Caml.Filename.concat base_dir path in

        let phase_result =
          preload_module compiler builtin m.Mod.ds path
          |> Result.map ~f:(fun p0 -> Mod.Phase1CollectTopLevels p0)
        in
        let m = Mod.{ m with phase_result } in
        if Mod.is_failed m then compiler.has_fatal <- true;

        ModDict.update dict m);
    dict
  in
  PkgDict.update dict ~key:pkg ~data:mod_dict

let rec preload_pkg_cont compiler dict builtin pkg =
  (* preload deps *)
  let deps = Package.deps pkg in
  List.iter deps ~f:(preload_pkg_cont compiler dict builtin);

  let mod_dict = PkgDict.get dict ~key:pkg in
  let mod_rels = ModDict.to_alist mod_dict in
  List.iter mod_rels ~f:(fun (path, m) ->
      let ds = m.Mod.ds in
      match m.Mod.phase_result with
      | Ok (Mod.Phase1CollectTopLevels (p1ast, subst)) ->
          let phase_result =
            Phases.phase1_declare_toplevels ~compiler ~ds ~builtin ~subst p1ast
            |> Result.map ~f:(fun s -> Mod.Phase1DeclareTopLevels s)
          in
          let m = Mod.{ m with phase_result } in
          ModDict.update mod_dict m
      | _ -> ())

let to_ir compiler dict builtin m =
  let ds = m.Mod.ds in
  match m.Mod.phase_result with
  | Ok (Mod.Phase2 (p2ast, subst)) ->
      (*
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
      *)

      (* TODO: check that there are no errors in ds *)
      let p3ast = Phases.phase3 ~compiler ~ds ~subst p2ast in

      let codegen_phase = CodegenPhaseLLVM in

      let open With_return in
      with_return (fun r ->
          let rir =
            let ctx = Codegen.Rir_gen.context ~ds ~subst ~builtin in
            Codegen.Rir_gen.generate_module ~ctx p3ast
          in
          let m =
            let phase_result = Ok (Mod.ArtifactRir rir) in
            Mod.{ m with phase_result }
          in
          if Poly.equal codegen_phase CodegenPhaseRir then r.return m;

          let llvm =
            let ctx = Codegen.Llvm_gen.context ~ds ~subst ~builtin in
            Codegen.Llvm_gen.generate_module ~ctx rir
          in
          let m =
            let phase_result = Ok (Mod.ArtifactLlvm llvm) in
            Mod.{ m with phase_result }
          in
          m)
  | _ -> m

let build_pkg_internal compiler dict builtin pkg =
  let mod_dict = PkgDict.get dict ~key:pkg in
  let mod_rels = ModDict.to_alist mod_dict in
  List.iter mod_rels ~f:(fun (path, m) ->
      let ds = m.Mod.ds in
      match m.Mod.phase_result with
      | Ok (Mod.Phase1DeclareTopLevels (p1ast, subst)) ->
          let phase_result =
            Phases.phase2 ~compiler ~ds ~builtin ~subst p1ast
            |> Result.map ~f:(fun p0 -> Mod.Phase2 p0)
          in
          let m = Mod.{ m with phase_result } in
          let m = to_ir compiler dict builtin m in
          ModDict.update mod_dict m
      | _ -> ())

let build_pkg compiler pkg =
  let pkg_dict = PkgDict.create () in
  let builtin = Sema.Builtin.create () in

  let open With_return in
  with_return (fun r ->
      (* TODO: check state and ds to restrict compilaction *)
      preload_pkg compiler pkg_dict builtin pkg;
      if compiler.has_fatal then r.return pkg_dict;

      preload_pkg_cont compiler pkg_dict builtin pkg;
      if compiler.has_fatal then r.return pkg_dict;

      (* TODO: check that all top-levels have bound type-vars *)
      build_pkg_internal compiler pkg_dict builtin pkg;

      pkg_dict)
