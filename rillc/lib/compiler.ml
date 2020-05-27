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

type phase_t =
  | CannotParsed
  | Parsed of Syntax.Ast.t
  | SemaP1 of Sema.Phase1.TopAst.t
  | SemaP2 of Sema.Phase2.TAst.t
[@@deriving sexp_of]

let create workspace : t = { has_fatal = false; workspace }

type get_artifact_t = GetRir | GetLLVM

type artifact_t =
  | ArtifactRir of Rir.Module.t
  | ArtifactLlvm of Codegen.Llvm_gen.Module.t
[@@deriving show]

let dump_rir rir = ()

let dump_llvm llvm =
  Stdio.printf "%s" (Codegen.Llvm_gen.L.string_of_llmodule llvm)

let dump_artifact art =
  match art with
  | ArtifactRir m -> ()
  | ArtifactLlvm m ->
      Stdio.printf "%s" (Codegen.Llvm_gen.L.string_of_llmodule m)

type failed_t = phase_t * Diagnostics.Elem.t

type build_result_t = (artifact_t, failed_t) Result.t

module Phases = struct
  let parse ~compiler ~ds ~path =
    Syntax.parse_from_file ~ds path
    |> Result.map_error ~f:(fun e -> (CannotParsed, e))

  type phase0_t =
    (Sema.Phase1.TopAst.t * Typing.Subst.t) * t * Diagnostics.t * Sema.Builtin.t

  let phase_1_0 ~compiler ~ds ~builtin ast : (phase0_t, failed_t) Result.t =
    let open Result.Let_syntax in
    (* TODO: fix treatment of subst *)
    let subst = Typing.Subst.create () in
    let builtin = Sema.Builtin.create () in
    let%bind p1ast =
      let ctx = Sema.Phase1.context ~parent:None ~ds ~subst ~builtin in
      Sema.Phase1.collect_toplevels ~ctx ast
      |> Result.map_error ~f:(fun e -> (Parsed ast, e))
    in
    return ((p1ast, subst), compiler, ds, builtin)

  type phase1_t = Sema.Phase1.TopAst.t * Typing.Subst.t

  let phase1_cont ((p1ast, subst), compiler, ds, builtin) =
    let open Result.Let_syntax in
    let%bind subst =
      let ctx = Sema.Phase1_1.context ~ds ~subst in
      Sema.Phase1_1.declare_toplevels ~ctx p1ast
      |> Result.map ~f:(fun e -> Sema.Phase1_1.(ctx.subst))
      |> Result.map_error ~f:(fun e -> (SemaP1 p1ast, e))
    in
    return (p1ast, subst)

  let phase1 ~compiler ~ds ~builtin ast =
    let open Result.Let_syntax in
    let%bind cont = phase_1_0 ~compiler ~ds ~builtin ast in
    phase1_cont cont

  type phase2_t = Sema.Phase2.TAst.t * Typing.Subst.t

  let phase3 ~compiler ~ds ~subst p2ast =
    let ctx = Sema.Phase3.context ~ds ~subst in
    let env = Sema.Phase3.Env.create () in
    Sema.Phase3.normalize ~ctx ~env p2ast
end

let build_from_file compiler ~ds ~path : build_result_t =
  let open Result.Let_syntax in
  let%bind (_stete, parsed) = Phases.parse ~compiler ~ds ~path in

  (* TODO: check state and ds to restrict compilaction *)
  let builtin = Sema.Builtin.create () in
  let%bind (p1ast, subst) = Phases.phase1 ~compiler ~ds ~builtin parsed in

  (* TODO: check that all top-levels have bound type-vars *)

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
  let p3ast = Phases.phase3 ~compiler ~ds ~subst p2ast in

  let get = GetLLVM in

  let open With_return in
  with_return (fun r ->
      let rir =
        let ctx = Codegen.Rir_gen.context ~ds ~subst ~builtin in
        Codegen.Rir_gen.generate_module ~ctx p3ast
      in
      if Poly.equal get GetRir then r.return (Ok (ArtifactRir rir));

      let llvm =
        let ctx = Codegen.Llvm_gen.context ~ds ~subst ~builtin in
        Codegen.Llvm_gen.generate_module ~ctx rir
      in

      Ok (ArtifactLlvm llvm))

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

module Mod = struct
  type t = {
    path : string;
    phase_result : (phase_t, failed_t) Result.t;
    ds : Diagnostics.t;
  }

  and phase_t =
    | Noop
    | Phase0 of Phases.phase0_t
    | Phase1 of Phases.phase1_t
    | Phase2 of Phases.phase2_t
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

let preload_path compiler builtin ds path =
  let open Result.Let_syntax in
  (* TODO: check state and ds to restrict compilaction *)
  let%bind (_stete, parsed) = Phases.parse ~compiler ~ds ~path in
  (* TODO: check state and ds to restrict compilaction *)
  let%bind cont = Phases.phase_1_0 ~compiler ~ds ~builtin parsed in
  return cont

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
          preload_path compiler builtin m.Mod.ds path
          |> Result.map ~f:(fun p0 -> Mod.Phase0 p0)
        in
        let m = Mod.{ m with phase_result } in
        if Mod.is_failed m then compiler.has_fatal <- true;
        ModDict.update dict m);
    dict
  in
  PkgDict.update dict ~key:pkg ~data:mod_dict

let rec preload_pkg2 compiler dict builtin pkg =
  (* preload deps *)
  let deps = Package.deps pkg in
  List.iter deps ~f:(preload_pkg2 compiler dict builtin);

  let mod_dict = PkgDict.get dict ~key:pkg in
  let mod_rels = ModDict.to_alist mod_dict in
  List.iter mod_rels ~f:(fun (path, m) ->
      let ds = m.Mod.ds in
      match m.Mod.phase_result with
      | Ok (Mod.Phase0 cont) ->
          let phase_result =
            Phases.phase1_cont cont |> Result.map ~f:(fun s -> Mod.Phase1 s)
          in
          let m = Mod.{ m with phase_result } in
          ModDict.update mod_dict m
      | _ -> ())

let to_ir compiler dict builtin m =
  let ds = m.Mod.ds in
  match m.Mod.phase_result with
  | Ok (Mod.Phase2 (p2ast, subst)) ->
      (* TODO: check that there are no errors in ds *)
      let p3ast = Phases.phase3 ~compiler ~ds ~subst p2ast in

      let get = GetLLVM in

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
          if Poly.equal get GetRir then r.return m;

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
      | Ok (Mod.Phase1 (p1ast, subst)) ->
          let phase_result =
            let ctx = Sema.Phase2.context ~ds ~subst ~builtin in
            Sema.Phase2.into_typed_tree ~ctx p1ast
            |> Result.map ~f:(fun n -> Mod.Phase2 (n, Sema.Phase2.(ctx.subst)))
            |> Result.map_error ~f:(fun e -> (SemaP1 p1ast, e))
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

      preload_pkg2 compiler pkg_dict builtin pkg;
      if compiler.has_fatal then r.return pkg_dict;

      (* TODO: check that all top-levels have bound type-vars *)
      build_pkg_internal compiler pkg_dict builtin pkg;

      pkg_dict)
