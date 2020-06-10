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
module Mod = Sema.Mod

type t = {
  mutable has_fatal : bool;
  workspace : Common.Workspace.t;
  subst_counter : Common.Counter.t;
}

let create workspace : t =
  let subst_counter = Common.Counter.create () in
  { has_fatal = false; workspace; subst_counter }

type codegen_phase_t = CodegenPhaseRir | CodegenPhaseLLVM

type phase_t =
  | CannotParsed
  | Parsed of Syntax.Ast.t
  | SemaP1 of Sema.Phase1.TopAst.t
  | SemaP2 of Sema.Phase2.TAst.t
[@@deriving show]

type artifact_t =
  | ArtifactRir of Rir.Module.t
  | ArtifactLlvm of Codegen.Llvm_gen.Module.t
[@@deriving show]

type failed_t = phase_t * Diagnostics.Elem.t

module Phases = struct
  let parse ~compiler m =
    let ds = m.Mod.ds in
    let path = m.Mod.path in
    Syntax.parse_from_file ~ds path
    |> Result.map_error ~f:(fun e -> (CannotParsed, e))

  let phase1_collect_toplevels ~compiler ~builtin m menv ast =
    let open Result.Let_syntax in
    let ds = m.Mod.ds in
    let subst = m.Mod.subst in

    let%bind p1ast =
      let ctx = Sema.Phase1.context ~parent:menv ~ds ~subst ~builtin in
      Sema.Phase1.collect_toplevels ~ctx ast
      |> Result.map_error ~f:(fun e -> (Parsed ast, e))
    in
    m.Sema.Mod.subst <- subst;
    return (p1ast, subst)

  let phase1_declare_toplevels ~compiler ~builtin m pkg_env p1ast =
    let open Result.Let_syntax in
    let ds = m.Mod.ds in
    let subst = m.Mod.subst in

    let%bind subst =
      let ctx = Sema.Phase1_1.context ~ds ~subst ~pkg_env in
      Sema.Phase1_1.declare_toplevels ~ctx p1ast
      |> Result.map ~f:(fun e -> Sema.Phase1_1.(ctx.subst))
      |> Result.map_error ~f:(fun e -> (SemaP1 p1ast, e))
    in
    m.Sema.Mod.subst <- subst;

    return p1ast

  let phase2 ~compiler ~builtin m p1ast =
    let open Result.Let_syntax in
    let ds = m.Mod.ds in
    let subst = m.Mod.subst in

    let%bind (p2ast, subst) =
      let ctx = Sema.Phase2.context ~ds ~subst ~builtin in
      Sema.Phase2.into_typed_tree ~ctx p1ast
      |> Result.map ~f:(fun n -> (n, Sema.Phase2.(ctx.subst)))
      |> Result.map_error ~f:(fun e -> (SemaP1 p1ast, e))
    in
    m.Sema.Mod.subst <- subst;

    return p2ast

  let phase3 ~compiler m p2ast =
    let ds = m.Mod.ds in
    let subst = m.Mod.subst in

    let ctx = Sema.Phase3.context ~ds ~subst in
    let env = Sema.Phase3.Env.create () in
    Sema.Phase3.normalize ~ctx ~env p2ast
end

module ModState = struct
  type t = { m : Mod.t; phase_result : (phase_t, failed_t) Result.t }

  and phase_t =
    | Phase1CollectTopLevels of Sema.Phase1.TopAst.t
    | Phase1DeclareTopLevels of Sema.Phase1.TopAst.t
    | Phase2 of Sema.Phase2.TAst.t
    | ArtifactRir of Rir.Module.t
    | ArtifactLlvm of Codegen.Llvm_gen.Module.t

  let create ~m ~phase_result = { m; phase_result }

  let is_failed m = Result.is_error m.phase_result
end

module ModDict = struct
  type t = { root_mod_env : Sema.Env.t; rel : (string, ModState.t) Hashtbl.t }

  let create root_mod_env =
    { root_mod_env; rel = Hashtbl.create (module String) }

  let update dict ms =
    let key = ms.ModState.m.Mod.path in
    Hashtbl.set dict.rel ~key ~data:ms

  let to_alist d = Hashtbl.to_alist d.rel
end

module PkgDict = struct
  type t = { rel : (Package.id_t, Package.t * ModDict.t) Hashtbl.t }

  let create () = { rel = Hashtbl.create (module Int) }

  let update d ~key ~data : unit =
    Hashtbl.add_exn d.rel ~key:key.Package.id ~data:(key, data)

  let get d ~key =
    let (_, mod_dict) = Hashtbl.find_exn d.rel key.Package.id in
    mod_dict

  let to_alist d = Hashtbl.to_alist d.rel |> List.map ~f:snd
end

let preload_module compiler builtin menv m =
  let open Result.Let_syntax in
  (* TODO: check state and ds to restrict compilaction *)
  let%bind (_stete, parsed) = Phases.parse ~compiler m in

  let%bind (p1ast, subst) =
    Phases.phase1_collect_toplevels ~compiler ~builtin m menv parsed
  in

  Ok p1ast

let rec preload_pkg compiler dict builtin pkg =
  (* preload deps *)
  let deps = Package.deps pkg in
  List.iter deps ~f:(preload_pkg compiler dict builtin);

  (* preload self *)
  let mod_dict =
    (* TODO: create root module for pkg *)
    let root_mod_env =
      let root_mod =
        let path = Package.base_dir pkg (* TODO: fix *) in

        (* TODO: fix treatment of subst *)
        let subst_id = Common.Counter.fresh compiler.subst_counter in
        let subst = Typing.Subst.create subst_id in

        Mod.create ~path ~subst ~pkg
      in
      let visibility = Sema.Env.Public in
      let binding_mut = Typing.Type.MutImm in
      let ty =
        Typing.Type.{ ty = Module; binding_mut; span = Common.Span.undef }
      in
      Sema.Env.create pkg.Package.name ~parent:None ~visibility ~ty
        ~ty_w:(Sema.Env.M root_mod)
    in

    let dict = ModDict.create root_mod_env in

    let paths = Package.src_paths pkg in
    List.iter paths ~f:(fun path ->
        (* TODO: fix treatment of subst *)
        let subst_id = Common.Counter.fresh compiler.subst_counter in
        let subst = Typing.Subst.create subst_id in

        let m = Mod.create ~path ~subst ~pkg in
        let menv =
          let name =
            Caml.Filename.basename path |> Caml.Filename.remove_extension
          in
          let visibility = Sema.Env.Public in
          let binding_mut = Typing.Type.MutImm in
          let ty =
            Typing.Type.{ ty = Module; binding_mut; span = Common.Span.undef }
          in
          Sema.Env.create name ~parent:None ~visibility ~ty ~ty_w:(Sema.Env.M m)
        in

        let phase_result =
          preload_module compiler builtin menv m
          |> Result.map ~f:(fun p0 -> ModState.Phase1CollectTopLevels p0)
        in
        let ms = ModState.create ~m ~phase_result in
        if ModState.is_failed ms then compiler.has_fatal <- true;

        (* TODO: open modules by root module *)
        Sema.Env.insert_meta root_mod_env menv;

        ModDict.update dict ms);
    dict
  in
  PkgDict.update dict ~key:pkg ~data:mod_dict

let rec preload_pkg_cont compiler dict builtin pkg_env pkg =
  (* preload deps *)
  let deps = Package.deps pkg in
  List.iter deps ~f:(preload_pkg_cont compiler dict builtin pkg_env);

  let mod_dict = PkgDict.get dict ~key:pkg in
  let mod_rels = ModDict.to_alist mod_dict in
  List.iter mod_rels ~f:(fun (path, ms) ->
      match ms.ModState.phase_result with
      | Ok (ModState.Phase1CollectTopLevels p1ast) ->
          let m = ms.ModState.m in
          let phase_result =
            Phases.phase1_declare_toplevels ~compiler ~builtin m pkg_env p1ast
            |> Result.map ~f:(fun s -> ModState.Phase1DeclareTopLevels s)
          in
          let ms = ModState.{ ms with phase_result } in
          ModDict.update mod_dict ms
      | _ -> ())

let to_ir compiler dict builtin code_gen_phase ms =
  match ms.ModState.phase_result with
  | Ok (ModState.Phase2 p2ast) ->
      let m = ms.ModState.m in

      (* TODO: check that there are no errors in ds *)
      let p3ast = Phases.phase3 ~compiler m p2ast in

      let open With_return in
      with_return (fun r ->
          let ds = m.Mod.ds in
          let subst = m.Mod.subst in

          (* Generate Rill-ir *)
          let rir =
            let ctx = Codegen.Rir_gen.context ~ds ~subst ~builtin in
            Codegen.Rir_gen.generate_module ~ctx p3ast
          in
          let m =
            let phase_result = Ok (ModState.ArtifactRir rir) in
            ModState.{ ms with phase_result }
          in
          if Poly.equal code_gen_phase CodegenPhaseRir then r.return m;

          (* Generate LLVM-ir *)
          let llvm =
            let ctx = Codegen.Llvm_gen.context ~ds ~subst ~builtin in
            Codegen.Llvm_gen.generate_module ~ctx rir
          in
          let ms =
            let phase_result = Ok (ModState.ArtifactLlvm llvm) in
            ModState.{ ms with phase_result }
          in
          ms)
  | _ -> ms

let build_pkg_internal compiler dict builtin emit pkg =
  let mod_dict = PkgDict.get dict ~key:pkg in
  let mod_rels = ModDict.to_alist mod_dict in
  List.iter mod_rels ~f:(fun (path, ms) ->
      match ms.ModState.phase_result with
      | Ok (ModState.Phase1DeclareTopLevels p1ast) ->
          let m = ms.ModState.m in
          let ds = m.Mod.ds in
          let phase_result =
            Phases.phase2 ~compiler ~builtin m p1ast
            |> Result.map ~f:(fun p0 -> ModState.Phase2 p0)
          in
          let ms = ModState.{ ms with phase_result } in
          ModDict.update mod_dict ms;

          (* *)
          let has_fatal = Mod.has_errors m in

          if has_fatal then compiler.has_fatal <- true;

          if not has_fatal then
            let ms = to_ir compiler dict builtin emit ms in
            ModDict.update mod_dict ms
      | _ -> ())

let build_mod_env pkg_dict =
  (* Ignore modules which couldn't reach to phase1 *)
  let visibility = Sema.Env.Public in
  let binding_mut = Typing.Type.MutImm in
  let ty = Typing.Type.{ ty = Module; binding_mut; span = Common.Span.undef } in
  let env = Sema.Env.create "" ~parent:None ~visibility ~ty ~ty_w:Sema.Env.N in

  let pkg_rels = PkgDict.to_alist pkg_dict in
  List.iter pkg_rels ~f:(fun (_, mod_dict) ->
      let root_mod_env = mod_dict.ModDict.root_mod_env in
      Sema.Env.insert_meta env root_mod_env);

  env

let build_pkg compiler pkg ~code_gen_phase =
  let pkg_dict = PkgDict.create () in
  let builtin = Sema.Builtin.create () in

  let open With_return in
  with_return (fun r ->
      (* TODO: check state and ds to restrict compilaction *)
      preload_pkg compiler pkg_dict builtin pkg;
      if compiler.has_fatal then r.return pkg_dict;

      let mod_env = build_mod_env pkg_dict in

      (* TODO: prevent cyclic imports *)
      preload_pkg_cont compiler pkg_dict builtin mod_env pkg;
      if compiler.has_fatal then r.return pkg_dict;

      (* TODO: check that all top-levels have bound type-vars *)
      build_pkg_internal compiler pkg_dict builtin code_gen_phase pkg;

      pkg_dict)
