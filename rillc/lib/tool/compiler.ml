(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module Package = Common.Package
module Triple = Common.Triple
module Mod = Sema.Mod
module Project_buildspace = Pkg_dict
module Pkg_buildspace = Mod_dict

type t = {
  mutable has_fatal : bool;
  workspace : Common.Workspace.t;
  host : (module Triple.PRESET);
  target : (module Triple.PRESET);
  subst_counter : Common.Counter.t;
}

let create ~workspace ~host ~target : t =
  let subst_counter = Common.Counter.create () in
  { has_fatal = false; workspace; host; target; subst_counter }

let return_if_failed ~compiler ms r =
  let open Base.With_return in
  if Mod_state.has_fatal ms then (
    compiler.has_fatal <- true;
    r.return ms )

let preload_module ~compiler ~builtin ~menv ms =
  let open Base.With_return in
  with_return (fun r ->
      (* parse *)
      let ms = Compiler_pipelines.Phases.Parse.to_parsed ~compiler ms in
      return_if_failed ~compiler ms r;

      (* phase1 *)
      let ms =
        Compiler_pipelines.Phases.Phase1_collect_toplevels.to_analyzed ~compiler
          ~builtin ~menv ms
      in
      return_if_failed ~compiler ms r;

      ms)

(* TODO: create root module for pkg *)
let make_root_mod_env_for_pkg ~compiler pkg : Sema.Env.t =
  let root_mod =
    let path = Package.base_dir pkg (* TODO: fix *) in

    (* TODO: fix treatment of subst *)
    let subst_id = Common.Counter.fresh compiler.subst_counter in
    let subst = Typing.Subst.create subst_id in

    Mod.create ~path ~subst ~pkg
  in

  let visibility = Sema.Env.Public in
  let binding_mut = Typing.Type.MutImm in
  let ty = Typing.Type.{ ty = Module; binding_mut; span = Common.Span.undef } in
  Sema.Env.create pkg.Package.name ~parent:None ~visibility ~ty
    ~kind:(Sema.Env.M root_mod) ~lookup_space:Sema.Env.LkGlobal

let make_pkg_space_for_mods ~compiler proj_space builtin pkg =
  let root_mod_env = make_root_mod_env_for_pkg ~compiler pkg in

  (* e.g. pkg:foo, deps:core,std
   * ModEnv:foo (root)
   *)
  let deps = Package.deps_flatten pkg (* TODO: do not use flatten's *) in
  List.iter deps ~f:(fun dep ->
      let dep_space = Project_buildspace.get proj_space ~key:dep in
      let Pkg_buildspace.{ root_mod_env = dep_root_env; _ } = dep_space in

      Sema.Env.insert_meta root_mod_env dep_root_env |> Sema.Phase1.assume_new;
      ());

  (* e.g. pkg:foo, mods:a,b
   * ModEnv:foo (root)
   *   - (deps packages)*
   *   - ModEnv:a
   *   - ModEnv:b
   *)
  let pkg_space = Pkg_buildspace.create root_mod_env in
  let paths = Package.src_paths pkg in
  List.iter paths ~f:(fun path ->
      (* TODO: fix treatment of subst *)
      let subst_id = Common.Counter.fresh compiler.subst_counter in
      let subst = Typing.Subst.create subst_id in

      let m = Mod.create ~path ~subst ~pkg in
      let menv =
        let name =
          Stdlib.Filename.basename path |> Stdlib.Filename.remove_extension
        in
        [%loga.debug "mod -> %s" name];
        let visibility = Sema.Env.Public in
        let binding_mut = Typing.Type.MutImm in
        let ty =
          Typing.Type.{ ty = Module; binding_mut; span = Common.Span.undef }
        in
        (* Per modules have a root_mod_env as a root *)
        Sema.Env.create name ~parent:(Some root_mod_env) ~visibility ~ty
          ~kind:(Sema.Env.M m) ~lookup_space:Sema.Env.LkGlobal
      in

      let ms = Mod_state.create ~m in
      let ms = preload_module ~compiler ~builtin ~menv ms in

      (* TODO: open modules by root module *)
      Sema.Env.insert_meta root_mod_env menv |> Sema.Phase1.assume_new;

      Pkg_buildspace.update pkg_space ms);

  pkg_space

let prepare_linkable_mods_env proj_space self_pkg_space self_mod_path =
  (* Ignore modules which couldn't reach to phase1 *)
  let env =
    let visibility = Sema.Env.Public in
    let binding_mut = Typing.Type.MutImm in
    let ty =
      Typing.Type.{ ty = Module; binding_mut; span = Common.Span.undef }
    in
    Sema.Env.create "" ~parent:None ~visibility ~ty ~kind:Sema.Env.N
      ~lookup_space:Sema.Env.LkGlobal
  in

  (* e.g.
   * - env
   *   - core
   *     - ~~
   *   - std
   *     - ~~
   *   - <self.modules except for self>*
   *)
  let pkg_rels = Project_buildspace.to_alist proj_space in
  List.iter pkg_rels ~f:(fun (_, pkg_space) ->
      let Pkg_buildspace.{ root_mod_env; _ } = pkg_space in
      Sema.Env.insert_meta env root_mod_env |> Sema.Phase1.assume_new);

  env

let cross_ref_mods ~compiler proj_space builtin pkg_space =
  let mod_rels = Pkg_buildspace.to_alist pkg_space in
  List.iter mod_rels ~f:(fun (path, ms) ->
      let external_pkgs_env =
        prepare_linkable_mods_env proj_space pkg_space path
      in
      let ms =
        Compiler_pipelines.Phases.Phase1_declare_toplevels.to_analyzed ~compiler
          ~builtin ~external_pkgs_env ms
      in
      Pkg_buildspace.update pkg_space ms)

let include_pkgs_symbols ~compiler proj_space builtin pkgs =
  List.iter pkgs ~f:(fun pkg ->
      let pkg_space =
        make_pkg_space_for_mods ~compiler proj_space builtin pkg
      in
      cross_ref_mods ~compiler proj_space builtin pkg_space;

      Project_buildspace.update proj_space ~key:pkg ~data:pkg_space)

let to_ir ~compiler pkg_space builtin ms format =
  let open Base.With_return in
  with_return (fun r ->
      (* to rill-ir *)
      let ms =
        Compiler_pipelines.To_rill_ir.to_artifact ~compiler pkg_space builtin ms
      in
      return_if_failed ~compiler ms r;
      let () = match format with Emitter.Rill_ir -> r.return ms | _ -> () in

      (* to llvm-ir *)
      let ms =
        Compiler_pipelines.To_llvm_ir.to_artifact ~compiler pkg_space builtin ms
      in
      return_if_failed ~compiler ms r;
      let () =
        match format with
        | Emitter.Llvm_ir | Emitter.Llvm_bc -> r.return ms
        | _ -> ()
      in

      ms)

let analyze_pkg ~compiler pkg_space builtin pkg =
  (* TODO: now modules can be built in parallel *)
  let mod_rels = Pkg_buildspace.to_alist pkg_space in
  List.iter mod_rels ~f:(fun (path, ms) ->
      let ms =
        Compiler_pipelines.Phases.Phase2.to_analyzed ~compiler ~builtin ms
      in
      Pkg_buildspace.update pkg_space ms)

let assume_no_errors ~compiler ~printer proj_space =
  let open Result.Let_syntax in
  let pkg_rels = Project_buildspace.to_alist proj_space in
  let has_errors = ref false in
  List.iter pkg_rels ~f:(fun (pkg, pkg_space) ->
      let mod_rels = Pkg_buildspace.to_alist pkg_space in
      List.iter mod_rels ~f:(fun (path, ms) ->
          let Mod_state.{ m; phase_result; _ } = ms in
          let Mod.{ ds; _ } = m in

          let ctx = () in
          let () =
            match phase_result with
            | Ok _ -> Diagnostic_printer.print ~ctx printer ds
            | Error failed ->
                Diagnostic_printer.print_with_last_error ~ctx printer
                  (failed, ds)
          in

          if Mod_state.has_errors ms then has_errors := true || !has_errors;

          ()));
  Diagnostic_printer.flush printer;

  let%bind () =
    if !has_errors then Error Errors.There_are_warnings_or_errors else Ok ()
  in
  Ok ()

let compile_pkg_project compiler pkg ~(format : Emitter.t) =
  let proj_space = Project_buildspace.create () in
  let builtin = Sema.Builtin.create () in

  let open With_return in
  with_return (fun r ->
      (* TODO: check state and ds to restrict compilaction *)
      (* TODO: fix deps graph(like prevent cyclic imports) *)
      let dep_pkgs = Package.deps_flatten_with_self pkg in
      List.iter dep_pkgs ~f:(fun pkg ->
          [%loga.debug "pkg -> %s" pkg.Package.name]);

      include_pkgs_symbols ~compiler proj_space builtin dep_pkgs;
      if compiler.has_fatal then r.return proj_space;

      (* TODO: check that all top-levels have bound type-vars *)
      let pkg_space = Project_buildspace.get proj_space ~key:pkg in
      analyze_pkg ~compiler pkg_space builtin pkg;

      (* Build only modules depend on self package *)
      let () =
        let mod_rels = Pkg_buildspace.to_alist pkg_space in
        List.iter mod_rels ~f:(fun (path, ms) ->
            let has_errors = Mod_state.has_errors ms in
            if has_errors then compiler.has_fatal <- true;

            let ms = to_ir ~compiler pkg_space builtin ms format in
            Pkg_buildspace.update pkg_space ms)
      in

      proj_space)

let compile ~compiler ~format ~printer ~pack out_to pkg =
  let open Result.Let_syntax in
  let (module Target : Triple.PRESET) = compiler.target in
  let format =
    Option.value format ~default:(Emitter.default_emitter_of Target.triple)
  in

  let proj_space = compile_pkg_project compiler pkg ~format in
  let%bind () = assume_no_errors ~compiler ~printer proj_space in

  (* Build only modules depend on self package *)
  let pkg_space = Project_buildspace.get proj_space ~key:pkg in
  let%bind () =
    Writer.write_pkg_artifacts ~pkg_space ~pack ~triple:Target.name ~format
      out_to
  in

  Ok ()
