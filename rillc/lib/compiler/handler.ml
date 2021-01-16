(*
 * Copyright yutopp 2021 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

module Sema_helper = struct
  module Mod = Sema.Mod

  (* TODO: Find root module for pkg like a lib.rs *)
  let create_root_mod ~pkg_struct : Mod.t =
    let visibility = Sema.Env.Public in
    let binding_mut = Typing.Type.MutImm in
    let ty =
      Typing.Type.{ ty = Module; binding_mut; span = Common.Span.undef }
    in
    let pty = Typing.Pred.of_type ty in
    let ty_sc = Typing.Scheme.of_ty pty in
    let menv =
      let name = Structure.tag pkg_struct |> Package_tag.name in
      Sema.Env.create name ~parent:None ~visibility ~ty_sc ~kind:Sema.Env.M
        ~lookup_space:Sema.Env.LkGlobal
    in

    let root_mod =
      let path = Structure.base_dir pkg_struct in
      Mod.create ~path ~menv ~pkg:1
    in

    root_mod

  let load_mod ~root_mod ~path =
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
      let pty = Typing.Pred.of_type ty in
      let ty_sc = Typing.Scheme.of_ty pty in
      (* Per modules have a root_mod_env as a root *)
      let root_mod_env = Mod.menv root_mod in
      Sema.Env.create name ~parent:(Some root_mod_env) ~visibility ~ty_sc
        ~kind:Sema.Env.M ~lookup_space:Sema.Env.LkGlobal
    in

    Mod.create ~path ~menv ~pkg:1

  let insert_source_mod ~root_mod mh =
    (* TODO: open modules by root module *)
    Sema.Env.insert_meta (Mod.menv root_mod) (mh |> Mod_handle.inner |> Mod.menv)
    |> Sema.Phase1_collect_toplevels.assume_new

  let insert_dep_meta ~root_mod ph =
    let dep_root_mod = Package_handle.root_mod ph in

    Sema.Env.insert_meta (Mod.menv root_mod) (Mod.menv dep_root_mod)
    |> Sema.Phase1_collect_toplevels.assume_new
end

let return_if_failed mh r =
  let open Base.With_return in
  if Mod_handle.has_fatal mh then r.return ()

let load_module_symbols mh ~builtin ~subst =
  let open Base.With_return in
  with_return (fun r ->
      (* parse *)
      let () = Pipelines.Phases.Parse.to_parsed mh in
      return_if_failed mh r;

      let () =
        Pipelines.Phases.Phase1_collect_toplevels.to_analyzed mh ~builtin ~subst
      in
      return_if_failed mh r;

      ())

let declare_module_symbols mh ~builtin ~subst =
  let subst =
    Pipelines.Phases.Phase1_declare_toplevels.to_analyzed mh ~builtin ~subst
  in
  let subst =
    Pipelines.Phases.Phase1_declare_impls.to_analyzed mh ~builtin ~subst
  in
  subst

let assume_no_errors ~printer ~ph =
  let open Result.Let_syntax in
  let has_errors = ref false in

  let mhs = Package_handle.mod_handles ph in
  List.iter mhs ~f:(fun mh ->
      let Mod_handle.{ m; phase_result; _ } = mh in
      let Sema.Mod.{ ds; _ } = m in

      let ctx = () in
      let () =
        match phase_result with
        | Ok _ -> Diagnostic_printer.print ~ctx printer ds
        | Error failed ->
            Diagnostic_printer.print_with_last_error ~ctx printer (failed, ds)
      in

      if Mod_handle.has_errors mh then has_errors := true || !has_errors;

      ());
  Diagnostic_printer.flush printer;

  let%bind () =
    if !has_errors then Error Errors.There_are_warnings_or_errors else Ok ()
  in
  Ok ()

let load_pkg_symbols ~ws ~pkg_handle =
  let builtin = Workspace.builtin ws in

  let pkg_struct = Package_handle.structure pkg_handle in
  let subst = Package_handle.subst pkg_handle in

  (* Create a module for package root *)
  let root_mod = Sema_helper.create_root_mod ~pkg_struct in
  Package_handle.set_root_mod pkg_handle ~root_mod;

  let has_fatal = ref false in

  let deps = Structure.dependencies pkg_struct in
  List.iter deps ~f:(fun dep ->
      [%loga.debug "dep_pkg -> %s" dep.Package_tag.name];
      let dep_ph =
        match Workspace.find_pkg_handle ws ~tag:dep with
        | Some v -> v
        | None -> failwith "[ICE] dep is not loaded"
      in
      Sema_helper.insert_dep_meta ~root_mod dep_ph);

  let paths = Structure.src_paths pkg_struct in
  List.iter paths ~f:(fun path ->
      (* Create a module for source *)
      let m = Sema_helper.load_mod ~root_mod ~path in

      let mh = Mod_handle.create ~m in
      load_module_symbols mh ~builtin ~subst;
      Sema_helper.insert_source_mod ~root_mod mh;

      Package_handle.add_mod_handle pkg_handle ~mh;
      ())

let declare_package_symbols ~ws ~pkg_handle =
  let builtin = Workspace.builtin ws in

  (* NOTE: this loop must be run in sequentially and same order in preload
   * (to merge toplevel substs)
   *)
  let mhs = Package_handle.mod_handles pkg_handle in
  List.iter mhs ~f:(fun mh ->
      let subst = Package_handle.subst pkg_handle in
      let subst = declare_module_symbols mh ~builtin ~subst in
      let () = Package_handle.update_subst pkg_handle ~subst in
      ())

let decide_exported_interfaces ~ws ~pkg_handle =
  let subst = Package_handle.subst pkg_handle in

  let mhs = Package_handle.mod_handles pkg_handle in
  List.iter mhs ~f:(fun mh ->
      let m = Mod_handle.inner mh in
      let menv = Sema.Mod.menv m in
      Sema.Fix_env.fix menv ~subst;
      ())

let analyze_package ~ws ~pkg_handle =
  let builtin = Workspace.builtin ws in
  let subst = Package_handle.subst pkg_handle in

  (* TODO: now modules can be built in parallel *)
  let mhs = Package_handle.mod_handles pkg_handle in
  List.iter mhs ~f:(fun mh ->
      (* now the result of analysis does not affect to toplevel subst.
       * the result is closed to the per modules
       *)
      let () = Pipelines.Phases.Phase2.to_analyzed mh ~builtin ~subst in
      ())

let generate_ir_package ~ws ~pkg_handle ~emitter =
  let builtin = Workspace.builtin ws in
  let subst = Package_handle.subst pkg_handle in

  (* TODO: now modules can be built in parallel *)
  let mhs = Package_handle.mod_handles pkg_handle in
  List.iter mhs ~f:(fun mh ->
      let open Base.With_return in
      with_return (fun r ->
          Pipelines.Phases.Phase3.to_normalized mh;
          return_if_failed mh r;

          (* to rill-ir *)
          Pipelines.Phases.To_rill_ir.to_artifact mh ~builtin ~pkg_handle;
          return_if_failed mh r;
          let () =
            match emitter with Emitter.Rill_ir -> r.return () | _ -> ()
          in

          (* to llvm-ir *)
          Pipelines.Phases.To_llvm_ir.to_artifact mh ~builtin ~pkg_handle;
          return_if_failed mh r;
          let () =
            match emitter with
            | Emitter.Llvm_ir | Emitter.Llvm_bc -> r.return ()
            | _ -> ()
          in
          ()))

let write_artifacts_package ~ws ~pkg_handle ~pack ~triple ~emitter ~out_to =
  let open Result.Let_syntax in
  (* TODO: now modules can be built in parallel *)
  let%bind assets =
    let mhs = Package_handle.mod_handles pkg_handle in
    List.fold_result mhs ~init:[] ~f:(fun assets mh ->
        let%bind asset =
          match Mod_handle.phase_result mh with
          | Ok (Mod_handle.Artifact art) ->
              Ok Asset.{ art; path = Mod_handle.path mh }
          | _ -> Error (Errors.Failed_to_export_artifact "[ICE] not artifacts")
        in
        Ok (asset :: assets))
  in
  Writer.write_assets ~assets ~pack ~triple ~emitter ~out_to

let load_pkg_as_header ~ws ~printer ~pkg_handle =
  let open Result.Let_syntax in
  load_pkg_symbols ~ws ~pkg_handle;
  let%bind () = assume_no_errors ~printer ~ph:pkg_handle in

  declare_package_symbols ~ws ~pkg_handle;
  let%bind () = assume_no_errors ~printer ~ph:pkg_handle in

  decide_exported_interfaces ~ws ~pkg_handle;

  Ok ()

let compile ~ws ~printer ~pkg_handle ~pack ~triple ~emitter ~out_to =
  let open Result.Let_syntax in
  load_pkg_symbols ~ws ~pkg_handle;
  let%bind () = assume_no_errors ~printer ~ph:pkg_handle in

  declare_package_symbols ~ws ~pkg_handle;
  let%bind () = assume_no_errors ~printer ~ph:pkg_handle in

  analyze_package ~ws ~pkg_handle;
  let%bind () = assume_no_errors ~printer ~ph:pkg_handle in

  generate_ir_package ~ws ~pkg_handle ~emitter;
  let%bind () = assume_no_errors ~printer ~ph:pkg_handle in

  let%bind paths =
    write_artifacts_package ~ws ~pkg_handle ~pack ~triple ~emitter ~out_to
  in

  Ok paths

let link_bin ~spec ~lib_dirs ~pkgs ~objs ~out =
  let open Result.Let_syntax in
  let lib_names =
    List.map pkgs ~f:(fun pkg ->
        let pkg_struct = Package_handle.structure pkg in
        Structure.lib_names pkg_struct)
    |> List.join
  in

  let%bind () = Common.Os.cc_exe ~spec ~lib_dirs ~lib_names ~objs ~out () in
  Ok ()
