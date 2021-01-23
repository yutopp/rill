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
      let name = Structure.tag pkg_struct |> Group.Pkg_tag.name in
      let tag =
        let path = Structure.base_dir pkg_struct in
        Group.Mod_tag.create ~pkg_tag:(Structure.tag pkg_struct) ~path
      in
      Sema.Env.create name ~parent:None ~visibility ~ty_sc
        ~kind:(Sema.Env.M tag) ~lookup_space:Sema.Env.LkGlobal
    in

    let root_mod =
      let path = Structure.base_dir pkg_struct in
      let tag =
        Group.Mod_tag.create ~pkg_tag:(Structure.tag pkg_struct) ~path
      in
      Mod.create ~tag ~menv
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
      let tag =
        let root_mod_tag = Mod.tag root_mod in
        Group.Mod_tag.with_path root_mod_tag ~path
      in
      Sema.Env.create name ~parent:(Some root_mod_env) ~visibility ~ty_sc
        ~kind:(Sema.Env.M tag) ~lookup_space:Sema.Env.LkGlobal
    in

    let tag =
      let root_mod_tag = Mod.tag root_mod in
      Group.Mod_tag.with_path root_mod_tag ~path
    in
    Mod.create ~tag ~menv

  let insert_new_mod ~parent_mod ~child_mod =
    Sema.Env.insert_meta (Mod.menv parent_mod) (Mod.menv child_mod)
    |> Sema.Phase1_collect_toplevels.assume_new

  let insert_source_mod ~root_mod mh =
    (* TODO: open modules by root module *)
    insert_new_mod ~parent_mod:root_mod ~child_mod:(mh |> Mod_handle.inner)

  let insert_dep_meta ~root_mod ph =
    let dep_root_mod = Package_handle.root_mod ph in
    insert_new_mod ~parent_mod:root_mod ~child_mod:dep_root_mod
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

let assume_no_errors ~printer mhs =
  let open Result.Let_syntax in
  let has_errors = ref false in

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

let assume_no_errors_in_pkg ~printer ~pkg_handle =
  let has_errors = ref false in

  let mhs = Package_handle.mod_handles pkg_handle in
  assume_no_errors ~printer mhs

let load_pkg_symbols ~ws ~pkg_handle =
  let builtin = Workspace.builtin ws in

  let pkg_struct = Package_handle.structure pkg_handle in
  let subst = Package_handle.subst pkg_handle in

  (* Create a module for package root *)
  let root_mod = Sema_helper.create_root_mod ~pkg_struct in
  Package_handle.set_root_mod pkg_handle ~root_mod;

  let has_fatal = ref false in

  (* Add a recursive refecence
   * e.g.
   * root_mod:foo <-|
   *   - mod:foo  --|
   *)
  Sema_helper.insert_new_mod ~parent_mod:root_mod ~child_mod:root_mod;

  (* Add dependencies *)
  let deps = Structure.dependencies pkg_struct in
  List.iter deps ~f:(fun dep ->
      [%loga.debug "dep_pkg -> %s" (Group.Pkg_tag.name dep)];
      let dep_ph =
        match Workspace.find_pkg_handle ws ~tag:dep with
        | Some v -> v
        | None -> failwith "[ICE] dep is not loaded"
      in
      (* e.g.
       * root_mod:foo
       *   - mod:core (root_mod of core)
       *   - mod:std (root_mod of std)
       *)
      Sema_helper.insert_dep_meta ~root_mod dep_ph);

  (* e.g. pkg:foo, files:a.rill, b.rill *)
  let paths = Structure.src_paths pkg_struct in
  List.iter paths ~f:(fun path ->
      (* Create a module for source *)
      let m = Sema_helper.load_mod ~root_mod ~path in

      let mh = Mod_handle.create ~m in
      load_module_symbols mh ~builtin ~subst;
      Package_handle.add_mod_handle pkg_handle ~mh;

      (* add a rerefence to the root.
       * e.g.
       * root_mod:foo
       *   - mod:a
       *   - mod:b
       *)
      Sema_helper.insert_source_mod ~root_mod mh;
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

let pack_llvm_modules assets =
  let open Result.Let_syntax in
  let%bind llvm_modules =
    List.fold_result assets ~init:[] ~f:(fun mods asset ->
        let Asset.{ art; _ } = asset in
        match art with
        | Emitter.Artifact.Llvm_ir { m = llvm } -> Ok (llvm :: mods)
        | _ ->
            Error
              (Errors.Failed_to_export_artifact
                 "Cannot pack modules which are not LLVM format"))
  in
  let llvm = Llvm_gen.merge_modules llvm_modules in
  let asset =
    Asset.{ path = ""; art = Emitter.Artifact.Llvm_ir { m = llvm } }
  in
  Ok [ asset ]

let to_natives assets ~triple ~emitter =
  let open Result.Let_syntax in
  let%bind backend =
    let (module Triple : Common.Triple.PRESET) = triple in
    let llvm_triple = Triple.name in
    Llvm_gen.Backend.create ~triple:llvm_triple
    |> Result.map_error ~f:(fun _e ->
           Errors.Failed_to_export_artifact "Could not create LLVM backend")
  in
  let assets =
    List.map assets ~f:(fun asset ->
        match (asset, emitter) with
        | (Asset.{ art = Emitter.Artifact.Llvm_ir { m = llvm }; _ }, Emitter.Asm)
        | (Asset.{ art = Emitter.Artifact.Llvm_ir { m = llvm }; _ }, Emitter.Obj)
          ->
            let art = Emitter.Artifact.Native { backend; m = llvm } in
            { asset with art }
        | _ -> asset)
  in
  Ok assets

let generate_assets ~ws ~pkg_handle ~printer ~emitter ~pack ~triple =
  let to_assets mhs =
    let open Result.Let_syntax in
    let%bind () = assume_no_errors ~printer mhs in

    List.fold_result mhs ~init:[] ~f:(fun assets mh ->
        let%bind asset =
          match Mod_handle.phase_result mh with
          | Ok (Mod_handle.Artifact art) ->
              Ok Asset.{ art; path = Mod_handle.path mh }
          | _ -> Error (Errors.Failed_to_export_artifact "[ICE] not artifacts")
        in
        Ok (asset :: assets))
  in

  let builtin = Workspace.builtin ws in
  let subst = Package_handle.subst pkg_handle in

  (* TODO: now modules can be built in parallel *)
  let mhs = Package_handle.mod_handles pkg_handle in

  let open Base.With_return in
  with_return (fun r ->
      let open Result.Let_syntax in
      (* to rill-ir *)
      let mhs =
        List.map mhs ~f:(fun mh ->
            let mh = Mod_handle.clone mh in
            Pipelines.Generator.To_phase3.apply mh;
            Pipelines.Generator.To_rill_ir.to_artifact mh ~builtin ~pkg_handle;
            mh)
      in
      let () =
        match emitter with
        | Emitter.Rill_ir -> r.return (mhs |> to_assets)
        | _ -> ()
      in

      (* to llvm-ir *)
      let mhs =
        List.map mhs ~f:(fun mh ->
            Pipelines.Generator.To_llvm_ir.to_artifact mh ~builtin ~pkg_handle;
            mh)
      in
      let () =
        match emitter with
        | Emitter.Llvm_ir | Emitter.Llvm_bc -> r.return (to_assets mhs)
        | _ -> ()
      in

      let%bind assets = to_assets mhs in

      let%bind assets = if pack then pack_llvm_modules assets else Ok assets in

      (* to native *)
      let%bind assets = to_natives assets ~triple ~emitter in

      Ok assets)

let load_pkg_as_header ~ws ~printer ~pkg_handle =
  let open Result.Let_syntax in
  load_pkg_symbols ~ws ~pkg_handle;
  let%bind () = assume_no_errors_in_pkg ~printer ~pkg_handle in

  declare_package_symbols ~ws ~pkg_handle;
  let%bind () = assume_no_errors_in_pkg ~printer ~pkg_handle in

  decide_exported_interfaces ~ws ~pkg_handle;

  Ok ()

let compile ~ws ~printer ~pkg_handle ~pack ~triple ~emitter ~out_to =
  (*
   *  src  ->  rir  ->         llvm_ir        ->  asm/obj  -> lib/bin
   * [src] -> [rir] ->        [llvm_ir]       -> [asm/obj] -> lib/bin
   * [src] -> [rir] -> ([llvm_ir] -> llvm_ir) ->  asm/obj  -> lib/bin
   *)
  let open Result.Let_syntax in
  load_pkg_symbols ~ws ~pkg_handle;
  let%bind () = assume_no_errors_in_pkg ~printer ~pkg_handle in

  declare_package_symbols ~ws ~pkg_handle;
  let%bind () = assume_no_errors_in_pkg ~printer ~pkg_handle in

  decide_exported_interfaces ~ws ~pkg_handle;

  analyze_package ~ws ~pkg_handle;
  let%bind () = assume_no_errors_in_pkg ~printer ~pkg_handle in

  (* [rir]/[llvm_ir]/[asm]/[obj] *)
  let%bind assets =
    generate_assets ~ws ~pkg_handle ~printer ~emitter ~pack ~triple
  in
  let%bind paths = Writer.write_assets ~assets ~emitter ~out_to in

  (* lib/bin *)
  Ok paths

let link_bin ~spec ~lib_dirs ~pkgs ~objs ~out_to =
  let open Result.Let_syntax in
  let out =
    match out_to with
    | Writer.OutputToFile out_file -> out_file |> Option.value ~default:"a.out"
    | _ -> failwith "[ICE]"
  in

  let lib_names =
    List.map pkgs ~f:(fun pkg ->
        let pkg_struct = Package_handle.structure pkg in
        Structure.lib_names pkg_struct)
    |> List.join
  in

  let%bind () = Common.Os.cc_exe ~spec ~lib_dirs ~lib_names ~objs ~out () in
  Ok ()
