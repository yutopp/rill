(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module Workspace = Common.Workspace
module Package = Common.Package
module Triple = Common.Triple
module Target_spec = Common.Target_spec
module Os = Common.Os

module Export = struct
  type t =
    | Artifact of {
        emitter : Compiler.Emitter.t option;
        pack : bool;
        out_to : Compiler.Writer.output_t;
      }
    | Executable of { out_path : string option }
    | Library of { out_path : string option }
end

type t = {
  sysroot : string option;
  corelib_srcdir : string option;
  corelib_libdir : string option;
  stdlib_srcdir : string option;
  stdlib_libdir : string option;
  target : Triple.tag_t option;
  export : Export.t;
  input_files : string list;
}

let validate opts =
  if List.length opts.input_files < 1 then
    raise (Errors.Invalid_argument "no input file")

let builtin_pkg_info ~sysroot ~srcdir ~lib_names ~name =
  let tag = Compiler.Package_tag.create ~name ~version:"builtin" in

  let pkg_srcdir =
    match srcdir with
    | Some dir -> dir
    | None -> Os.join_path [ sysroot; "lib"; "rill-lib"; "src"; name; "src" ]
  in

  let pkg_struct = Compiler.Structure.create ~tag ~base_dir:pkg_srcdir () in

  let paths =
    Os.grob_dir pkg_srcdir "^.*\\.rill$"
    |> List.map ~f:(fun n -> Stdlib.Filename.concat pkg_srcdir n)
  in
  Compiler.Structure.add_src_paths pkg_struct ~paths;

  Compiler.Structure.add_lib_names pkg_struct ~names:lib_names;

  pkg_struct

let entry opts =
  let open Result.Let_syntax in
  let%bind () = Result.try_with (fun () -> validate opts) in

  let sysroot = Args.sysroot opts.sysroot in

  let host_triple = Args.host_triple () in
  let target_triple = Args.target_triple opts.target in

  let target_sysroot = Args.target_sysroot ~sysroot ~triple:target_triple in

  let%bind target_spec = Args.target_spec ~target_sysroot in

  let ws = Compiler.Workspace.create () in

  (* core *)
  let core_lib =
    let pkg_struct =
      builtin_pkg_info ~sysroot ~srcdir:opts.corelib_srcdir
        ~lib_names:[ "core-c" ] ~name:"core"
    in
    Compiler.Workspace.new_pkg ws ~pkg_struct
  in

  (* std *)
  let std_lib =
    let pkg_struct =
      let s =
        builtin_pkg_info ~sysroot ~srcdir:opts.stdlib_srcdir
          ~lib_names:[ "std-c" ] ~name:"std"
      in
      Compiler.Structure.add_dependency s
        ~tag:(Compiler.Package_handle.tag core_lib);
      s
    in
    Compiler.Workspace.new_pkg ws ~pkg_struct
  in

  (* target *)
  let pkg =
    let pkg_struct =
      let tag = Compiler.Package_tag.create ~name:"main" ~version:"latest" in

      let s = Compiler.Structure.create ~tag ~base_dir:"." () in
      Compiler.Structure.add_src_paths s ~paths:opts.input_files;

      Compiler.Structure.add_dependency s
        ~tag:(Compiler.Package_handle.tag core_lib);

      Compiler.Structure.add_dependency s
        ~tag:(Compiler.Package_handle.tag std_lib);
      s
    in
    Compiler.Workspace.new_pkg ws ~pkg_struct
  in

  let printer = Stdio.stderr in

  (* TODO: calc dependency order *)
  let%bind () =
    Compiler.Handler.load_pkg_as_header ~ws ~printer ~pkg_handle:core_lib
  in

  let%bind () =
    Compiler.Handler.load_pkg_as_header ~ws ~printer ~pkg_handle:std_lib
  in

  let (module Target : Triple.PRESET) = target_triple in
  let%bind () =
    match opts.export with
    | Export.Artifact { emitter; pack; out_to } ->
        let emitter =
          Option.value emitter
            ~default:(Compiler.Emitter.default_emitter_of Target.triple)
        in
        let%bind _ =
          Compiler.Handler.compile ~ws ~printer ~pkg_handle:pkg ~pack
            ~triple:target_triple ~emitter ~out_to
        in
        Ok ()
    | Export.Executable { out_path } ->
        let%bind tmp_dir = Os.mktemp_dir "rillc.XXXXXXXX" in
        let emitter = Compiler.Emitter.default_emitter_of Target.triple in
        let pack = false in
        let out_to = Compiler.Writer.OutputToDir tmp_dir in
        let%bind filenames =
          Compiler.Handler.compile ~ws ~printer ~pkg_handle:pkg ~pack
            ~triple:target_triple ~emitter ~out_to
        in
        [%loga.debug "compiled = %s" (String.concat ~sep:"; " filenames)];

        (* adhoc-impl *)
        let lib_dirs = [ Os.join_path [ target_sysroot; "lib" ] ] in
        let pkgs = [ core_lib; std_lib ] in

        let out = out_path |> Option.value ~default:"a.out" in
        let%bind () =
          Compiler.Handler.link_bin ~spec:target_spec ~lib_dirs ~pkgs
            ~objs:filenames ~out
        in
        Ok ()
    | _ -> failwith "[ICE]"
  in

  Ok ()
