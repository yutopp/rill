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
    | Artifact of { emit : Compiler.Emitter.t option; pack : bool }
    | Executable
    | Library
end

type t = {
  sysroot : string option;
  corelib_srcdir : string option;
  corelib_libdir : string option;
  stdlib_srcdir : string option;
  stdlib_libdir : string option;
  target : Triple.tag_t option;
  export : Export.t;
  out_to : Compiler.Writer.output_t;
  input_files : string list;
}

let validate opts =
  if List.length opts.input_files < 1 then
    raise (Errors.Invalid_argument "no input file")

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
    Builtin.new_core_lib ~ws ~sysroot ~srcdir:opts.corelib_srcdir
  in

  (* std *)
  let std_lib =
    Builtin.new_std_lib ~ws ~sysroot ~srcdir:opts.stdlib_srcdir ~core_lib
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
    | Export.Artifact { emit = emitter; pack } ->
        let emitter =
          Option.value emitter
            ~default:(Compiler.Emitter.default_emitter_of Target.triple)
        in
        let%bind _ =
          Compiler.Handler.compile ~ws ~printer ~pkg_handle:pkg ~pack
            ~triple:target_triple ~emitter ~out_to:opts.out_to
        in
        Ok ()
    | Export.Executable ->
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

        let%bind () =
          Compiler.Handler.link_bin ~spec:target_spec ~lib_dirs ~pkgs
            ~objs:filenames ~out_to:opts.out_to
        in
        Ok ()
    | _ -> failwith "[ICE]"
  in

  Ok ()
