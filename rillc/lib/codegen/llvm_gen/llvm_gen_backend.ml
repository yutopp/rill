(*
 * Copyright yutopp 2019 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module Span = Common.Span
module Diagnostics = Common.Diagnostics
module L = Llvm
module L_all_backends = Llvm_all_backends
module L_targets = Llvm_target

let initialize_if_not_initialized =
  let initialized = ref false in
  let f () =
    if not !initialized then (
      L_all_backends.initialize ();
      initialized := true )
  in
  f

type t = { mc : L_targets.TargetMachine.t }

let create ~triple =
  let open Result.Let_syntax in
  initialize_if_not_initialized ();

  let%bind target =
    try Ok (L_targets.Target.by_triple triple)
    with L_targets.Error message -> Error (L_targets.Error message)
  in
  let mc =
    let cpu = "" in
    let features = "" in
    let level = L_targets.CodeGenOptLevel.Default in
    let reloc_mode = L_targets.RelocMode.PIC in
    let code_model = L_targets.CodeModel.Default in
    L_targets.TargetMachine.create ~triple ~cpu ~features ~level ~reloc_mode
      ~code_model target
  in

  Ok { mc }

let write_to ~ch ~asm backend m =
  let { mc; _ } = backend in

  let data_layout = L_targets.TargetMachine.data_layout mc in
  L.set_data_layout (L_targets.DataLayout.as_string data_layout) m;

  let ft =
    match asm with
    | true -> L_targets.CodeGenFileType.AssemblyFile
    | false -> L_targets.CodeGenFileType.ObjectFile
  in
  let ll_membuf = L_targets.TargetMachine.emit_to_memory_buffer m ft mc in

  (* TODO: fix memory efficiency *)
  let s = L.MemoryBuffer.as_string ll_membuf in
  Stdio.Out_channel.output_string ch s;
  L.MemoryBuffer.dispose ll_membuf;

  Ok ()
