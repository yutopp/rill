(*
 * Copyright yutopp 2017 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries
module L = Llvm
module LT = Llvm_target

external rillc_initialize_llvm_codegen : unit -> unit
  = "rillc_cg_initialize_llvm_codegen"
external rillc_emit_file_for_target : L.llmodule -> string -> unit
  = "rillc_cg_emit_file_for_target_machine"

let initialize_codegen =
  let is_initialized = ref false in
  let initialize () =
    match !is_initialized with
    | false ->
       rillc_initialize_llvm_codegen ();
       is_initialized := true
    | true ->
       ()
  in
  initialize

(* TODO: support many options *)
let emit_file filepath m =
  (* TODO: set this on elsewhere *)
  let triple = LT.Target.default_triple () in
  L.set_target_triple triple m;

  (* data layout *)
  (* let dl = LT.TargetMachine.data_layout target_machine in
  L.set_data_layout (LT.DataLayout.as_string dl) m;*)

  Debug.printf "module: TRIPLE: %s / DATA_LAYOUT: %s\n%!"
                (triple)
                (L.data_layout m);

  let () =
    match Llvm_analysis.verify_module m with
    | Some msg -> Debug.printf "error(verify_module): %s\n%!" msg
    | None     -> ()
  in

  initialize_codegen ();
  rillc_emit_file_for_target m filepath
