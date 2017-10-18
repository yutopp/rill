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

type file_type =
  | FtAssembly
  | FtObject
  | FtRaw

(* at cpp_ext *)
external rillc_initialize_llvm_codegen : unit -> unit
  = "rillc_cg_initialize_llvm_codegen"
external rillc_emit_file_for_target : L.llmodule -> file_type -> string -> unit
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

let () =
  initialize_codegen ()

(* TODO: support many options *)
let emit_file filepath m object_format =
  (*let _ =
    LT.Target.all ()
    |> List.map LT.Target.name
    |> List.map (Debug.printf "-> %s")
  in*)
  let triple = L.target_triple m in

  let target =
    try LT.Target.by_triple triple with
    | Llvm_target.Error error ->
       failwith error
  in

  (* TODO: set triple when instantiate a module *)
  let target_machine =
    Llvm_target.TargetMachine.create ~triple:triple
                                     ~cpu:""
                                     ~features:""
                                     ~level:LT.CodeGenOptLevel.Default
                                     ~reloc_mode:LT.RelocMode.PIC
                                     ~code_model:LT.CodeModel.Default
                                     target
  in

  (* data layout *)
  let dl = LT.TargetMachine.data_layout target_machine in
  L.set_data_layout (LT.DataLayout.as_string dl) m;

  Debug.printf "module: TRIPLE: %s / DATA_LAYOUT: %s\n%!"
               (L.target_triple m)
               (L.data_layout m);

  let () =
    match Llvm_analysis.verify_module m with
    | Some msg -> Debug.printf "error(verify_module): %s\n%!" msg
    | None     -> ()
  in

  let file_type =
    match object_format with
    | Codegen_format.OfAssembly -> FtAssembly
    | Codegen_format.OfObject   -> FtObject
    | Codegen_format.OfLlvm     -> FtRaw
    | _ -> failwith "[ERR] unsupported object_format"
  in
  rillc_emit_file_for_target m file_type filepath
