(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

type t = Rill_ir | Llvm_ir | Llvm_bc | Asm | Obj | Wasm

let ext_of emitter =
  match emitter with
  | Rill_ir -> "rir"
  | Llvm_ir -> "ll"
  | Llvm_bc -> "bc"
  | Asm -> "s"
  | Obj -> "o"
  | Wasm -> "wasm"

let emit_map =
  [
    ("rill-ir", Rill_ir);
    ("llvm-ir", Llvm_ir);
    ("llvm-bc", Llvm_bc);
    ("asm", Asm);
    ("obj", Obj);
    ("wasm", Wasm);
  ]

let default_emitter_of triple =
  match triple with
  | Triple.{ arch = Arch_wasm32; sys = Some Sys_wasi; _ } -> Wasm
  | Triple.{ arch = Arch_x86_64; sys = Some Sys_linux; _ } -> Llvm_bc (*Obj*)
  | _ -> failwith "[ICE] unsupported triple"

module Artifact = struct
  type t =
    | Rill_ir of { m : Rir.Module.t }
    | Llvm_ir of { m : Llvm_gen.Module.t }
end
