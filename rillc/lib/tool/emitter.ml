(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

type t = Rill_ir | Llvm_ir | Llvm_bc | Asm | Obj

let ext_of emitter =
  match emitter with
  | Rill_ir -> "rir"
  | Llvm_ir -> "ll"
  | Llvm_bc -> "bc"
  | Asm -> "s"
  | Obj -> "o"

let emit_map =
  [
    ("rill-ir", Rill_ir);
    ("llvm-ir", Llvm_ir);
    ("llvm-bc", Llvm_bc);
    ("asm", Asm);
    ("obj", Obj);
  ]

let default_emitter_of triple =
  match triple with
  | Triple.{ arch = Arch_wasm32; sys = Some Sys_wasi; _ } -> Obj
  | Triple.{ arch = Arch_x86_64; sys = Some Sys_linux; _ } -> Obj
  | _ -> failwith "[ICE] unsupported triple"

module Artifact = struct
  type t =
    | Rill_ir of { m : Rir.Module.t }
    | Llvm_ir of { m : Llvm_gen.Module.t }
    | Native of { native : Llvm_gen.Backend.t }

  let tag_string_of art =
    match art with
    | Rill_ir _ -> "rill-ir"
    | Llvm_ir _ -> "llvm-ir"
    | Native _ -> "native"
end
