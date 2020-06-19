(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

(* <arch><sub>-<vendor>-<sys>-<abi> *)
type arch_t = Arch_x86_64 | Arch_wasm32

type sub_t = unit

type vendor_t = Vendor_unknown

type sys_t = Sys_linux | Sys_wasi

type abi_t = ABI_gnu

type t = {
  arch : arch_t;
  sub : sub_t option;
  vendor : vendor_t option;
  sys : sys_t option;
  abi : abi_t option;
}

module type TARGET_PRESET = sig
  val name : string

  val triple : t
end

module X86_64_unknown_linux_gnu : TARGET_PRESET = struct
  let name = "x86_64-unknown-linux-gnu"

  let triple =
    {
      arch = Arch_x86_64;
      sub = None;
      vendor = Some Vendor_unknown;
      sys = Some Sys_linux;
      abi = Some ABI_gnu;
    }
end

module Wasm32_wasi : TARGET_PRESET = struct
  let name = "wasm32-wasi"

  let triple =
    {
      arch = Arch_wasm32;
      sub = None;
      vendor = None;
      sys = Some Sys_wasi;
      abi = None;
    }
end

type targets_t = Target_X86_64_unknown_linux_gnu | Target_Wasm32_wasi

let to_triple target : (module TARGET_PRESET) =
  match target with
  | Target_X86_64_unknown_linux_gnu -> (module X86_64_unknown_linux_gnu)
  | Target_Wasm32_wasi -> (module X86_64_unknown_linux_gnu)

let targets_map =
  [ X86_64_unknown_linux_gnu.(name, triple); Wasm32_wasi.(name, triple) ]
