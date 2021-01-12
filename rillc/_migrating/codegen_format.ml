(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

type t =
  | OfObject
  | OfAssembly
  | OfLlvm
  | OfExecutable

let of_string s =
  match s with
  | "obj" -> OfObject
  | "asm" -> OfAssembly
  | "llvm" -> OfLlvm
  | _ -> failwith (Printf.sprintf "[ERR] %s is not supported" s)
