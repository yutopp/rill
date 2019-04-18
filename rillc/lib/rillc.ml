(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

(* exports *)
module Syntax = Syntax

module Span = Common.Span
module Diagnostics = Common.Diagnostics

module Sema = Sema
module Hir = Hir
module Rir = Rir
module Codegen_llvm = Codegen_llvm

let test () =
  Printf.printf "Rillc\n"
