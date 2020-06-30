(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module Span = Common.Span

class no_target ~(message : string) ~(descriptions : string list) =
  object (self)
    inherit Diagnostics.Error_info.base

    method to_string _ctx =
      let descs = String.concat ~sep:"\n" descriptions in
      Printf.sprintf "LLVM: There are no target: %s.\nCandidates=\n%s" message
        descs
  end
