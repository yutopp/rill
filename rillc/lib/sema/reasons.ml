(*
 * Copyright yutopp 2019 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

module Diagnostics = Common.Diagnostics

class type_mismatch (a : Type.t) (b :Type.t) =
object (self)
  inherit Diagnostics.reason
  method to_string =
    Printf.sprintf
      "Type mismatch: %s = %s"
      (Type.sexp_of_t a |> Sexp.to_string_hum)
      (Type.sexp_of_t b |> Sexp.to_string_hum)
end
