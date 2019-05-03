(*
 * Copyright yutopp 2019 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

module Diagnostics = Common.Diagnostics

class invalid_token ~(token : string) =
  object (self)
    inherit Diagnostics.reason
    method to_string =
      Printf.sprintf "Invalid token: \"%s\"" token
  end

class unexpected_token ~(ch : char) =
  object (self)
    inherit Diagnostics.reason
    method to_string =
      Printf.sprintf "Unexpected charactor: \"%c\"" ch
  end

class invalid_syntax =
  object (self)
    inherit Diagnostics.reason
    method to_string =
      Printf.sprintf "Syntax error"
  end
