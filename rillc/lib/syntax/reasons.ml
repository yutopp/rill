(*
 * Copyright yutopp 2019 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

class invalid_token ~(token : string) =
  object (self)
    inherit Diagnostics.Error_info.base

    method to_string _ctx = Printf.sprintf "Invalid token: \"%s\"" token
  end

class unexpected_token ~(ch : char) =
  object (self)
    inherit Diagnostics.Error_info.base

    method to_string _ctx = Printf.sprintf "Unexpected charactor: \"%c\"" ch
  end

class invalid_syntax ~(msg : string) =
  object (self)
    inherit Diagnostics.Error_info.base

    method to_string _ctx = Printf.sprintf "Syntax error: %s" msg
  end
