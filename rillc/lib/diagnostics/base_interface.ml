(*
 * Copyright yutopp 2018 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

module Ctx = struct
  type t = unit
end

class virtual base =
  object (self)
    method virtual to_string : Ctx.t -> string
  end
