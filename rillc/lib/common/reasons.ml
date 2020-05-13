(*
 * Copyright yutopp 2019 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

class compilation_stopped =
  object (self)
    inherit Diagnostics.Error.base

    method to_string = Printf.sprintf "There some errors"
  end

class id_not_found ~(name : string) =
  object (self)
    inherit Diagnostics.Error.base

    method to_string = Printf.sprintf "Not found: id = %s" name
  end

class internal_exception ~(e : exn) =
  object (self)
    inherit Diagnostics.Error.base

    method to_string =
      Printf.sprintf "ICE(InternalException)\n%s" (Exn.to_string e)
  end

class internal_error ~(message : string) =
  object (self)
    inherit Diagnostics.Error.base

    method to_string = Printf.sprintf "[ICE] Message: %s" message
  end
