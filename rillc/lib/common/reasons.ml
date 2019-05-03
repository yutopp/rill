(*
 * Copyright yutopp 2019 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

class id_not_found ~(name : string) =
object (self)
  inherit Diagnostics.reason
  method to_string =
    Printf.sprintf "Not found: id = %s" name
end

class internal_unsupported_node ~(name : string) =
object (self)
  inherit Diagnostics.reason
  method to_string =
    Printf.sprintf "ICE(UnsupportedNodeError)\n%s" name
end

class internal_exception ~(e : exn) =
object (self)
  inherit Diagnostics.reason
  method to_string =
    Printf.sprintf "ICE(InternalException)\n%s" (Exn.to_string e)
end

class multiple ~(rs : Diagnostics.reason list) =
object (self)
  inherit Diagnostics.reason
  method to_string =
    List.map rs ~f:(fun r -> r#to_string) |> String.concat ~sep:""
end
