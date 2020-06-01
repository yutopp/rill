(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

exception Invalid_argument of string

exception There_are_warnings_or_errors

module Flags = struct
  let into_result e =
    match e with
    | Invalid_argument msg ->
        `Error (false, Printf.sprintf "Invalid_argument: %s" msg)
    | There_are_warnings_or_errors ->
        `Error (false, "There are warnings or errors.")
    | _ -> raise e
end
[@@warning "-44"]

include Flags
