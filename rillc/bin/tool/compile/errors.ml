(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

module Flags = struct
  type errot_t = Tool_error of exn | No_input_filenames

  let into_result err =
    match err with
    | Tool_error e -> (
        let open Rillc.Tool.Errors in
        match e with
        | Invalid_argument msg ->
            `Error (false, Printf.sprintf "Invalid_argument: %s" msg)
        | There_are_warnings_or_errors ->
            `Error (false, "There are warnings or errors.")
        | _ -> raise e )
    | No_input_filenames -> `Error (false, "No input filenames.")
end
[@@warning "-44"]
