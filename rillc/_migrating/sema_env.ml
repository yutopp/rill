(*
 * Copyright yutopp 2017 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries
open Sema_error

let error_if_env_is_dupped loc result =
  match result with
  | Ok () ->
     ()
  | Bad (`Duplicated found_env) ->
     error (Error_msg.MultiSymbol (found_env, loc))
