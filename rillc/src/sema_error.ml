(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Sema_definitions
open Sema_context

let process_error err ctx =
  Printf.printf "\n===============================\n";
  Error_msg_printer.print err;
  store_error_message "" ctx;
  Printf.printf "\n===============================\n";
  ()
