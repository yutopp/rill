(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

module Diagnostics = Common.Diagnostics

let print_line ch d =
  Diagnostics.Elem.print_for_human ch d;
  Stdio.Out_channel.fprintf ch "\n"

let print ch ds = Diagnostics.iter ~f:(print_line ch) ds

let print_with_last_error ch (failed, ds) =
  let Mod_state.{ last_error; _ } = failed in
  print_line ch last_error;
  print ch ds

let flush ch = Stdio.Out_channel.flush ch
