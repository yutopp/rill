(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

let print_line ~ctx ch d =
  Diagnostics.Elem.print_for_human ~ctx ch d;
  Stdio.Out_channel.fprintf ch "\n"

let print ~ctx ch ds = Diagnostics.iter ~f:(print_line ~ctx ch) ds

let print_with_last_error ~ctx ch (failed, ds) =
  let Mod_state.{ last_error; _ } = failed in
  print_line ~ctx ch last_error;
  print ~ctx ch ds

let flush ch = Stdio.Out_channel.flush ch
