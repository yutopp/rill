(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

module Flags = struct
  open Cmdliner

  let section =
    [
      `S Manpage.s_common_options;
      `P "These options are common to all commands.";
      `S "MORE HELP";
      `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command.";
      `S Manpage.s_bugs;
      `P "Check bug reports at https://github.com/yutopp/rill/issues.";
    ]
end
[@@warning "-44"]

include Flags
