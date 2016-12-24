(*
 * Copyright yutopp 2016 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

type t =
  {
    fi_has_terminator: bool;
  }

let empty =
  {
    fi_has_terminator = false;
  }

let set_has_terminator b fi =
  { (*fi with*) fi_has_terminator = b }

let has_terminator fi =
  fi.fi_has_terminator
