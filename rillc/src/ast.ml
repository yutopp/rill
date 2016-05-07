(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

module Context =
  struct
    type 'a current_ctx_t = unit
    type 'a term_ctx_t = Nodes.Loc.t
    type 'a prev_ctx_t = unit
  end

include Nodes.Make(Context)
