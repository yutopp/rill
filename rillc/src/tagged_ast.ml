(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

module AstContext =
  struct
    type 'a current_ctx_t = ('a Env.env_t) option
    type 'a term_ctx_t = 'a Env.env_t Type.info_t
    type 'a prev_ctx_t = Ast.ast
  end

module TaggedAst = Nodes.Make(AstContext)

include TaggedAst
