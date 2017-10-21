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
    type 'a prev_ctx_t = Ast.t
    type 'a term_aux_t = ('a term_ctx_t) Aux.t
    type 'a attr_value_t = Ast.t
  end

module TaggedAst = Nodes.Make(AstContext)

include TaggedAst

let extract_prev_pass_node node =
  match node with
  | PrevPassNode n -> n
  | _ -> failwith "[ICE] not prev node"
