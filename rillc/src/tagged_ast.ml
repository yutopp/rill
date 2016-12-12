(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

module Aux =
  struct
    type 'ast t = {
      ta_type           : 'ast Env.env_t Type.info_t;
      ta_vcat           : Value_category.t;
      ta_lt             : Lifetime.t;
      ta_ml             : Meta_level.t;
      ta_loc            : Loc.t;
    }

    let ty aux =
      aux.ta_type

    let loc aux =
      aux.ta_loc

    let ml aux =
      aux.ta_ml

    let lt aux =
      aux.ta_lt

    let make ~ty ~vcat ~lt ~ml ~loc =
      {
        ta_type = ty;
        ta_vcat = vcat;
        ta_lt = lt;
        ta_ml = ml;
        ta_loc = loc;
      }
  end

module AstContext =
  struct
    type 'a current_ctx_t = ('a Env.env_t) option
    type 'a term_ctx_t = 'a Env.env_t Type.info_t
    type 'a prev_ctx_t = Ast.ast
    type 'ast term_aux_t = 'ast Aux.t
    type 'a attr_value_t = Ast.ast
  end

module TaggedAst = Nodes.Make(AstContext)

include TaggedAst

let extract_prev_pass_node node =
  match node with
  | PrevPassNode n -> n
  | _ -> failwith "[ICE] not prev node"
