(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

module TAst = Tagged_ast

type env_t = TAst.t Env.env_t
type type_info_t = env_t Type.info_t
type ctfe_val_t = type_info_t Ctfe_value.t
type error_msg_t = (type_info_t, env_t) Error_msg.t

type ctx_t = {
  sc_root_env               : env_t;
  mutable sc_builtin_m_env  : env_t option;

  sc_module_bag         : env_t Module_info.Bag.t;
  sc_module_search_dirs : string list;

  (* ctfe engine *)
  sc_ctfe_engine        : Ctfe_engine.t;

  (* type sets *)
  sc_tsets              : env_t Type_sets.type_sets_t;

  (* for template *)
  sc_unification_ctx    : (type_info_t, ctfe_val_t) Unification.t;

  (* errors *)
  sc_handle_error   : bool;
  mutable sc_errors : string list;
}

(* TODO: currently, this implementation doesn't deep copy *)
let make_temporary_context ctx =
  {
    ctx with
    sc_handle_error = false;
  }

let store_error_message msg ctx =
  ctx.sc_errors <- msg :: ctx.sc_errors
