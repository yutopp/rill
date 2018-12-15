(*
 * Copyright yutopp 2018 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

let rec transform k_form : (Rir_term.t, Diagnostics.t) Result.t =
  match k_form with
  | Rir_k_norm.{kind = Module {nodes}; ty; span} ->
     let ctx = () in
     let ctx' = List.fold_left nodes ~init:ctx ~f:transform' in
     let m = Rir_term.{
       funcs = []
     } in
     Ok m

  | Rir_k_norm.{span; _} ->
     let detail = k_form |> Rir_k_norm.sexp_of_t |> Sexp.to_string_hum ~indent:2 in
     let reason = Diagnostics.InternalUnsupportedNode detail in
     Error (Diagnostics.create ~reason ~span ~phase:Diagnostics.PhaseRirTrans)

and transform' ctx k_form =
  ctx
