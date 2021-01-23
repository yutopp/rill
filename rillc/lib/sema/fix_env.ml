(*
 * Copyright yutopp 2021 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

let rec fix env ~subst =
  let Env.{ ty_sc; kind; _ } = env in
  [%loga.debug "fix before = %s" (Typing.Scheme.to_string ty_sc)];
  let ty_sc = Typing.Subst.subst_scheme subst ty_sc in
  [%loga.debug "fix after = %s" (Typing.Scheme.to_string ty_sc)];

  (* TODO: check there are no type vars (not forall) *)
  Env.update_ty_sc env ~ty_sc;

  (* TODO: check visibility and  lookup_space *)
  match kind with
  | Env.M _ ->
      let children = Env.collect_all env in
      List.iter children ~f:(fix ~subst)
  | _ -> ()
