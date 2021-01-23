(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module Package = Common.Package
module Triple = Common.Triple
module Mod = Sema.Mod

type t = {
  m : Mod.t;
  mutable phase_result : (phase_t, failure_t) Result.t;
  format : Emitter.t option;
}

and phase_t =
  | NotParsed
  | Parsed of Syntax.Ast.t
  | Phase1CollectTopLevels of Sema.Phase1_collect_toplevels.TopAst.t
  | Phase1DeclareTopLevels of Sema.Phase1_collect_toplevels.TopAst.t
  | Phase1DeclareImpls of Sema.Phase1_collect_toplevels.TopAst.t
  | Phase2 of Sema.Phase2.TAst.t
  | Phase3 of Sema.Phase3.NAst.t
  | Artifact of Emitter.Artifact.t

and failure_t = { previous_phase : phase_t; last_error : Diagnostics.Elem.t }

let create ~m = { m; phase_result = Ok NotParsed; format = None }

let clone mh = { m = mh.m; phase_result = mh.phase_result; format = mh.format }

let inner mh = mh.m

let phase_result mh = mh.phase_result

let update_phase_result mh ~phase_result = mh.phase_result <- phase_result

let path mh = inner mh |> Mod.path

(* if true, cannot forward steps completely *)
let has_fatal mh = Result.is_error mh.phase_result

let has_errors mh = Mod.has_errors mh.m || has_fatal mh
