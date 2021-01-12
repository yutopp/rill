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
  phase_result : (phase_t, failed_t) Result.t;
  format : Emitter.t option;
}

and phase_t =
  | NotParsed
  | Parsed of Syntax.Ast.t
  | Phase1CollectTopLevels of Sema.Phase1.TopAst.t
  | Phase1DeclareTopLevels of Sema.Phase1.TopAst.t
  | Phase1DeclareImpls of Sema.Phase1.TopAst.t
  | Phase2 of Sema.Phase2.TAst.t
  | Artifact of Emitter.Artifact.t

and failed_t = { previous_phase : phase_t; last_error : Diagnostics.Elem.t }

let create ~m = { m; phase_result = Ok NotParsed; format = None }

(* if true, cannot forward steps completely *)
let has_fatal m = Result.is_error m.phase_result

let has_errors m = Mod.has_errors m.m || has_fatal m
