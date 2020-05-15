(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module Span = Common.Span
module Diagnostics = Common.Diagnostics
module Ast = Syntax.Ast

let guard_dup_value ~span penv_opt name =
  match Option.bind penv_opt ~f:(fun penv -> Env.find_value penv name) with
  | Some other ->
      (* TODO: fix a span to point correct location(use 'other') *)
      let e = new Reasons.defined_twice ~other:span ~name in
      let elm = Diagnostics.Elem.error ~span e in
      Error elm
  | None -> Ok ()
