(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module Span = Common.Span
module Ast = Syntax.Ast

let guard_dup ~span ~ns penv name =
  match Env.find ~ns penv name with
  | Some other ->
      (* TODO: fix a span to point correct location(use 'other') *)
      let e = new Reasons.defined_multiple_times ~previous:span ~name in
      let elm = Diagnostics.Elem.error ~span e in
      Error elm
  | None -> Ok ()

let guard_dup_value ~span penv name =
  guard_dup ~span ~ns:Env.NamespaceValue penv name

let guard_dup_type ~span penv name =
  guard_dup ~span ~ns:Env.NamespaceType penv name
