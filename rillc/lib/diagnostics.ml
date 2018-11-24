(*
 * Copyright yutopp 2018 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

type t = {
  reason: reason_t;
  span: Span.t;
}
and reason_t =
  | Id_not_found of string

let create ~reason ~span =
  {
    reason = reason;
    span = span;
  }

let to_string err =
  match err.reason with
  | Id_not_found id ->
     Printf.sprintf "%s: Not found: id = %s" (Span.to_string err.span) id
