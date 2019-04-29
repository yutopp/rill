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
  phase: phase_t option;
}

and reason_t =
  | InvalidToken of string
  | UnexpectedToken of char
  | InvalidSyntax
  | Id_not_found of string
  | InternalUnsupportedNode of string
  | InternalException of exn
  | Multiple of t list

and phase_t =
  | PhaseParsing
  | PhaseSema
  | PhaseRirTrans
[@@deriving sexp_of]

let create ?phase ~reason ~span =
  {
    reason;
    span;
    phase;
  }

let rec to_string d =
  match d with
  | {reason = InvalidToken detail; span; _} ->
     Printf.sprintf "%s: Invalid token: \"%s\"" (Span.to_string span) detail
  | {reason = UnexpectedToken c; span; _} ->
     Printf.sprintf "%s: Unexpected charactor: \"%c\"" (Span.to_string span) c
  |  {reason = InvalidSyntax; span; _} ->
     Printf.sprintf "%s: Syntax error:" (Span.to_string span)
  |  {reason = Id_not_found id; span; _} ->
     Printf.sprintf "%s: Not found: id = %s" (Span.to_string span) id
  |  {reason = InternalUnsupportedNode detail; span; _} ->
     Printf.sprintf "%s: ICE(UnsupportedNodeError)\n%s" (Span.to_string span) detail
  |  {reason = InternalException e; span; _} ->
     Printf.sprintf "%s: ICE(InternalException)\n%s" (Span.to_string span) (Exn.to_string e)
  |  {reason = Multiple ds; span; _} ->
      List.map ds ~f:to_string |> String.concat ~sep:""

module Multi = struct
  type nonrec t = t list

  let create () =
    []

  let append m d =
    d :: m

  let to_list m =
    m
end
