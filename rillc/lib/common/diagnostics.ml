(*
 * Copyright yutopp 2018 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

class virtual reason =
  object (self)
    method virtual to_string : string
  end

type t = {
  reason: reason;
  span: Span.t;
  phase: phase_t option;
}

and phase_t =
  | PhaseParsing
  | PhaseSema
  | PhaseRirTrans

let create ?phase ~reason ~span =
  {
    reason = reason;
    span;
    phase;
  }

let rec to_string d =
  let {reason; span; _} = d in
  let span_s = (Span.to_string span) in
  let reason_s = reason#to_string in
  Printf.sprintf "%s: %s" span_s reason_s

module Multi = struct
  type nonrec t = t list

  let create () =
    []

  let append m d =
    d :: m

  let to_list m =
    m

  let is_failed m =
    not ((List.length m) = 0)
end
