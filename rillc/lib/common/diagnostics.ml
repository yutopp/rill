(*
 * Copyright yutopp 2018 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

module Unit = struct
  class virtual base =
    object (self)
      method virtual to_string : string
    end
end

module Error = struct
  class virtual base =
    object (self)
      inherit Unit.base
    end
end

module Elem = struct
  type t = { reason : reason_t; span : Span.t }

  and reason_t = Error of Error.base | Warning of Error.base

  let error ~span e = { reason = Error e; span }

  let to_string_human elem =
    let { reason; span; _ } = elem in
    let span_s = Span.to_string span in
    let (level, inner) =
      match reason with Error e -> ("ERROR", e) | Warning w -> ("WARNING", w)
    in
    let message = inner#to_string in
    Printf.sprintf "%s: %s\n%s\n" level message span_s

  let print_for_human ch elem =
    Stdio.Out_channel.output_string ch (to_string_human elem)
end

type t = { mutable elems_rev : Elem.t list }

let create () : t = { elems_rev = [] }

let append ds elem = ds.elems_rev <- elem :: ds.elems_rev

let append_all ds ds' = ds.elems_rev <- List.append ds'.elems_rev ds.elems_rev

let iter ds ~f = List.rev ds.elems_rev |> List.iter ~f

let errors ds = List.rev ds.elems_rev

let warnings ds = []
