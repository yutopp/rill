(*
 * Copyright yutopp 2018 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module Span = Common.Span

type t = { reason : reason_t; span : Span.t }

and reason_t = Error of Error_info.base | Warning of Error_info.base

let error ~span e = { reason = Error e; span }

let to_string_human ~ctx elem =
  let buf = Buffer.create 256 in

  let { reason; span; _ } = elem in
  let span_s = Span.to_string span in
  let (level, inner) =
    match reason with Error e -> ("ERROR", e) | Warning w -> ("WARNING", w)
  in
  let message = inner#to_string ctx in
  Buffer.add_string buf (Printf.sprintf "%s: %s\n" level message);
  Buffer.add_string buf (Printf.sprintf "  --> %s\n" span_s);

  Buffer.contents buf

let print_for_human ~ctx ch elem =
  Stdio.Out_channel.output_string ch (to_string_human ~ctx elem)
