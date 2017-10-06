(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries

module Timer : sig
  type t
  val create: unit -> t
  val elapsed: t -> t
  val string_of_elapsed: t -> string
end =
  struct
    type t = float

    let create () =
      Sys.time()

    (* seconds *)
    let elapsed t =
      Sys.time() -. t

    let string_of_elapsed t =
      let e = elapsed t in
      Printf.sprintf "%fs" e;
  end

let () =
  let ignore_debug_log =
    try Sys.getenv "RILL_IGNORE_DEBUG_LOG" <> "" with
    | Not_found -> false
  in
  if Config.is_release || ignore_debug_log then
    Loga.set_base_severity Loga_severity.Emergency
  else
    Loga.set_base_severity Loga_severity.Debug

let out_ch =
  let ignore_debug_log =
    try Sys.getenv "RILL_IGNORE_DEBUG_LOG" <> "" with
    | Not_found -> false
  in
  if Config.is_release || ignore_debug_log then
    stdnull
  else
    stderr

let printf fmt =
  Printf.fprintf out_ch (fmt ^^ "\n%!")

let verbose = ref true

let set_verbose flag =
  verbose := flag

let reportf fmt =
  let ch = if (!verbose) then stdout else stdnull in
  Printf.fprintf ch (fmt ^^ "\n%!")

let record_backtrace () =
  Printexc.record_backtrace true
