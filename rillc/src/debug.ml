(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries

let out_ch =
  if Config.is_release then
    open_out "/dev/null"
  else
    stderr

let printf fmt =
  let o = Printf.fprintf out_ch fmt in
  if Config.is_release then () else flush out_ch;
  o

let record_backtrace () =
  Printexc.record_backtrace true
