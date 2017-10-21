(*
 * Copyright yutopp 2016 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

type info_t = {
  pos_fname           : string;
  pos_begin_cnum      : int;
  pos_begin_lnum      : int;
  pos_begin_bol       : int;
  pos_end_cnum        : int;
  pos_end_lnum        : int;
  pos_end_bol         : int;
}

type t = info_t option

let dummy = None

let to_string opt_loc =
  match opt_loc with
  | Some loc ->
     let bline = loc.pos_begin_lnum in
     let bcol = loc.pos_begin_bol in
     let eline = loc.pos_end_lnum in
     let ecol = loc.pos_end_bol in
     let pos_s =
       if bline = eline then
         Printf.sprintf "Line %d, charactor %d-%d (%d)"
                        bline bcol ecol loc.pos_begin_cnum
       else
         Printf.sprintf "Line %d, charactor %d to Line %d, %d"
                        bline bcol eline ecol
     in
     Printf.sprintf "%s in %s" pos_s loc.pos_fname
  | None -> "Unknown location"
