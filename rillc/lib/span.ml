(*
 * Copyright yutopp 2018 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

type t =
  | Raw of pos_t * pos_t
  | Dummy
[@@deriving sexp]

and pos_t = {
  lnum: int;
  cnum: int;
  bcnum: int;
}
[@@deriving sexp]

let to_pos p =
  {
    lnum = p.Lexing.pos_lnum;
    cnum = p.Lexing.pos_cnum;
    bcnum = p.Lexing.pos_cnum - p.Lexing.pos_bol
  }

let create ~b ~e =
  Raw (b |> to_pos, e |> to_pos)

let to_string span =
  match span with
  | Raw (b, e) ->
     if b.lnum = e.lnum then
       Printf.sprintf "Line %d, charactor %d-%d"
                      b.lnum b.bcnum e.bcnum
     else
       Printf.sprintf "Line %d, charactor %d to Line %d, charactor %d"
                      b.lnum b.bcnum e.lnum e.bcnum
  | Dummy ->
     "[?]"
