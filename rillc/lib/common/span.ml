(*
 * Copyright yutopp 2018 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

module Pos = struct
  type t = {
    lnum: int;
    cnum: int;
    bcnum: int;
  }
  [@@deriving sexp]

  let from_lexpos p =
    {
      lnum = p.Lexing.pos_lnum;
      cnum = p.Lexing.pos_cnum;
      bcnum = p.Lexing.pos_cnum - p.Lexing.pos_bol;
    }
end

type t = {
  path: string; (* TODO: fix *)
  loc_opt: (Pos.t * Pos.t) option;
}
[@@deriving sexp]

let create ~path ~loc_opt : t =
  {
    path;
    loc_opt;
  }

let create_path ~path : t =
  create ~path ~loc_opt:None

let create_from_lex_loc ~path ~lex_loc =
  let (s, e) = lex_loc in
  let loc = (s |> Pos.from_lexpos, e |> Pos.from_lexpos) in
  create ~path ~loc_opt:(Some loc)

let to_string (span : t) =
  let {path; loc_opt;} = span in

  let message = "" in
  let message = message ^ (Printf.sprintf "File: \"%s\"" path) in

  let message = match loc_opt with
    | Some (s, e) ->
       let range_s =
         if s.Pos.lnum = e.Pos.lnum then
           Printf.sprintf "line %d, charactors %d-%d"
                          s.Pos.lnum s.Pos.bcnum e.Pos.bcnum
         else
           Printf.sprintf "line %d-%d, charactors %d-%d"
                          s.Pos.lnum e.Pos.lnum s.Pos.bcnum e.Pos.bcnum
       in
       message ^ (Printf.sprintf ", %s" range_s)

    | None ->
       message
  in
  message
