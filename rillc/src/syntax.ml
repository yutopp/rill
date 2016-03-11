(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)


let make_lexedbuf_from_input input =
  input |> BatIO.to_input_channel |> Lexing.from_channel

let make_ast full_filepath pkg_names mod_name lexedbuf =
  let module P = Parser.Make(struct
                              let full_filepath = full_filepath
                              let package_names = pkg_names
                              let module_name = mod_name
                            end) in
  try
    P.program_entry Lexer.token lexedbuf
  with
  | Lexer.UnexpectedToken c ->
     let open Lexing in
     let pos = lexeme_start_p lexedbuf in
     let line_num = pos.pos_lnum in
     let column_pos = pos.pos_cnum - pos.pos_bol in
     Printf.eprintf "unexpected character(Line: %d, Pos: %d) \"%c\"\n" line_num column_pos c;
     exit (-2)

  | P.Error ->
     let open Lexing in
     let start_pos = lexeme_start_p lexedbuf in
     let bpos = start_pos.pos_cnum - start_pos.pos_bol in
     Printf.eprintf "At offset %d / line: %d -- syntax error.\n%!" bpos (start_pos.pos_lnum);
     exit (-2)

let make_ast_from_file ~default_pkg_names ~default_mod_name full_filepath =
  Batteries.File.with_file_in full_filepath
                              (fun input -> input
                                            |> make_lexedbuf_from_input
                                            |> make_ast full_filepath default_pkg_names default_mod_name)
