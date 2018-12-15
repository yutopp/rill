(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

let parse_from_file path : (Ast.t, Diagnostics.t) Result.t =
  let f chan =
    let lexbuf = chan |> Lexing.from_channel in
    try
      let ast = Syntax.program_entry Lexer.token lexbuf in
      Ok ast
    with
    | Lexer.LexerError detail ->
       (* TODO: use typed reason instead of detail *)
       let reason = Diagnostics.InvalidToken detail in
       let span = Span.from_lexbuf lexbuf in
       Error (Diagnostics.create ~reason ~span ~phase:Diagnostics.PhaseParsing)

    | Lexer.UnexpectedToken tok ->
       let reason = Diagnostics.UnexpectedToken tok in
       let span = Span.from_lexbuf lexbuf in
       Error (Diagnostics.create ~reason ~span ~phase:Diagnostics.PhaseParsing)

    | Syntax.Error ->
       let reason = Diagnostics.InvalidSyntax in
       let span = Span.from_lexbuf lexbuf in
       Error (Diagnostics.create ~reason ~span ~phase:Diagnostics.PhaseParsing)
  in
  try Stdio.In_channel.with_file ~binary:true path ~f:f with
  | e ->
     let reason = Diagnostics.InternalException e in
     Error (Diagnostics.create ~reason ~span:(Span.Dummy) ~phase:Diagnostics.PhaseParsing)
