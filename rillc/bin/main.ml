(*
 * Copyright yutopp 2018 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

let () =
  let opts = Flags.parse () in
  let opts = match Flags.validate opts with
    | Ok opts ->
       opts
    | Error _ ->
       Stdio.eprintf "ERR: args\n"; (* TODO: fix *)
       Caml.exit 1
  in

  let filename = Flags.input_files opts |> List.hd |> Option.value ~default:"DUMMY" in
  Stdio.eprintf "filename: %s\n" filename;

  let ast = match Rillc.Parser.parse_from_file filename with
    | Ok ast ->
       ast
    | Error msg ->
       Stdio.eprintf "ERR: parsing: %s\n" msg; (* TODO: fix *)
       Caml.exit 1
  in
  Stdio.eprintf "AST = \n%s\n" (Rillc.Ast.sexp_of_t ast |> Sexplib.Sexp.to_string_hum ~indent:2);

  let _ = match Rillc.Sema.sem ast with
    | Ok (ast', _) ->
       let _ = Stdio.printf "SEMA = \n%s\n" (Rillc.Hir.sexp_of_t ast' |> Sexp.to_string_hum ~indent:2) in
       let rir' = Rillc.Rir.generate ast' in
       let _ = Stdio.printf "RIR = \n%s\n" (Rillc.Rir.sexp_of_t rir' |> Sexp.to_string_hum ~indent:2) in
       ()
    | Error errs ->
       Stdio.eprintf "Errors - (%d)\n" (List.length errs);
       List.iter ~f:(fun e -> Stdio.eprintf "-> %s\n" (Rillc.Diagnostics.to_string e)) errs
  in

  Rillc.test ()
