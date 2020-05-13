(*
 * Copyright yutopp 2019 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module Diagnostics = Common.Diagnostics
module Span = Common.Span

class defined_twice ~(other : Span.t) =
  object (self)
    inherit Diagnostics.Error.base

    method to_string =
      Printf.sprintf "Definition which has same symbol is defined twice.\n %s\n"
        (Span.sexp_of_t other |> Sexp.to_string_hum)
  end

class type_mismatch ~(detail : Typer_err.t) =
  object (self)
    inherit Diagnostics.Error.base

    method to_string =
      let to_string_ty lhs rhs =
        Printf.sprintf "Type mismatch: Expect = %s, but Actual = %s"
          (Typing.Type.to_string lhs)
          (Typing.Type.to_string rhs)
      in
      let to_string_d lhs rhs d =
        let anot =
          match d with
          | Typer_err.ErrFuncArgLength { r; l } -> "Arg length"
          | Typer_err.ErrFuncArgs index -> Printf.sprintf "Arg at (%d)" index
          | Typer_err.ErrFuncArgRet -> "Return"
          | Typer_err.ErrUnify -> "Diff"
        in
        let ty_s = to_string_ty lhs rhs in
        [ anot; ty_s ]
      in
      let rec to_string_ds d msgs_acc =
        let Typer_err.{ lhs; rhs; kind; nest } = d in
        let msgs = to_string_d lhs rhs kind in
        let msgs_acc = msgs :: msgs_acc in
        match nest with
        | None -> msgs_acc
        | Some inner -> to_string_ds inner msgs_acc
      in
      let msgs = to_string_ds detail [] |> List.rev |> List.concat in
      String.concat ~sep:"\n" msgs
  end
