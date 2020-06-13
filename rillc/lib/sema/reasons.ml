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

class defined_twice ~(other : Span.t) ~(name : string) =
  object (self)
    inherit Diagnostics.Error.base

    method to_string =
      Printf.sprintf
        "Definition which has same symbol(%s) is defined twice.\n %s\n" name
        (Span.sexp_of_t other |> Sexp.to_string_hum)
  end

class cannot_assign =
  object (self)
    inherit Diagnostics.Error.base

    method to_string = Printf.sprintf "Cannot assign to immutable variable"
  end

class cannot_reference_mut =
  object (self)
    inherit Diagnostics.Error.base

    method to_string =
      Printf.sprintf "Cannot reference immutable value as mutable"
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

      let to_string_mut lhs rhs =
        Printf.sprintf "Type mutability mismatch: Expect = %s, but Actual = %s"
          (Typing.Type.to_string_mut lhs)
          (Typing.Type.to_string_mut rhs)
      in

      let to_string_diff diff =
        match diff with
        | Typer_err.Type { lhs; rhs } -> to_string_ty lhs rhs
        | Typer_err.Mutability { lhs; rhs } -> to_string_mut lhs rhs
        | Typer_err.Linkage { lhs; rhs } -> ""
      in

      let to_string_kind kind =
        match kind with
        | Typer_err.ErrFuncArgLength { r; l } -> "Arg length"
        | Typer_err.ErrFuncArgs index -> Printf.sprintf "Arg at (%d)" index
        | Typer_err.ErrFuncLinkage -> "Linkage"
        | Typer_err.ErrFuncArgRet -> "Return"
        | Typer_err.ErrUnify -> "Diff"
        | Typer_err.ErrArrayElem -> "Elem"
        | Typer_err.ErrPointerElem -> "PointerElem"
        | Typer_err.ErrArrayLength { r; l } -> "Array length"
      in

      let rec to_string_ds d msgs_acc =
        let Typer_err.{ diff; kind; nest } = d in
        let desc = to_string_kind kind in
        let msg = to_string_diff diff in
        let msgs_acc = [ desc; msg ] :: msgs_acc in
        match nest with
        | None -> msgs_acc
        | Some inner -> to_string_ds inner msgs_acc
      in

      let msgs = to_string_ds detail [] |> List.rev |> List.concat in
      String.concat ~sep:"\n" msgs
  end
