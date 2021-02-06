(*
 * Copyright yutopp 2019 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module Span = Common.Span

class defined_multiple_times ~(previous : Span.t) ~(name : string) =
  object (self)
    inherit Diagnostics.Error_info.base

    method to_string _ctx =
      Printf.sprintf "The symbol `%s` is defined multiple times" name
  end

class cannot_assign =
  object (self)
    inherit Diagnostics.Error_info.base

    method to_string _ctx = Printf.sprintf "Cannot assign to immutable variable"
  end

class cannot_reference_mut =
  object (self)
    inherit Diagnostics.Error_info.base

    method to_string _ctx =
      Printf.sprintf "Cannot reference immutable value as mutable"
  end

class not_struct_type =
  object (self)
    inherit Diagnostics.Error_info.base

    method to_string _ctx = Printf.sprintf "Not struct type"
  end

class type_mismatch ~(detail : Typer_err.t) =
  object (self)
    inherit Diagnostics.Error_info.base

    method to_string ctx =
      let to_string_ty ~subst lhs rhs =
        Printf.sprintf "Type mismatch: Expect = %s, but Actual = %s"
          (Typing.Type.to_string lhs)
          (Typing.Type.to_string rhs)
      in

      let to_string_mut ~subst lhs rhs =
        Printf.sprintf "Type mutability mismatch: Expect = %s, but Actual = %s"
          (Typing.Type.to_string_mut lhs)
          (Typing.Type.to_string_mut rhs)
      in

      let to_string_diff ~subst diff =
        match diff with
        | Typer_err.Type { lhs; rhs } -> to_string_ty ~subst lhs rhs
        | Typer_err.Mutability { lhs; rhs } -> to_string_mut ~subst lhs rhs
        | Typer_err.Linkage { lhs; rhs } -> ""
      in

      let to_string_kind ~subst kind =
        match kind with
        | Typer_err.ErrFuncArgLength { r; l } -> "Arg length"
        | Typer_err.ErrFuncArgs index -> Printf.sprintf "Arg at (%d)" index
        | Typer_err.ErrFuncLinkage -> "Linkage"
        | Typer_err.ErrFuncArgRet -> "Return"
        | Typer_err.ErrUnify -> "Diff"
        | Typer_err.ErrArrayElem -> "Elem"
        | Typer_err.ErrPointerElem -> "PointerElem"
        | Typer_err.ErrArrayLength { r; l } -> "Array length"
        | Typer_err.ErrTypeElem -> "TypeElem"
        | Typer_err.ErrNumBits { r; l } -> "NumBits"
        | Typer_err.ErrNumSigned { r; l } -> "NumSigned"
      in

      let rec to_string_ds ~subst d msgs_acc =
        let Typer_err.{ diff; kind; nest } = d in
        let desc = to_string_kind ~subst kind in
        let msg = to_string_diff ~subst diff in
        let msgs_acc = [ desc; msg ] :: msgs_acc in
        match nest with
        | None -> msgs_acc
        | Some inner -> to_string_ds ~subst inner msgs_acc
      in

      let subst = () in
      let msgs = to_string_ds ~subst detail [] |> List.concat in
      String.concat ~sep:"\n" msgs
  end
