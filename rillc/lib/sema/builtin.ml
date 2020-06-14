(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

type t = {
  bool_ : Typing.Type.t;
  i32_ : Typing.Type.t;
  string_ : Typing.Type.t;
  unit_ : Typing.Type.t;
  array_ : Typing.Type.t -> int -> Typing.Type.t;
  pointer_ : Typing.Type.mutability_t -> Typing.Type.t -> Typing.Type.t;
  (* meta *)
  type_ : Typing.Type.t -> Typing.Type.t;
}

let create () : t =
  let binding_mut = Typing.Type.MutMut in
  let bool_ = Typing.Type.{ ty = Bool; binding_mut; span = Span.undef } in
  let i32_ = Typing.Type.{ ty = Int; binding_mut; span = Span.undef } in
  let string_ = Typing.Type.{ ty = String; binding_mut; span = Span.undef } in
  let unit_ = Typing.Type.{ ty = Unit; binding_mut; span = Span.undef } in
  let array_ elem n =
    Typing.Type.{ ty = Array { elem; n }; binding_mut; span = Span.undef }
  in
  let pointer_ mut elem =
    Typing.Type.{ ty = Pointer { mut; elem }; binding_mut; span = Span.undef }
  in
  let type_ ty = Typing.Type.{ ty = Type ty; binding_mut; span = Span.undef } in
  { bool_; i32_; string_; unit_; array_; pointer_; type_ }
