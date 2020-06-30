(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

type t = {
  bool_ : Typing.Type.t;
  i8_ : Typing.Type.t;
  i32_ : Typing.Type.t;
  i64_ : Typing.Type.t;
  u64_ : Typing.Type.t;
  usize_ : Typing.Type.t;
  isize_ : Typing.Type.t;
  string_ : Typing.Type.t;
  unit_ : Typing.Type.t;
  array_ : Typing.Type.t -> int -> Typing.Type.t;
  pointer_ : Typing.Type.mutability_t -> Typing.Type.t -> Typing.Type.t;
  (* meta *)
  type_ : Typing.Type.t -> Typing.Type.t;
}

let create () : t =
  let binding_mut = Typing.Type.MutMut in
  let bool_ =
    Typing.Type.
      { ty = Num { bits = 1; signed = false }; binding_mut; span = Span.undef }
  in
  let i8_ =
    Typing.Type.
      { ty = Num { bits = 8; signed = true }; binding_mut; span = Span.undef }
  in
  let i32_ =
    Typing.Type.
      { ty = Num { bits = 32; signed = true }; binding_mut; span = Span.undef }
  in
  let i64_ =
    Typing.Type.
      { ty = Num { bits = 64; signed = true }; binding_mut; span = Span.undef }
  in
  let u64_ =
    Typing.Type.
      { ty = Num { bits = 64; signed = false }; binding_mut; span = Span.undef }
  in
  let usize_ =
    Typing.Type.{ ty = Size { signed = false }; binding_mut; span = Span.undef }
  in
  let isize_ =
    Typing.Type.{ ty = Size { signed = true }; binding_mut; span = Span.undef }
  in
  let string_ = Typing.Type.{ ty = String; binding_mut; span = Span.undef } in
  let unit_ = Typing.Type.{ ty = Unit; binding_mut; span = Span.undef } in
  let array_ elem n =
    Typing.Type.{ ty = Array { elem; n }; binding_mut; span = Span.undef }
  in
  let pointer_ mut elem =
    Typing.Type.{ ty = Pointer { mut; elem }; binding_mut; span = Span.undef }
  in
  let type_ ty = Typing.Type.{ ty = Type ty; binding_mut; span = Span.undef } in
  {
    bool_;
    i8_;
    i32_;
    i64_;
    u64_;
    usize_;
    isize_;
    string_;
    unit_;
    array_;
    pointer_;
    type_;
  }
