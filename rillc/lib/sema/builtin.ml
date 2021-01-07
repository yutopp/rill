(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

module Span = Common.Span

type t = {
  bool_ : span:Span.t -> Typing.Type.t;
  i8_ : span:Span.t -> Typing.Type.t;
  i32_ : span:Span.t -> Typing.Type.t;
  i64_ : span:Span.t -> Typing.Type.t;
  u64_ : span:Span.t -> Typing.Type.t;
  usize_ : span:Span.t -> Typing.Type.t;
  isize_ : span:Span.t -> Typing.Type.t;
  string_ : span:Span.t -> Typing.Type.t;
  unit_ : span:Span.t -> Typing.Type.t;
  array_ : span:Span.t -> Typing.Type.t -> int -> Typing.Type.t;
  pointer_ :
    span:Span.t -> Typing.Type.mutability_t -> Typing.Type.t -> Typing.Type.t;
  (* meta *)
  type_ : span:Span.t -> Typing.Type.t -> Typing.Type.t;
}

let create () : t =
  let open Typing.Type in
  let binding_mut = MutMut in
  let bool_ ~span =
    { ty = Num { bits = 1; signed = false }; binding_mut; span }
  in
  let i8_ ~span = { ty = Num { bits = 8; signed = true }; binding_mut; span } in
  let i32_ ~span =
    { ty = Num { bits = 32; signed = true }; binding_mut; span }
  in
  let i64_ ~span =
    { ty = Num { bits = 64; signed = true }; binding_mut; span }
  in
  let u64_ ~span =
    { ty = Num { bits = 64; signed = false }; binding_mut; span }
  in
  let usize_ ~span = { ty = Size { signed = false }; binding_mut; span } in
  let isize_ ~span = { ty = Size { signed = true }; binding_mut; span } in
  let string_ ~span = { ty = String; binding_mut; span } in
  let unit_ ~span = { ty = Unit; binding_mut; span } in
  let array_ ~span elem n = { ty = Array { elem; n }; binding_mut; span } in
  let pointer_ ~span mut elem =
    { ty = Pointer { mut; elem }; binding_mut; span }
  in
  let type_ ~span ty = { ty = Type ty; binding_mut; span } in
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
