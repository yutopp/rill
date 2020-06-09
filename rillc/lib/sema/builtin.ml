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
}

let create () : t =
  let binding_mut = Typing.Type.MutImm in
  let bool_ = Typing.Type.{ ty = Bool; binding_mut; span = Span.undef } in
  let i32_ = Typing.Type.{ ty = Int; binding_mut; span = Span.undef } in
  let string_ = Typing.Type.{ ty = String; binding_mut; span = Span.undef } in
  let unit_ = Typing.Type.{ ty = Unit; binding_mut; span = Span.undef } in
  { bool_; i32_; string_; unit_ }
