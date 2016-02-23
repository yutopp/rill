(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

type ref_val_t =
    Ref
  | Val
  | XRef
  | RefValUndef

type mut_t =
    Immutable
  | Const
  | Mutable
  | MutUndef

type lifetime_t =
    Static
  | ThreadLocal
  | Scoped
  | LifetimeUndef

type attr_t = {
  ta_ref_val    : ref_val_t;
  ta_mut        : mut_t;
}


let undef = {
  ta_ref_val = RefValUndef;
  ta_mut = MutUndef;
}
