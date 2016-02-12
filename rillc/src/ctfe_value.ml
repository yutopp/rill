(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

type 'ty t =
  | Type of 'ty
  | Int32 of int32
  | Undef of Unification.id_t * ('ty, 'ty t) Unification.t
