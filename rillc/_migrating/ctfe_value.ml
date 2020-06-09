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
  | Uint32 of Stdint.uint32
  | Int64 of int64
  | Uint64 of Stdint.uint64
  | Bool of bool
  | Undef of Unification.id_t
