(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

let to_string v =
  match v with
  | Ctfe_value.Type ty -> Type.to_string ty
  | Ctfe_value.Bool b -> Printf.sprintf "%b" b
  | Ctfe_value.Int32 n -> Int32.to_string n
  | Ctfe_value.Uint32 n -> Stdint.Uint32.to_string n
  | Ctfe_value.Int64 n -> Int64.to_string n
  | Ctfe_value.Uint64 n -> Stdint.Uint64.to_string n
  | Ctfe_value.Undef _ -> "%%ctfe_val(undef)%%"

let debug_print_ctfe_value value =
  Debug.printf "%s" (to_string value)
