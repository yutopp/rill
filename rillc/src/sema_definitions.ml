(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries
open Sema_context



type type_gen_t = env_t Type.Generator.t

type conv_filter_t = (type_info_t * env_t) option
type earg_t = TAst.ast * TAst.term_aux_t


exception Instantiation_failed
exception Template_type_mismatch


exception Fatal_error of string
let fatal_error msg =
  raise (Fatal_error msg)

module Error = struct
  module PosMap = Map.Make(Int)

  type t =
    | DifferentArgNum of int * int (* num of params * num of args *)
    | ConvErr of (string * Nodes.Loc.t) PosMap.t
    | NoMatch of t list * Nodes.Loc.t
    | MemberNotFound of Nodes.Loc.t
    | Msg of string
end

exception NError of Error.t
let error err =
  raise (NError err)

let error_msg msg =
  raise (NError (Error.Msg msg))


module FuncMatchLevel =
  struct
    type t =
      | ExactMatch
      | QualConv
      | ImplicitConv
      | NoMatch

    let to_int = function
      | ExactMatch      -> 0
      | QualConv        -> 1
      | ImplicitConv    -> 2
      | NoMatch         -> 3

    let of_int = function
      | 0 -> ExactMatch
      | 1 -> QualConv
      | 2 -> ImplicitConv
      | 3 -> NoMatch
      | _ -> failwith "invalid"

    let bottom a b =
      of_int (max (to_int a) (to_int b))

    (* ascending order, ExactMatch -> ... -> NoMatch *)
    let compare a b =
      compare (to_int a) (to_int b)

    (* if 'a' is matched than 'b', returns true *)
    let is_better a b =
      (to_int a) < (to_int b)

    let is_same a b =
      (to_int a) = (to_int b)

    let to_string = function
      | ExactMatch      -> "ExactMatch"
      | QualConv        -> "QualConv"
      | ImplicitConv    -> "ImplicitConv"
      | NoMatch         -> "NoMatch"
  end


let ctor_name = "ctor"
let ctor_id_name = Nodes.Pure ctor_name

let assign_name = Nodes.BinaryOp "="

(* default qual *)
let default_ty_attr = {
  Type_attr.ta_ref_val = Type_attr.Val;
  Type_attr.ta_mut = Type_attr.Const;
}
