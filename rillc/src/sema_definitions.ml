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
    | MemberNotFound of type_info_t * Nodes.Loc.t
    | Msg of string

  let rec print ?(loc=None) err =
    match err with
    | DifferentArgNum (params_num, args_num) ->
       Printf.printf "%s:\nError: requires %d but given %d\n"
                     (Nodes.Loc.to_string loc) params_num args_num

    | ConvErr m ->
       begin
         Printf.printf "%s:\nError:\n" (Nodes.Loc.to_string loc);
         let p k (msg, arg_loc) =
           (* TODO: prinf arg_loc *)
           Printf.printf "%s: %dth arg %s\n" (Nodes.Loc.to_string arg_loc) k msg
         in
         PosMap.iter p m
       end

    | NoMatch (errs, loc) ->
       Printf.printf "%s:\nError: nomatch\n" (Nodes.Loc.to_string loc);
       List.iter (fun err -> print ~loc:loc err) errs

    | MemberNotFound (in_ty, loc) ->
       let s_ty = Type.to_string in_ty in
       Printf.printf "%s:\nError: member not found in %s\n"
                     (Nodes.Loc.to_string loc)
                     s_ty

    | Msg msg ->
       Printf.printf "\n------------------\nError:\n %s\n\n-------------------\n" msg

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
