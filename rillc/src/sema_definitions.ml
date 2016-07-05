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

let dtor_name = "dtor"
let dtor_id_name = Nodes.Pure dtor_name

let assign_name = Nodes.BinaryOp "="

(* default qual *)
let default_ty_attr = {
  Type_attr.ta_ref_val = Type_attr.Val;
  Type_attr.ta_mut = Type_attr.Const;
}

exception Instantiation_failed
exception Template_type_mismatch

exception Fatal_error of string
let fatal_error msg =
  raise (Fatal_error msg)

let pos_of_earg earg =
  let (_, aux) = earg in
  let (_, _, _, _, pos) = aux in
  pos

let type_of_earg earg =
  let (_, aux) = earg in
  let (ty, _, _, _, pos) = aux in
  ty

module Error = struct
  module ArgPosMap = Map.Make(Int)

  type t =
    (* num of params * num of args *)
    | DifferentArgNum of int * int
    (* target_type * source_arg * ErrorLevel *)
    | ConvErr of (type_info_t * earg_t * FuncMatchLevel.t) ArgPosMap.t * env_t
    | NoMatch of t list * Nodes.Loc.t
    | MemberNotFound of type_info_t * env_t list * Nodes.Loc.t
    | Msg of string

  let print_env_trace env =
    let env_loc = env.Env.loc in
    match Env.get_env_record env with
    | Env.Module (lt, r) ->
       Printf.printf "  module   : %s (%s)\n"
                     r.Env.mod_name
                     (Nodes.Loc.to_string env_loc)
    | Env.Function (lt, r) ->
       let name = Nodes.string_of_id_string r.Env.fn_name in
       Printf.printf "  function : %s (%s)\n"
                     name
                     (Nodes.Loc.to_string env_loc)
    | Env.Class (lt, r) ->
       let name = Nodes.string_of_id_string r.Env.cls_name in
       Printf.printf "  class    : %s (%s)\n"
                     name
                     (Nodes.Loc.to_string env_loc)
    | Env.Scope _ ->
       begin
         Printf.printf "Scope\n"
       end
    | Env.Root _ -> ()
    | Env.MetaVariable _ -> ()
    | Env.MultiSet _ -> ()
    | _ -> failwith "[ICE] unknown env"

  let string_of_loc_region loc =
    match loc with
    | Some l ->
       Bytes.sub_string l.Nodes.Loc.source_code
                        l.Nodes.Loc.pos_begin_cnum
                        (l.Nodes.Loc.pos_end_cnum - l.Nodes.Loc.pos_begin_cnum)
    | None -> failwith ""

  let rec print ?(loc=None) err =
    match err with
    | DifferentArgNum (params_num, args_num) ->
       Printf.printf "%s:\nError: requires %d but given %d\n"
                     (Nodes.Loc.to_string loc) params_num args_num

    | ConvErr (m, f_env) ->
       let p k (trg_ty, src_arg, level) =
         let src_loc = pos_of_earg src_arg in
         let src_ty = type_of_earg src_arg in

         let msg = match level with
           | FuncMatchLevel.NoMatch ->
              Printf.sprintf "type '%s' of expr '%s' is not suitable for '%s'"
                             (Type.to_string src_ty)
                             (string_of_loc_region src_loc)
                             (Type.to_string trg_ty)
           | _ -> failwith ""
         in
         Printf.printf "  %dth arg: %s\n" k msg
       in
       ArgPosMap.iter p m

    | NoMatch (errs, loc) ->
       Printf.printf "%s:\nError: There is no matched function\n"
                     (Nodes.Loc.to_string loc);
       List.iter (fun err -> print err) errs

    | MemberNotFound (in_ty, history, loc) ->
       let s_ty = Type.to_string in_ty in

       Printf.printf "%s:\nError: member \"%s\" is not found in %s\n"
                     (Nodes.Loc.to_string loc)
                     (string_of_loc_region loc)
                     s_ty;
       Printf.printf "Searched scopes are...\n";
       List.iter (fun env -> print_env_trace env) history

    | Msg msg ->
       Printf.printf "\n------------------\nError:\n %s\n\n-------------------\n" msg


end

exception NError of Error.t
let error err =
  raise (NError err)

let error_msg msg =
  raise (NError (Error.Msg msg))
