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

type conv_filter_t =
  | Trans of type_info_t
  | ConvFunc of type_info_t * env_t * Lifetime.t list

(*module Eargs : sig
  type t


end = struct
  type t = TAst.ast * TAst.term_aux_t * unit list

  let rec split' lists (acca,accb,accc) =
    match lists with
    | [] -> (acca,accb,accc)
    | (a, b, c)::rest -> split' rest (a::acca, b::accb, c::accc)

  let split3 lists =
    let (a, b, c) = split3' lists ([], [], []) in
    (a |> List.rev, b |> List.rev, c |> List.rev)
end*)

type earg_t = TAst.ast * TAst.term_aux_t

let make_earg ast aux constraints =
  (ast, aux, constraints)

let rec split3' lists (acca,accb,accc) =
  match lists with
  | [] -> (acca,accb,accc)
  | (a, b, c)::rest -> split3' rest (a::acca, b::accb, c::accc)

let split3 lists =
  let (a, b, c) = split3' lists ([], [], []) in
  (a |> List.rev, b |> List.rev, c |> List.rev)


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
let ctor_id_name = Id_string.Pure ctor_name

let dtor_name = "dtor"
let dtor_id_name = Id_string.Pure dtor_name

let assign_name = Id_string.BinaryOp "="

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
  TAst.Aux.loc aux

let type_of_earg earg =
  let (_, aux) = earg in
  TAst.Aux.ty aux

module ErrorMsg = struct
  module ArgPosMap = Map.Make(Int)

  type t =
    (* num of params * num of args *)
    | DifferentArgNum of int * int
    (* target_type * source_arg * ErrorLevel *)
    | ConvErr of (type_info_t * earg_t * FuncMatchLevel.t) ArgPosMap.t * env_t
    | NoMatch of t list * Loc.t
    | MemberNotFound of env_t * env_t list * Loc.t
    | Msg of string

    | TmpError of string * env_t

  let print_env_trace env =
    let env_loc = env.Env.loc in
    match Env.get_env_record env with
    | Env.Module (lt, r) ->
       Printf.printf "  module   : %s (%s)\n"
                     r.Env.mod_name
                     (Loc.to_string env_loc)
    | Env.Function (lt, r) ->
       let name = Id_string.to_string r.Env.fn_name in
       let f pk =
         match pk with
         | Env.FnParamKindType ty -> Type.to_string ty
       in
       let params_s = r.Env.fn_param_kinds |> List.map f |> String.join ", " in
       let ret_ty_s = r.Env.fn_return_type |> Type.to_string in
       Printf.printf "  function : %s(%s): %s (%s)\n"
                     name
                     params_s
                     ret_ty_s
                     (Loc.to_string env_loc)
    | Env.Class (lt, r) ->
       let name = Id_string.to_string r.Env.cls_name in
       Printf.printf "  class    : %s (%s)\n"
                     name
                     (Loc.to_string env_loc)
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
       assert (l.Loc.pos_begin_cnum >= 0);
       assert (l.Loc.pos_end_cnum >= 0);
       assert (l.Loc.pos_end_cnum > l.Loc.pos_begin_cnum);
       Debug.printf "%s\n%d\n" (l.Loc.source_code) (String.length l.Loc.source_code);
       Debug.printf "%d %d\n" l.Loc.pos_begin_cnum l.Loc.pos_end_cnum;

       Bytes.sub_string l.Loc.source_code
                        l.Loc.pos_begin_cnum
                        (l.Loc.pos_end_cnum - l.Loc.pos_begin_cnum)
    | None -> "<unknown>"

  let rec print ?(loc=None) err =
    match err with
    | DifferentArgNum (params_num, args_num) ->
       Printf.printf "%s:\nError: requires %d but given %d\n"
                     (Loc.to_string loc) params_num args_num

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
                     (Loc.to_string loc);
       List.iter (fun err -> print err) errs

    | MemberNotFound (env, history, loc) ->
       let env_name = Id_string.to_string (Env.get_name env) in

       Printf.printf "%s:\nError: member \"%s\" is not found in %s\n"
                     (Loc.to_string loc)
                     (string_of_loc_region loc)
                     env_name;
       Printf.printf "Searched scopes are...\n";
       List.iter (fun env -> print_env_trace env) history

    | Msg msg ->
       Printf.printf "\n------------------\nError:\n %s\n\n-------------------\n" msg

    | TmpError (msg, env) ->
       Printf.printf "\n------------------\nError:\n %s\n\n-------------------\n" msg;
       let history =
         let rec f oe xs =
           match oe with
           | Some e -> f (Env.get_parent_env_opt e) (e :: xs)
           | None -> xs
         in
         f (Some env) [] |> List.rev
       in
       List.iter (fun env -> print_env_trace env) history
end

exception NError of ErrorMsg.t
let error err =
  raise (NError err)

let error_msg msg =
  raise (NError (ErrorMsg.Msg msg))
