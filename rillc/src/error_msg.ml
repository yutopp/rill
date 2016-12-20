(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries

module ArgPosMap = Map.Make(Int)

type ('ty, 'env, 'earg) t =
  (* num of params * num of args *)
  | DifferentArgNum of int * int
  (* target_type * source_arg * ErrorLevel *)
  | ConvErr of ('ty * 'earg * Function.MatchLevel.t) ArgPosMap.t * 'env
  | NoMatch of ('ty, 'env, 'earg) t list * Loc.t
  | MemberNotFound of 'env * 'env list * Loc.t
  | PackageNotFound of string * (string * string list) list

  | Msg of string

  | TmpError of string * 'env

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
     Printf.printf "begin = %d / end = %d\n" l.Loc.pos_begin_cnum l.Loc.pos_end_cnum;
     Printf.printf "s = %s\n" (Loc.to_string (Some l));

     let lines =
       Batteries.File.with_file_in l.Loc.pos_fname
                                   (fun input ->
                                     let lines =
                                       input
                                       |> IO.to_input_channel
                                       |> input_lines
                                     in

                                     Enum.drop (l.Loc.pos_begin_lnum-1) lines;
                                     let rng =
                                       Enum.take (l.Loc.pos_end_lnum - (l.Loc.pos_begin_lnum - 1)) lines
                                     in

                                     rng |> List.of_enum
                                   )
     in
     let range = lines |> String.join "\n" in
     if l.Loc.pos_begin_lnum == l.Loc.pos_end_lnum then
       String.sub range l.Loc.pos_begin_bol (l.Loc.pos_end_bol - l.Loc.pos_begin_bol)
     else
       (* TODO: implement *)
       range
  | None -> "<unknown>"

let rec print ?(loc=None) err =
  match err with
  | DifferentArgNum (params_num, args_num) ->
     Printf.printf "%s:\nError: requires %d but given %d\n"
                   (Loc.to_string loc) params_num args_num

  | ConvErr (m, f_env) ->
     let p k (trg_ty, src_arg, level) =
       let (_, aux) = src_arg in
       let src_loc = Aux.loc aux in
       let src_ty = Aux.ty aux in

       let msg = match level with
         | Function.MatchLevel.NoMatch ->
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

  | PackageNotFound (full_module_name, hist) ->
     Printf.printf "Error: module \"%s\" is not found.\n" full_module_name;
     hist |> List.iter
               (fun (package_name, dirs) ->
                 Printf.printf " \"%s\" is not found in\n" package_name;
                 dirs |> List.iter
                           (fun d ->
                             Printf.printf "  -> %s\n" d;
                           );
                 ()
               )

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
