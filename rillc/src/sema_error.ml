(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries
open Sema_definitions
open Sema_context

exception Instantiation_failed of string
exception Template_type_mismatch

exception Normal_error of error_msg_t
exception Fatal_error of error_msg_t

let error err =
  raise (Normal_error err)

let error_msg msg =
  raise (Normal_error (Error_msg.Msg msg))

let fatal_error err =
  raise (Fatal_error err)

let fatal_error_msg msg =
  raise (Fatal_error (Error_msg.Msg msg))

let show_env env =
  let open Error_msg in

  let env_loc = env.Env.loc in
  match Env.get_env_record env with
  | Env.Module (lt, r) ->
     Printf.sprintf "  module   : %s (%s)"
                    r.Env.mod_name
                    (Loc.to_string env_loc)
  | Env.Function (lt, r) ->
     let name = Env.get_name env |> Id_string.to_string in
     let f pk =
       match pk with
       | Env.FnParamKindType ty -> Type.to_string ty
     in
     let params_s = r.Env.fn_param_kinds |> List.map f |> String.join ", " in
     let ret_ty_s = r.Env.fn_return_type |> Type.to_string in
     Printf.sprintf "  function : %s(%s): %s (%s)"
                    name
                    params_s
                    ret_ty_s
                    (Loc.to_string env_loc)
  | Env.Class (lt, r) ->
     let name = Id_string.to_string r.Env.cls_name in
     Printf.sprintf "  class    : %s (%s)"
                    name
                    (Loc.to_string env_loc)

  | Env.Variable r ->
     let name = Id_string.to_string r.Env.var_name in
     Printf.sprintf "  variable    : %s (%s)"
                    name
                    (Loc.to_string env_loc)

  | Env.Scope _ ->
     Printf.sprintf "Scope"

  | Env.Root _ -> ""
  | Env.MetaVariable _ -> ""
  | Env.MultiSet _ -> ""
  | _ -> failwith "[ICE] unknown env"

let string_of_loc_region loc =
  match loc with
  | Some l ->
     assert (l.Loc.pos_begin_cnum >= 0);
     assert (l.Loc.pos_end_cnum >= 0);
     assert (l.Loc.pos_end_cnum > l.Loc.pos_begin_cnum);

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
  let open Error_msg in

  match err with
  | DifferentArgNum (params_num, args_num) ->
     Printf.printf "%s:\nError: requires %d but given %d\n"
                   (Loc.to_string loc) params_num args_num

  | ConvErr (trg_ty, (src_ty, src_loc), env) ->
     Printf.printf "Type mismatch at %s:\n"
                   (show_env env);
     Printf.printf "    expr   :  %s (%s)\n"
                   (string_of_loc_region src_loc) (Loc.to_string src_loc);
     Printf.printf "    found  :  %s\n" (Type.to_string src_ty);
     Printf.printf "    expect :  %s\n" (Type.to_string trg_ty);

  | ArgConvErr (m, f_env) ->
     Printf.printf "Type mismatch to call %s:\n"
                   (show_env f_env);
     let p k (trg_ty, (src_ty, src_loc), level) =
       Printf.printf "  %dth arg\n" k;
       let _ = match level with
         | Function.MatchLevel.NoMatch ->
            Printf.printf "    expr   :  %s (%s)\n"
                          (string_of_loc_region src_loc) (Loc.to_string src_loc);
            Printf.printf "    found  :  %s\n" (Type.to_string src_ty);
            Printf.printf "    expect :  %s\n" (Type.to_string trg_ty);
         | _ ->
            failwith "[ERR] not supported"
       in
       ()
     in
     ArgLocMap.iter p m

  | NoOverloadSet (errs, _loc) ->
     List.iter (fun err -> print err; Printf.printf "\n") errs

  | NoMatch (errs, loc) ->
     Printf.printf "%s:\nError: There are no matched functions (candidate: %d)\n"
                   (Loc.to_string loc)
                   (List.length errs);
     List.iter (fun err -> print err; Printf.printf "\n") errs

  | MemberNotFound (env, history, loc) ->
     let env_name = Id_string.to_string (Env.get_name env) in

     Printf.printf "%s:\nError: member \"%s\" is not found in %s\n"
                   (Loc.to_string loc)
                   (string_of_loc_region loc)
                   env_name;
     Printf.printf "Searched scopes are...\n";
     List.iter (fun env -> show_env env |> Printf.printf "%s\n") history

  | ModuleNotFound (full_module_name, hist, loc) ->
     Printf.printf "%s:\nError: module \"%s\" is not found.\n"
                   (Loc.to_string loc)
                   full_module_name;
     hist |> List.iter
               (fun (package_name, dirs) ->
                 Printf.printf " \"%s\" is not found in\n" package_name;
                 dirs |> List.iter
                           (fun d ->
                             Printf.printf "  -> %s\n" d;
                           );
                 ()
               )

  | ModuleNameDifferent (expect_mod_head, actual_mod_head) ->
     let loc = Loc.dummy in
     let to_string (pkg_names, mod_name) =
       String.join "." (pkg_names @ [mod_name])
     in
     Printf.printf "%s:\nError: module \"%s\" should be \"%s\".\n"
                   (Loc.to_string loc)
                   (to_string actual_mod_head)
                   (to_string expect_mod_head);

  | Ambiguous (_, f_envs, loc) ->
     Printf.printf "%s:\nError: Ambiguous\n"
                   (Loc.to_string loc);
     List.iter (fun env ->
                Printf.printf "%s\n" (show_env env))
               f_envs

  | MultiSymbol (env, loc) ->
     Printf.printf "%s:\nError: Symbol is already defined\n"
                   (Loc.to_string loc);
     Printf.printf "%s" (show_env env);

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
     List.iter (fun env -> show_env env |> Printf.printf "%s\n") history

  | DiffExecLevel {loc; expect; actual} ->
     Printf.printf "%s:\nError: Cannot execute in execution context\n"
                   (Loc.to_string loc);
     Printf.printf "    expr   :  %s\n" (string_of_loc_region loc);
     Printf.printf "    actual :  %s\n" (Meta_level.to_string actual);
     Printf.printf "    expect :  %s\n" (Meta_level.to_string expect)

  | DiffReturnType {loc; expect; actual} ->
     Printf.printf "%s:\nError: Return type must be same\n"
                   (Loc.to_string loc);
     Printf.printf "    expr   :  %s\n" (string_of_loc_region loc);
     Printf.printf "    actual :  %s\n" (Type.to_string actual);
     Printf.printf "    expect :  %s\n" (Type.to_string expect)

  | NoReturnStmts {loc: Loc.t; env} ->
     (* HINT: when some errors are happened in the function *)
     Printf.printf "%s:\nError: There is no return statements in this control flow\n"
                   (Loc.to_string loc)
  | ReturnTypeIsNotDetermined {loc: Loc.t; env} ->
     (* HINT: when the function is recursive called *)
     Printf.printf "%s:\nError: Type of this function is not determined\n"
                   (Loc.to_string loc)

let store_error_message err ctx =
  ctx.sc_errors <- err :: ctx.sc_errors

let process_error err ctx =
  Printf.printf "\n===============================\n";
  print err;
  store_error_message err ctx;
  Printf.printf "\n===============================\n";
  ()

let print_errors ctx =
  List.iter (fun err ->
             process_error err ctx
            ) ctx.sc_errors
