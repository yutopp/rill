(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries
open Sema_context

module LifetimeMap =
  struct
    module M = Map.Make(Lifetime.Var_id)

    type 'v t = {
      ltm_map:  'v M.t;
      ltm_list: 'v list;
    }

    let empty =
      {
        ltm_map = M.empty;
        ltm_list = [];
      }

    let find key m =
      let { ltm_map = mm } = m in
      M.find key mm

    let mem key m =
      let { ltm_map = mm } = m in
      M.mem key mm

    let append key value m =
      let { ltm_map = mm; ltm_list = gsl } = m in
      {
        ltm_map = M.add key value mm;
        ltm_list = value :: gsl;    (* NOTE: save as reversed order *)
      }

    let to_list m =
      let { ltm_list = gsl } = m in
      gsl |> List.rev
  end

let rec solve_var lt m =
  match lt with
  | Lifetime.LtVar (var_id, _, _, _, _, _)
  | Lifetime.LtVarPlaceholder (var_id, _) ->
     begin
       try
         [%Loga.debug "lt = %s" (Lifetime.Var_id.to_string var_id)];
         let v = LifetimeMap.find var_id m in
         let () =
           match v with
           | Lifetime.LtVar (id, _, _, _, _, _)
           | Lifetime.LtVarPlaceholder (id, _) when id = var_id ->
              failwith "[ICE] Solve same lifetime ids"
           | _ ->
              ()
         in
         solve_var v m
       with
       | Not_found -> lt
     end
  | _ -> lt

let rec map_generics_args ?(m=LifetimeMap.empty) generis_params generics_args =
  match (generis_params, generics_args) with
  | ([], []) ->
     m
  | ([], _) ->
     failwith "[ERR]"
  | (_, []) ->
     m
  | (p::px, a::ax) ->
     begin
       match p with
       | Lifetime.LtVar (var_id, _, _, _, _, _) when not (Lifetime.is_undef a) ->
          Debug.printf " try REGISTER(mg) ARG %s -> %s\n"
                       (Lifetime.Var_id.to_string var_id) (Lifetime.to_string a);

          let nm = match (var_id, a) with
            | (var_id, Lifetime.LtVar (rhs_var_id, _, _, _, _, _)) when var_id = rhs_var_id ->
               Debug.printf " -> skip";
               m
            | _ ->
               LifetimeMap.append var_id a m
          in
          map_generics_args ~m:nm px ax
       | _ -> failwith ""
     end

let lifetime_check_constraint mm c =
  match c with
  (* TODO: lt > rt. lt will live longer than rt *)
  | Lifetime.LtMin (lt, rt) ->
     Debug.printf "LT: %s > %s"
                  (Lifetime.to_string lt) (Lifetime.to_string rt);
     Debug.printf "  : %s > %s"
                  (Lifetime.to_string @@ solve_var lt mm)
                  (Lifetime.to_string @@ solve_var rt mm);

     let b =
       let (>=) = Lifetime_constraints.(>=) in
       (solve_var lt mm) >= (solve_var rt mm)
     in
     if not b then
       failwith "[ERR] constraints"

exception Lifetime_param_length_is_different of type_info_t * type_info_t

let lifetime_map_generics_in_params mm params eargs : 'v LifetimeMap.t =
  let for_type mm pk earg =
    let (_, arg_aux) = earg in
    let {
      Aux.ta_type = arg_ty;
      Aux.ta_lt = arg_lt;
    } = arg_aux in
    match pk with
    | Env.FnParamKindType param_ty ->
       (* fix check aux *)
       let mm =
         match param_ty.Type_info.ti_aux_generics_args with
         | [] -> mm
         | [aux_generics_val] ->
            begin
              match aux_generics_val with
              | Lifetime.LtVar (var_id, _, _, _, _, _) ->
                 Debug.printf " REGISTER AUX %s -> %s\n" (Lifetime.Var_id.to_string var_id) (Lifetime.to_string arg_lt);
                 LifetimeMap.append var_id arg_lt mm
              | _ ->
                 failwith "[ICE]"
            end
         | _ -> failwith "[ICE]"
       in

       (**)
       let for_lifetime_arg mm param_generics_val arg_lt =
         let is_registerd var_id = LifetimeMap.mem var_id mm in
         match param_generics_val with
         | Lifetime.LtVar (var_id, _, _, _, _, _) ->
            Debug.printf " REGISTER PARAM %s <- %s | %b\n"
                         (Lifetime.Var_id.to_string var_id)
                         (Lifetime.to_string arg_lt)
                         (is_registerd var_id);

            let nm =
              if not (is_registerd var_id) then
                LifetimeMap.append var_id arg_lt mm
              else
                (* use the shortest lifetime *)
                let p = solve_var param_generics_val mm in
                let lt = Lifetime_constraints.min p arg_lt in
                LifetimeMap.append var_id lt mm
            in
            nm
         | _ ->
            mm
       in
       if not (List.length param_ty.Type_info.ti_generics_args = List.length arg_ty.Type_info.ti_generics_args) then
         raise (Lifetime_param_length_is_different (param_ty, arg_ty));

       List.fold_left2 for_lifetime_arg mm
                       param_ty.Type_info.ti_generics_args
                       arg_ty.Type_info.ti_generics_args
  in
  List.fold_left2 for_type mm params eargs

let clone_for_all lts =
  lts |> List.map Lifetime.clone
