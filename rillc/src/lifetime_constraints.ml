(*
 * Copyright yutopp 2016 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

(* returns a lifetime which has a lifetime shorter than else one *)
let min lhs rhs =
  Debug.printf "LtCompare %s <> %s\n" (Lifetime.to_string lhs) (Lifetime.to_string rhs);

  (* in implementation, lifetime is shorter if nest_level is larger*)
  let min lhs rhs =
    let (lhs_env_id, lhs_nest_level, lhs_lt) = lhs in
    let (rhs_env_id, rhs_nest_level, rhs_lt) = rhs in
    if lhs_env_id <= rhs_env_id && lhs_nest_level >= rhs_nest_level then
      lhs_lt
    else
      rhs_lt
  in

  let b =
    match (lhs, rhs) with
    | (Lifetime.LtDynamic (lhs_e, lhs_n, _, _),
       Lifetime.LtVar (_, _, rhs_e, rhs_n, _, _))
    | (Lifetime.LtVar (_, _, lhs_e, lhs_n, _, _),
       Lifetime.LtDynamic (rhs_e, rhs_n, _, _))
    | (Lifetime.LtDynamic (lhs_e, lhs_n, _, _),
       Lifetime.LtDynamic (rhs_e, rhs_n, _, _))
    | (Lifetime.LtVar (_, _, lhs_e, lhs_n, _, _),
       Lifetime.LtVar (_, _, rhs_e, rhs_n, _, _)) ->
       min (lhs_e, lhs_n, lhs) (rhs_e, rhs_n, rhs)

    | ((Lifetime.LtVar _ as lt), Lifetime.LtUnmanaged)
    | ((Lifetime.LtDynamic _ as lt), Lifetime.LtUnmanaged)
    | (Lifetime.LtUnmanaged, (Lifetime.LtVar _ as lt))
    | (Lifetime.LtUnmanaged, (Lifetime.LtDynamic _ as lt)) -> lt

    | ((Lifetime.LtVar _ as lt), Lifetime.LtStatic)
    | ((Lifetime.LtDynamic _ as lt), Lifetime.LtStatic)
    | (Lifetime.LtStatic, (Lifetime.LtVar _ as lt))
    | (Lifetime.LtStatic, (Lifetime.LtDynamic _ as lt)) -> lt

    | (Lifetime.LtUnmanaged, Lifetime.LtUnmanaged) -> Lifetime.LtUnmanaged
    | (Lifetime.LtStatic, Lifetime.LtStatic) -> Lifetime.LtStatic

    | _ -> failwith @@ "[ERR] cannot compare " ^ (Lifetime.to_string lhs) ^ " : " ^ (Lifetime.to_string rhs)
  in

  Debug.printf "LtCompare %s <> %s = %s\n"
               (Lifetime.to_string lhs)
               (Lifetime.to_string rhs)
               (Lifetime.to_string b);

  b

(* returns true if lhs will live at least as long as rhs *)
let (>=) lhs rhs =
  let check lhs_env_id lhs_nest rhs_env_id rhs_nest =
    (* returns true, if maybe_inner_env_id is a sub env of parent_env_id *)
    let rec is_inner_env_id parent_env_id maybe_inner_env_id =
      let Env_system.EnvId.E (parent_uniq_env_id, _) = parent_env_id in
      let Env_system.EnvId.E (inner_uniq_env_id, opt_parent_env_id) = maybe_inner_env_id in
      Debug.printf "-> %s = %s\n"
                   (Env_system.EnvId.to_string parent_env_id)
                   (Env_system.EnvId.to_string maybe_inner_env_id);
      match opt_parent_env_id with
      | Some next_parent_env_id ->
         let Env_system.EnvId.E (np_uniq_env_id, _) = next_parent_env_id in
         Debug.printf "compare %s = %s\n"
                      (Env_system.EnvId.to_string parent_env_id)
                      (Env_system.EnvId.to_string next_parent_env_id);
         if parent_uniq_env_id = np_uniq_env_id then
           (Debug.printf "matched\n";
            true)
         else
           is_inner_env_id parent_env_id next_parent_env_id
      | None ->
         false
    in

    if lhs_env_id = rhs_env_id then
      (* if env id has same level,
       * the one that nest level is shorter or same will have longer lifetime *)
      lhs_nest <= rhs_nest
    else
      (* assume the situation of the constraint "'a: 'b"
       * in this case, at least 'a must not be a child env of 'b *)
      not (is_inner_env_id rhs_env_id lhs_env_id)
  in

  match (lhs, rhs) with
  | (lhs, rhs) when lhs = rhs -> true

  | (Lifetime.LtDynamic (lhs_env_id, lhs_nest, _, _)), (Lifetime.LtDynamic (rhs_env_id, rhs_nest, _, _))
  | (Lifetime.LtVar (_, _, lhs_env_id, lhs_nest, _, _)), (Lifetime.LtDynamic (rhs_env_id, rhs_nest, _, _))
  | (Lifetime.LtDynamic (lhs_env_id, lhs_nest, _, _)), (Lifetime.LtVar (_, _, rhs_env_id, rhs_nest, _, _)) ->
     check lhs_env_id lhs_nest rhs_env_id rhs_nest

  | (Lifetime.LtVar (_, _, lhs_env_id, lhs_nest, lhs_comparables_in_rhs, _),
     Lifetime.LtVar (rhs_var_id, _, rhs_env_id, rhs_nest, _, _)) ->
     if List.exists (fun a -> a = rhs_var_id) lhs_comparables_in_rhs then
       check lhs_env_id lhs_nest rhs_env_id rhs_nest
     else
       failwith "[ERR] cannot compare"

  | (Lifetime.LtUnmanaged, Lifetime.LtDynamic _) ->
     true

  | (Lifetime.LtStatic, Lifetime.LtUnmanaged)
  | (Lifetime.LtUnmanaged, Lifetime.LtStatic) ->
     true

  | (_, _) ->
     failwith @@ "uncomparable " ^ (Lifetime.to_string lhs) ^ " " ^ (Lifetime.to_string rhs)
