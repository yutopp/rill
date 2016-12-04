(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries

module Var_id = Generic_counter.Counter(Int32)

type sort =
  | LtSingle of Id_string.t * Loc.t
  | LtLongerThan of Id_string.t * Id_string.t

type sub_level_virt_t =
  | LtSlNormal of int
  | LtSlParamRef of int

type t =
  | LtStatic
  | LtUnmanaged
  | LtGc
  (* env_id * nest_level * sub_nest * (aux counter) *)
  | LtDynamic of Env_system.EnvId.t * Env_system.NestLevel.t * sub_level_virt_t * int
  | LtVar of Var_id.t * Id_string.t * Env_system.EnvId.t * Env_system.NestLevel.t * Var_id.t list * Loc.t
  | LtVarPlaceholder of Var_id.t * Loc.t
  | LtUndef

type constraint_t =
  | LtMin of t * t

let make_placeholder lt_spec =
  let (id, loc) = match lt_spec with
    | LtSingle (id, loc) -> (id, loc)
    | LtLongerThan (lhs_id, _) -> (lhs_id, Loc.dummy)
  in
  let var_id = Var_id.generate () in
  (LtVarPlaceholder (var_id, loc), id)

let to_string lt = match lt with
  | LtStatic -> "`static"
  | LtUnmanaged -> "`unmanaged"
  | LtGc -> "`gc"
  | LtDynamic (env_id, nest, sub_nest, aux) ->
     let s = match sub_nest with
       | LtSlNormal i -> string_of_int i
       | LtSlParamRef i -> "ref^" ^ (string_of_int i)
     in
     Printf.sprintf "`DYNAMIC(env: %s, nest: %d, %s, %d)"
       (Env_system.EnvId.to_string env_id) (Int32.to_int nest) s aux
  | LtVar (id, name, env_id, nest, rhs_comparable, loc) ->
     Printf.sprintf "`VAR(%s[%s] | env: %s, nest: %d, [%s], loc: %s)"
       (Var_id.to_string id) (Id_string.to_string name)
       (Env_system.EnvId.to_string env_id) (Int32.to_int nest)
       (rhs_comparable |> List.map Var_id.to_string |> String.join ", ")
       (Loc.to_string loc)
  | LtVarPlaceholder (id, _) ->
     Printf.sprintf "`PLACEHOLDER(%s)" (Var_id.to_string id)
  | LtUndef -> "`UNDEF"

let is_undef lt =
  match lt with
  | LtUndef -> true
  | _ -> false

let get_id_from_sort s =
  match s with
  | LtSingle (id, _) -> id
  | LtLongerThan (id, _) -> id

module G = struct
  module Vertex = struct
    type nonrec t = t
    let compare a b =
      match (a, b) with
      | (LtVar (l_var_id, _, _, _, _, _), LtVar (r_var_id, _, _, _, _, _))
      | (LtVar (l_var_id, _, _, _, _, _), LtVarPlaceholder (r_var_id, _))
      | (LtVarPlaceholder (l_var_id, _), LtVar (r_var_id, _, _, _, _, _))
      | (LtVarPlaceholder (l_var_id, _), LtVarPlaceholder (r_var_id, _)) ->
         Pervasives.compare l_var_id r_var_id
      | _ ->
         failwith "cannot compare"
    let equal = (=)
    let hash = Hashtbl.hash
  end

  include Graph.Imperative.Digraph.Concrete(Vertex)
end
module Comp = Graph.Components.Make(G)
module A = Graph.Oper.Neighbourhood(G)

module G2 = struct
  module Vertex = struct
    type t = int
    let compare = Pervasives.compare
    let equal = (=)
    let hash = Hashtbl.hash
  end

  include Graph.Imperative.Digraph.Concrete(Vertex)
end

(* `a: `b reads as lifetime `a is at least as long as `b *)
(* `a >: `b *)
let convert raw_lts lt_constaints base_env_id =
  let open Graph in

  let validate_as_fresh_lt lt = match lt with
    | LtVarPlaceholder (id, _) -> ()
    | _ -> (* TODO: change to exception *) failwith "should be fresh lifetime"
  in
  let validate_as_var_lt lt = match lt with
    | LtVarPlaceholder _
    | LtVar _ -> ()
    | _ -> (* TODO: change to exception *) failwith "should be var lifetime"
  in
  List.iter validate_as_fresh_lt raw_lts;
  List.iter (fun (longer, base) -> validate_as_fresh_lt longer; validate_as_var_lt base) lt_constaints;

  let g = G.create () in
  List.iter (fun n -> G.add_vertex g n) raw_lts;
  List.iter (fun (a, b) -> G.add_vertex g a; G.add_vertex g b) lt_constaints;

  (* "(a, b)" means A >= B. A will be live at least as longer as B. edge will be B -> A *)
  List.iter (fun (a, b) -> G.add_edge g b a) lt_constaints;

  List.iter (fun (a, b) -> Debug.printf "LL (%s: %s)\n" (to_string a) (to_string b)) lt_constaints;

  let (n, fs) = Comp.scc g in
  let scc_arr = Comp.scc_array g in

  let g2 = G2.create () in
  G.iter_vertex (fun v -> let v2 = v |> fs in G2.add_vertex g2 v2) g;
  G.iter_edges (fun a b -> let a' = a |> fs in
                           let b' = b |> fs in
                           if a' <> b' then
                             G2.add_edge g2 a' b') g;

  let rec all_pred' g v acc =
    let pred = G2.pred g v in
    let cacc = v :: acc in
    let f xs v = all_pred' g v xs in
    List.fold_left f cacc pred
  in
  let all_pred g v = all_pred' g v [] |> List.rev in

  let conved_lts =
    let f lt =
      let v = fs lt in
      let mm =
        let pred = all_pred g2 v in
        let rhs_cmps =
          pred |> List.map (fun v -> scc_arr.(v)) |> List.flatten
        in
        (lt, v, rhs_cmps)
      in
      mm
    in
    List.map f raw_lts
  in

  let to_ltvar (p_lt, v, rhs_cmps) =
    let get_var_id lt = match lt with
      | LtVarPlaceholder (id, _) -> id
      | LtVar (id, _, rhs_env_id, _, _, _) -> id
      | _ -> failwith ""
    in
    let get_loc lt = match lt with
      | LtVarPlaceholder (_, loc) -> loc
      | LtVar (_, _, _, _, _, loc) -> loc
      | _ -> failwith ""
    in
    let (lt_id, loc) = (get_var_id p_lt, get_loc p_lt) in
    LtVar (lt_id, Id_string.Pure "", base_env_id, Int32.of_int v, rhs_cmps |> List.map get_var_id, loc)
  in
  List.map to_ltvar conved_lts
