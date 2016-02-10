(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries

type id_t = int

type 'ty chain_t =
  | Undef
  | Link of id_t
  | Val of 'ty

type 'ty type_map_t = (id_t, 'ty chain_t) Hashtbl.t
type 'ty value_map_t = (id_t, ('ty Ctfe_value.t) chain_t) Hashtbl.t

type 'ty t = {
  mutable fresh_id  : id_t;
  type_map          : 'ty type_map_t;
  value_map         : 'ty value_map_t
}


let empty () =
  {
    fresh_id = 0;
    type_map = Hashtbl.create 10;
    value_map = Hashtbl.create 10;
  }


let new_fresh_id holder =
  let cur_uni_id = holder.fresh_id in
  holder.fresh_id <- holder.fresh_id + 1;
  cur_uni_id

let generate_uni_id holder =
  let new_uni_id = new_fresh_id holder in
  Hashtbl.add holder.type_map new_uni_id Undef;
  Hashtbl.add holder.value_map new_uni_id Undef;
  new_uni_id


let rec search_until_terminal ?(debug_s="") mapping uni_id =
  let c = Hashtbl.find mapping uni_id in
  match c with
  | Val _ | Undef ->
             begin
               Printf.printf "@%s@ val or undef [%d]\n" debug_s uni_id;
               (uni_id, c)
             end
  | Link link_t_id ->
     begin
       Printf.printf "@%s@ link to [%d] -> \n" debug_s uni_id;
       search_until_terminal mapping link_t_id
     end


let link ?(debug_s="") holder mapping uni_id_a uni_id_b =
  let (a_id, a_val) = search_until_terminal ~debug_s:debug_s mapping uni_id_a in
  let (b_id, b_val) = search_until_terminal ~debug_s:debug_s mapping uni_id_b in
  match (a_val, b_val) with
  | (Undef, Undef) ->
     begin
       let new_uni_id = generate_uni_id holder in
       Hashtbl.replace mapping a_id (Link new_uni_id);
       Hashtbl.replace mapping b_id (Link new_uni_id);
       Printf.printf "@%s@ LINK [%d] = [%d] -> %d" debug_s a_id b_id new_uni_id
     end
  | (Val _, Undef) ->
     begin
       Printf.printf "@%s@ LINK [%d] -> %d" debug_s b_id a_id;
       Hashtbl.replace mapping b_id (Link a_id)
     end
  | (Undef, Val _) ->
     begin
       Printf.printf "@%s@ LINK [%d] -> %d" debug_s a_id b_id;
       Hashtbl.replace mapping a_id (Link b_id)
     end
  | _ -> failwith "[ICE] link"

let update ?(debug_s="") holder mapping uni_id ty =
  Printf.printf "@%s@ update %d\n" debug_s uni_id;
  let (term_id, cur_val) = search_until_terminal ~debug_s:debug_s mapping uni_id in
  match cur_val with
  | Undef | Val _ -> Hashtbl.replace mapping term_id (Val ty)
  | _ -> failwith "[ICE] update"


let search_type_until_terminal holder uni_id =
  search_until_terminal ~debug_s:"type" holder.type_map uni_id

let link_type holder uni_id_a uni_id_b =
  link holder ~debug_s:"type" holder.type_map uni_id_a uni_id_b

let update_type holder uni_id ty =
  update ~debug_s:"type" holder holder.type_map uni_id ty


let search_value_until_terminal holder uni_id =
  search_until_terminal ~debug_s:"value" holder.value_map uni_id

let link_value holder uni_id_a uni_id_b =
  link ~debug_s:"value" holder holder.value_map uni_id_a uni_id_b

let update_value holder uni_id ty =
  update ~debug_s:"value" holder holder.value_map uni_id ty
