(*
 * Copyright yutopp 2016 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries
open Stdint

let analyze_boot_expr node ctx attr : 'ty Ctfe_value.t =
  match Ast.kind_of node with
  | Ast.IntLit (i, bits, signed, loc) ->
     begin
       match bits with
       | 32 -> if signed then
                 Ctfe_value.Int32 (Int32.of_int i)
               else
                 Ctfe_value.Uint32 (Uint32.of_int i)
       | _ -> failwith ""
     end

  | Ast.BoolLit (b, loc) ->
     begin
       Ctfe_value.Bool b
     end

  | Ast.StringLit (str, loc) ->
     begin
       failwith "[ICE]: not supported yet"
     end

  | _ ->
     begin
       Ast.print node;
       failwith "analyze_expr: unsupported node"
     end

let find_val_impl opt_attr key f =
  match opt_attr with
  | Some tbl ->
     begin
       let opt_value_node = Hashtbl.find_option tbl key in
       match opt_value_node with
         Some value_node ->
         begin
           match value_node with
           | None -> Some (Ctfe_value.Bool true)
           | Some value ->
              begin
                let ctfe_v = f value in
                Some (ctfe_v)
              end
         end
       | None -> None
     end
  | None -> None

(* it can treat simple nodes *)
let find_boot_val opt_attr key ctx =
  let f tnode =
    analyze_boot_expr tnode ctx None
  in
  find_val_impl opt_attr key f

(*
let find_attr_ctfe_val opt_attr key parent_env ctx =
  let f node =
    let sub_expr_spec = SubExprSpec.empty () in
    let (nnode, naux) =
      analyze_expr node parent_env sub_expr_spec ctx None
    in
    let {
      TAst.Aux.ta_type = ty;
      TAst.Aux.ta_ml = ml;
    } = naux in
    let (v, _) = eval_texpr_as_ctfe nnode ty ml parent_env ctx None in
    v
  in
  find_attr_val_impl opt_attr key f
 *)

let find_val opt_attr key ctx =
  (* TODO: support CTFE val *)
  find_boot_val opt_attr key ctx

let find_bool_val opt_attr key ctx =
  let opt_v = find_val opt_attr key ctx in
  match opt_v with
  | Some v ->
     begin
       match v with
       | Ctfe_value.Bool b -> b
       | _ -> failwith "[ERR] not bool value"
     end
  | None -> false (* default value *)

let find_int32_val opt_attr key ctx =
  let opt_v = find_val opt_attr key ctx in
  match opt_v with
  | Some v -> begin match v with
                    | Ctfe_value.Int32 i -> Some i
                    | _ -> failwith "[ERR] not int32 value"
              end
  | None -> None

let find_uint32_val opt_attr key ctx =
  let opt_v = find_val opt_attr key ctx in
  match opt_v with
  | Some v -> begin match v with
                    | Ctfe_value.Uint32 i -> Some i
                    | _ -> failwith "[ERR] not uint32 value"
              end
  | None -> None
