(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries

type checked_state =
    InComplete
  | Checking
  | Complete

type module_privacy =
  | ModPublic
  | ModPrivate


module Kind =
  struct
    type t =
        Module
      | Class
      | Function
      | Variable
  end


(* used for id of environments *)
type id_t = Num.num
let id_counter = ref @@ Num.num_of_int 0


type 'ast env_t = {
   env_id           : id_t;
   parent_env       : 'ast env_t option;
   module_env       : 'ast env_t option;
   er               : 'ast env_record_t;
   mutable state    : checked_state;

   mutable rel_node : 'ast option;
}

 and 'ast env_record_t =
  | Root of 'ast lookup_table_t
  | MultiSet of 'ast multiset_record
  | Template of 'ast template_record

  | Module of 'ast lookup_table_t * module_record
  | Function of 'ast lookup_table_t * 'ast function_record
  | Class of 'ast lookup_table_t * 'ast class_record
  | Variable of 'ast variable_record
  | Scope of 'ast lookup_table_t

  | MetaVariable of Unification.id_t

 and 'ast type_info_t = 'ast env_t Type.info_t

 and 'ast name_env_mapping = (string, 'ast env_t) Hashtbl.t

 and 'ast lookup_table_t = {
   scope                    : 'ast name_env_mapping;
   mutable imported_mods    : ('ast env_t * module_privacy) list;
 }

 and 'ast multiset_record = {
   ms_kind                  : Kind.t;
   mutable ms_templates     : 'ast template_record list;

   mutable ms_normal_instances      : 'ast env_t list;
   mutable ms_template_instances    : 'ast env_t list;

   ms_instanced_args_memo           : (string, 'ast env_t) Hashtbl.t;
 }


 and 'ast template_record = {
   tl_name          : Nodes.id_string;
   tl_params        : 'ast;
   tl_inner_node    : 'ast;
 }


 (*
  *
  *)
 and module_record = {
   mod_pkg_names    : string list;
   mod_name         : string;
 }


 (*
  *
  *)
 and 'ast function_record = {
   fn_name                      : Nodes.id_string;
   mutable fn_mangled           : string option;

   mutable fn_template_vals     : ('ast type_info_t Ctfe_value.t) list;
   mutable fn_param_types       : 'ast type_info_t list;
   mutable fn_return_type       : 'ast type_info_t;
   mutable fn_detail            : 'ast function_record_var;
 }
 and 'ast function_record_var =
   | FnRecordNormal of 'ast function_record_normal
   | FnRecordExtern of 'ast function_record_extern
   | FnUndef

 and 'ast function_record_normal = {
   fn_n_param_envs  : 'ast env_t option list;
 }

 and 'ast function_record_extern = {
   fn_e_name        : string;
   fn_e_is_builtin  : bool;
 }


 (*
  *
  *)
 and 'ast variable_record = {
   var_name             : string;
   mutable var_type     : 'ast type_info_t;
   mutable var_detail   : 'ast variable_record_var;
 }
 and 'ast variable_record_var =
   | VarRecordNormal of 'ast variable_record_normal
   | VarUndef

 and 'ast variable_record_normal = unit (* TODO: implement *)


 (*
  *
  *)
 and 'ast class_record = {
   cls_name             : Nodes.id_string;
   mutable cls_mangled  : string option;

   mutable cls_template_vals    : ('ast type_info_t Ctfe_value.t) list;
   mutable cls_detail           : 'ast class_record_var;
 }
 and 'ast class_record_var =
   | ClsRecordExtern of class_record_extern
   | ClsUndef

 and class_record_extern = {
   cls_e_name           : string;
 }


let get_env_record env =
  let { er = er; _ } = env in
  er

let get_lookup_table e =
  let { er = er; _ } = e in
  match er with
  | Root (r) -> r
  | Module (r, _) -> r
  | Function (r, _) -> r
  | Class (r, _) -> r
  | Scope (r) -> r
  | _ -> failwith "has no lookup table"

let get_symbol_table e =
  let lt = get_lookup_table e in
  lt.scope


let is_root e =
  let { er = er; _ } = e in
  match er with
    Root _  -> true
  | _       -> false

let parent_env e =
  if is_root e then
    failwith "root env has no parent env"
  else
    let { parent_env = opt_penv } = e in
    match opt_penv with
      Some penv -> penv
    | None -> failwith ""

(* *)
let find_on_env e name =
  let lt = get_lookup_table e in
  Hashtbl.find_option lt.scope name

let rec find_all_on_env ?(checked_env=[]) e name =
  if List.mem e.env_id checked_env then
    failwith "[ERR] recursive package search";

  match find_on_env e name with
  | Some e -> [e]
  | None ->
     begin
       let lt = get_lookup_table e in
       (* find from import tables *)
       let search_module envs (mod_env, priv) =
         match priv with
         | ModPrivate ->
            begin
              match find_on_env mod_env name with
              | Some e -> e :: envs
              | None -> envs
            end
         | ModPublic ->
            begin
              let res =
                find_all_on_env ~checked_env:(e.env_id::checked_env) mod_env name
              in
              res @ envs
            end
       in
       List.fold_left search_module [] lt.imported_mods
     end

(*  *)
let rec lookup e name =
  let target = find_all_on_env e name in
  match target with
  | [] -> if is_root e then
            failwith "[ICE] cannot find on root env"
          else
            let penv = parent_env e in
            if is_root penv then
              []
            else
              lookup penv name
  | xs -> xs


(*  *)
let add_inner_env target_env name e =
  let t = get_symbol_table target_env in
  Hashtbl.add t name e


let import_module ?(privacy=ModPrivate) env mod_env =
  let lt = get_lookup_table env in
  lt.imported_mods <- (mod_env, privacy) :: lt.imported_mods


let empty_lookup_table ?(init=8) () =
  {
    scope = Hashtbl.create init;
    imported_mods = [];
  }

let create_env parent_env er =
  let cur_id = !id_counter in
  Num.incr_num id_counter;

  let opt_mod_env = match parent_env.er with
    | Module _ -> Some parent_env
    | _ -> parent_env.module_env
  in
  {
    env_id = cur_id;
    parent_env = Some parent_env;
    module_env = opt_mod_env;
    er = er;
    state = InComplete;
    rel_node = None;
  }

(**)
let is_checked e =
  let { state = s; _ } = e in
  s = Checking || s = Complete

let is_incomplete e =
  not (is_checked e)

let is_complete e =
  let { state = s; _ } = e in
  s = Complete


let update_status e ns =
  e.state <- ns


(**)
let update_rel_ast e node =
  e.rel_node <- Some node

let get_rel_ast e =
  Option.get e.rel_node


(**)
let make_root_env () =
  let tbl = empty_lookup_table () in
  let cur_id = !id_counter in
  Num.incr_num id_counter;
  {
    env_id = cur_id;
    parent_env = None;
    module_env = None;
    er = Root (tbl);
    state = Complete;
    rel_node = None;
  }


module ModuleOp =
  struct
    let get_record env =
      let er = get_env_record env in
      match er with
      | Module (_, r) -> r
      | _ -> failwith "ModuleOp.get_record : not module"
  end


module MultiSetOp =
  struct
    let find_or_add env name k =
      let oe = find_on_env env name in
      match oe with
      (* Found *)
      | Some ({ er = (MultiSet { ms_kind = k; _ }); _} as e) ->
         e
      | Some _ -> failwith "[ERR] suitable multienv is not found"

      (* *)
      | None ->
         let e = create_env env (MultiSet {
                                     ms_kind = k;
                                     ms_templates = [];
                                     ms_normal_instances = [];
                                     ms_template_instances = [];
                                     ms_instanced_args_memo = Hashtbl.create 0
                                   }) in
         add_inner_env env name e;
         e

    let get_record env =
      let er = get_env_record env in
      match er with
      | MultiSet (r) -> r
      | _ -> failwith "MultiSetOp.get_record : not multiset"


    let add_normal_instances menv env =
      let record = get_record menv in
      (* add env to lists as candidates *)
      record.ms_normal_instances <- env :: record.ms_normal_instances

    let add_template_instances menv env =
      let record = get_record menv in
      (* add env to lists as candidates *)
      record.ms_template_instances <- env :: record.ms_template_instances
  end


module FunctionOp =
  struct
    let get_record env =
      let er = get_env_record env in
      match er with
      | Function (_, r) -> r
      | _ -> failwith "FunctionOp.get_record : not function"

    let get_extern_record env =
      let r = get_record env in
      match r.fn_detail with
      | FnRecordExtern r -> r
      | _ -> failwith "FunctionOp.get_extern_record : not extern"

    let get_normal_record env =
      let r = get_record env in
      match r.fn_detail with
      | FnRecordNormal r -> r
      | _ -> failwith "FunctionOp.get_extern_record : not normal"
  end


module ClassOp =
  struct
    let get_record env =
      let er = get_env_record env in
      match er with
      | Class (_, r) -> r
      | _ -> failwith "ClassOp.get_record : not class"
  end


module VariableOp =
  struct
    let get_record env =
      let er = get_env_record env in
      match er with
      | Variable (r) -> r
      | _ -> failwith "VariableOp.get_record : not function"
  end


let print env =
  let print_table tbl indent =
    let open Printf in
    let p name =
      printf "%s+ %s\n" indent name
    in
    tbl |> Hashtbl.iter @@ fun name _ -> p name
  in
  let print_ er f indent =
    let open Printf in
    let nindent = (indent ^ "  ") in
    match er with
    | Root (lt) ->
       begin
         printf "%sRootEnv\n" indent;
         print_table lt.scope indent;
         f nindent;
       end
    | Module (lt, r) ->
       begin
         printf "%sModuleEnv - %s\n" indent r.mod_name;
         print_table lt.scope indent;
         f nindent;
       end
    | Function (lt, r) ->
       begin
         let name = Nodes.string_of_id_string r.fn_name in
         printf "%sFunction - %s\n" indent name;
         print_table lt.scope indent;
         f nindent;
       end
    | Class (lt, r) ->
       begin
         let name = Nodes.string_of_id_string r.cls_name in
         printf "%sClassEnv - %s\n" indent name;
         print_table lt.scope indent;
         f nindent;
       end
    | Scope _ ->
       begin
         printf "%Scope\n" indent;
         f nindent
       end
    | MetaVariable uni_id ->
       begin
         printf "%sMetaVariable - %d\n" indent uni_id;
         f nindent
       end

    | MultiSet r ->
       begin
         let kind_s = function
           | Kind.Module -> "module"
           | Kind.Class -> "class"
           | Kind.Function -> "function"
           | Kind.Variable -> "variable"
         in
         printf "%sMultiSet - %s\n" indent (kind_s r.ms_kind);
         f nindent;
       end

    | _ -> failwith "print: unsupported env"
  in
  let rec dig env f =
    if is_root env then begin
      let { er = er; _ } = env in
      print_ er f ""
    end else begin
      let pr = parent_env env in
      let { er = er; _ } = env in
      let newf indent = print_ er f indent in
      dig pr newf
    end
  in
  dig env (fun _ -> ())


let get_full_module_name e =
  let mod_env = Option.get e.module_env in
  let mr = ModuleOp.get_record mod_env in
  mr.mod_pkg_names @ [mr.mod_name]
