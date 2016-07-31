(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries
open Env_system

type checked_state_t =
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
      | Other

    let to_string = function
      | Module -> "module"
      | Class -> "class"
      | Function -> "function"
      | Variable -> "variable"
      | Other -> "other"
  end


type 'ast env_t = {
   env_id                   : EnvId.t;
   parent_env               : 'ast env_t option;
   context_env              : 'ast env_t option;
   module_env               : 'ast env_t option;
   er                       : 'ast env_record_t;
   mutable state            : checked_state_t;
   mutable closed           : bool;
   mutable meta_level       : Meta_level.t;
   mutable rel_node         : 'ast option;
   mutable callee_when_exit : 'ast list;
   nest_level               : NestLevel.t;

   (* information for error message *)
   loc                      : Nodes.Loc.t;
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
  | LifetimeVariable of Lifetime.t

  | Unknown

 and 'ast type_info_t = 'ast env_t Type_info.t

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

   ms_instanced_args_cache_for_env  : (string, 'ast env_t) Hashtbl.t;
   ms_instanced_args_cache_for_node : (string, 'ast) Hashtbl.t;
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
   fn_name                          : Nodes.id_string;
   mutable fn_mangled               : string option;

   mutable fn_template_vals         : ('ast type_info_t Ctfe_value.t) list;
   mutable fn_param_kinds           : 'ast function_param_kind_t list;
   mutable fn_return_type           : 'ast type_info_t;
   mutable fn_is_auto_return_type   : bool;
   mutable fn_detail                : 'ast function_record_var;
 }
 and 'ast function_record_var =
   | FnRecordNormal of function_def_var * 'ast function_kind_var * 'ast function_spec
   | FnRecordImplicit of function_def_var * 'ast function_kind_var * 'ast function_spec
   | FnRecordExternal of function_def_var * 'ast function_kind_var * string
   | FnRecordBuiltin of function_def_var * 'ast function_kind_var * string
   | FnUndef

 and 'ast function_param_kind_t =
   | FnParamKindType of 'ast type_info_t

 and function_def_var =
   | FnDefProvidedByUser
   | FnDefDefaulted of bool (* is trivial *)
   | FnDefDeleted

 and 'ast function_kind_var =
   | FnKindFree
   | FnKindMember
   (* 'ast env_t option is a variable of reciever *)
   | FnKindDefaultConstructor of 'ast env_t option
   | FnKindCopyConstructor of 'ast env_t option
   | FnKindMoveConstructor of 'ast env_t option
   | FnKindConstructor of 'ast env_t option
   | FnKindDestructor of 'ast env_t option

 and 'ast function_spec = {
   fn_spec_param_envs  : 'ast env_t option list;
 }


 (*
  *
  *)
 and 'ast class_record = {
   cls_name             : Nodes.id_string;
   mutable cls_mangled  : string option;

   mutable cls_template_vals    : ('ast type_info_t Ctfe_value.t) list;
   mutable cls_detail           : class_record_var;
   mutable cls_traits           : class_traits_t;

   mutable cls_member_vars      : 'ast env_t list;  (* include templates *)
   mutable cls_member_funcs     : 'ast env_t list;  (* include templates *)

   mutable cls_size             : Stdint.uint32 option;  (* bytes *)
   mutable cls_align            : Stdint.uint32 option;  (* bytes *)

   mutable cls_default_ctor     : 'ast env_t option;
   mutable cls_copy_ctor        : 'ast env_t option;
   mutable cls_move_ctor        : 'ast env_t option;
   mutable cls_dtor             : 'ast env_t option;
   mutable cls_copy_assign      : 'ast env_t option;
   mutable cls_move_assign      : 'ast env_t option;
 }
 and class_record_var =
   | ClsRecordExtern of class_record_extern
   | ClsRecordNormal
   | ClsUndef

 and class_record_extern = {
   cls_e_name           : string;
 }

 and class_traits_t = {
   cls_traits_is_primitive              : bool;
   cls_traits_is_always_value           : bool;
   cls_traits_has_user_defined_ctor     : bool;
   cls_traits_default_ctor_state        : function_def_var;
   cls_traits_copy_ctor_state           : function_def_var;
   cls_traits_dtor_state                : function_def_var;
 }


 (*
  *
  *)
 and 'ast variable_record = {
   var_name             : string;
   mutable var_lifetime : Lifetime.t;
   mutable var_type     : 'ast type_info_t;
   mutable var_detail   : 'ast variable_record_var;
 }
 and 'ast variable_record_var =
   | VarRecordNormal of 'ast variable_record_normal
   | VarUndef

 and 'ast variable_record_normal = unit


let undef () =
  {
    env_id = undef_id;
    parent_env = None;
    context_env = None;
    module_env = None;
    er = Unknown;
    state = InComplete;
    closed = false;
    meta_level = Meta_level.Meta;
    rel_node = None;
    callee_when_exit = [];

    nest_level = NestLevel.zero;

    loc = None;
  }


let get_env_record env =
  let { er = er; _ } = env in
  er

let get_lookup_table env =
  match get_env_record env with
  | Root (r) -> r
  | Module (r, _) -> r
  | Function (r, _) -> r
  | Class (r, _) -> r
  | Scope (r) -> r
  | _ -> failwith "has no lookup table"

let get_symbol_table env =
  let lt = get_lookup_table env in
  lt.scope


let get_scope_lifetime ?(aux_count=0) env =
  let ctx_env = match env.context_env with
      Some e -> e
    | None -> failwith ""
  in
  Lifetime.LtDynamic (ctx_env.env_id, env.nest_level, aux_count)

let make_scope_lifetime ?(aux_count=0) env =
  get_scope_lifetime ~aux_count:aux_count env


let is_root e =
  let { er = er; _ } = e in
  match er with
    Root _  -> true
  | _       -> false

let get_parent_env e =
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
(*let rec lookup e name =
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
 *)
let rec kind_of_env e =
  match get_env_record e with
  | Root _ -> Kind.Other
  | Module _ -> Kind.Other
  | Function _ -> Kind.Function
  | Class _ -> Kind.Class
  | Scope _ -> Kind.Other
  | _ -> failwith ""


let rec lookup' ?(exclude=[]) e name acc =
  let skip e exk =
    if (kind_of_env e) = exk then
      if is_root e then e
      else (get_parent_env e)
    else e
  in
  let e = List.fold_left skip e exclude in
  let target = find_all_on_env e name in
  match target with
  | [] -> if is_root e then
            ([], e :: acc)
          else
            let penv = get_parent_env e in
            lookup' penv name (e :: acc)
  | xs -> (xs, e :: acc)

let rec lookup ?(exclude=[]) e name =
  let (res, history) = lookup' ~exclude:exclude e name [] in
  (res, List.rev history)

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

(* create an env as new context.
 * it will be used for functions, classes and so on... *)
let create_context_env parent_env er loc =
  let cur_id = generate_new_env_id () in

  let opt_mod_env = match parent_env.er with
    | Module _ -> Some parent_env
    | _ -> parent_env.module_env
  in
  let rec e = {
    env_id = cur_id;
    parent_env = Some parent_env;
    context_env = Some e;       (* self reference *)
    module_env = opt_mod_env;
    er = er;

    state = InComplete;
    closed = false;
    meta_level = Meta_level.Meta;
    rel_node = None;
    callee_when_exit = [];

    nest_level = NestLevel.add parent_env.nest_level NestLevel.one;

    loc = loc;
  } in
  e

(* create an env which is a part of the parent_env.
 * it will be used for block, if_scope and so on... *)
let create_scoped_env parent_env er loc =
  let cur_id = generate_new_env_id () in

  let opt_mod_env = match parent_env.er with
    | Module _ -> Some parent_env
    | _ -> parent_env.module_env
  in
  {
    env_id = cur_id;
    parent_env = Some parent_env;
    context_env = parent_env.context_env;
    module_env = opt_mod_env;
    er = er;

    state = InComplete;
    closed = false;
    meta_level = Meta_level.Meta;
    rel_node = None;
    callee_when_exit = [];

    nest_level = NestLevel.add parent_env.nest_level NestLevel.one;

    loc = loc;
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

(**)
let update_status e ns =
  e.state <- ns


(**)
let update_rel_ast e node =
  e.rel_node <- Some node

let get_rel_ast e =
  Option.get e.rel_node


(**)
let set_closed_flag env is_closed =
  env.closed <- is_closed

(**)
let update_meta_level e ml =
  e.meta_level <- ml


(**)
let make_root_env () =
  let tbl = empty_lookup_table () in
  let cur_id = generate_new_env_id () in
  {
    env_id = cur_id;
    parent_env = None;
    context_env = None;
    module_env = None;

    er = Root (tbl);
    state = Complete;
    closed = false;
    meta_level = Meta_level.Meta;
    rel_node = None;
    callee_when_exit = [];

    nest_level = NestLevel.zero;

    loc = None;
  }


(**)
let get_name env =
  let er = get_env_record env in
  match er with
  | Template (r) -> r.tl_name
  | Function (_, r) -> r.fn_name
  | Class (_, r) -> r.cls_name
  | _ -> failwith "get_name : not supported"

let append_callee_when_exit env node =
  (* LIFO *)
  env.callee_when_exit <- node :: env.callee_when_exit

let get_callee_funcs_when_scope_exit env =
  env.callee_when_exit

let get_callee_funcs_when_context_exit env =
  let ctx_env = match env.context_env with
    | Some c -> c
    | None -> failwith "[ICE] env must have context_env "
  in
  let rec collect env acc =
    let nodes = get_callee_funcs_when_scope_exit env in
    if env == ctx_env then
      nodes :: acc
    else
      let parent_env = match env.parent_env with
        | Some e -> e
        | None -> failwith ""
      in
      collect parent_env (nodes :: acc)
  in
  collect env [] |> List.rev |> List.flatten


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
    let find_or_add env id_name k =
      let name = Nodes.string_of_id_string id_name in
      let oe = find_on_env env name in
      match oe with
      (* Found *)
      | Some ({ er = (MultiSet { ms_kind = k; _ }); _} as e) ->
         (e, false)
      | Some _ -> failwith "[ERR] suitable multienv is not found"

      (* *)
      | None ->
         let er =
           MultiSet {
               ms_kind = k;
               ms_templates = [];
               ms_normal_instances = [];
               ms_template_instances = [];
               ms_instanced_args_cache_for_env = Hashtbl.create 0;
               ms_instanced_args_cache_for_node = Hashtbl.create 0;
             }
         in
         let e = create_scoped_env env er None in
         add_inner_env env name e;
         (e, true)

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

    let get_definition_status env =
      let er = get_record env in
      match er.fn_detail with
      | FnRecordNormal (stat, _, _) -> stat
      | FnRecordImplicit (stat, _, _) -> stat
      | FnRecordExternal (stat, _, _) -> stat
      | FnRecordBuiltin (stat, _, _) -> stat
      | _ -> failwith ""

    let is_trivial env =
      let stat = get_definition_status env in
      match stat with
      | FnDefDefaulted true -> true
      | _ -> false

    let get_kind env =
      let er = get_record env in
      match er.fn_detail with
      | FnRecordNormal (_, kind, _) -> kind
      | FnRecordImplicit (_, kind, _) -> kind
      | FnRecordExternal (_, kind, _) -> kind
      | FnRecordBuiltin (_, kind, _) -> kind
      | _ -> failwith ""

    let empty_record id_name =
      {
        fn_name = id_name;
        fn_mangled = None;
        fn_template_vals = [];
        fn_param_kinds = [];
        fn_return_type = Type_info.undef_ty;
        fn_is_auto_return_type = false;
        fn_detail = FnUndef;
      }

    let string_of_kind kind =
      match kind with
      | FnKindDefaultConstructor _ -> "default constructor"
      | FnKindCopyConstructor _ -> "copy constructor"
      | FnKindMoveConstructor _ -> "moce constructor"
      | FnKindConstructor _ -> "constructor"
      | FnKindDestructor _ -> "destructor"
      | FnKindFree -> "free"
      | FnKindMember -> "member"
  end


module ClassOp =
  struct
    let get_record env =
      let er = get_env_record env in
      match er with
      | Class (_, r) -> r
      | _ -> failwith "ClassOp.get_record : not class"

    let empty_record name =
      let default_traits = {
        cls_traits_is_primitive = false;
        cls_traits_is_always_value = false;
        cls_traits_has_user_defined_ctor = false;
        cls_traits_default_ctor_state = FnDefDefaulted true;
        cls_traits_copy_ctor_state = FnDefDefaulted true;
        cls_traits_dtor_state = FnDefDefaulted true;
      } in
      {
         cls_name = name;
         cls_mangled = None;
         cls_template_vals = [];
         cls_detail = ClsUndef;
         cls_traits = default_traits;
         cls_member_vars = [];
         cls_member_funcs = [];
         cls_size = None;
         cls_align = None;
         cls_default_ctor = None;
         cls_copy_ctor = None;
         cls_move_ctor = None;
         cls_dtor = None;
         cls_copy_assign = None;
         cls_move_assign = None;
      }

    let is_primitive env =
      let er = get_record env in
      er.cls_traits.cls_traits_is_primitive

    let push_member_variable env venv =
      let r = get_record env in
      r.cls_member_vars <- venv :: r.cls_member_vars

    let push_member_function env fenv =
      let r = get_record env in
      r.cls_member_funcs <- fenv :: r.cls_member_funcs
  end


module VariableOp =
  struct
    let get_record env =
      let er = get_env_record env in
      match er with
      | Variable (r) -> r
      | _ -> failwith "VariableOp.get_record : not variable"

    let empty_record name =
      {
        var_name = name;
        var_lifetime = Lifetime.LtUndef;
        var_type = Type_info.undef_ty;
        var_detail = VarUndef;
      }
  end



let debug_print env =
  let print_table tbl indent =
    let p name =
      Debug.printf "%s+ %s\n" indent name
    in
    tbl |> Hashtbl.iter @@ fun name _ -> p name
  in
  let print_ er f indent =
    let nindent = (indent ^ "  ") in
    match er with
    | Root (lt) ->
       begin
         Debug.printf "%sRootEnv\n" indent;
         print_table lt.scope indent;
         f nindent;
       end
    | Module (lt, r) ->
       begin
         Debug.printf "%sModuleEnv - %s\n" indent r.mod_name;
         print_table lt.scope indent;
         f nindent;
       end
    | Function (lt, r) ->
       begin
         let name = Nodes.string_of_id_string r.fn_name in
         Debug.printf "%sFunction - %s\n" indent name;
         print_table lt.scope indent;
         f nindent;
       end
    | Class (lt, r) ->
       begin
         let name = Nodes.string_of_id_string r.cls_name in
         Debug.printf "%sClassEnv - %s\n" indent name;
         print_table lt.scope indent;
         f nindent;
       end
    | Scope _ ->
       begin
         Debug.printf "%Scope\n" indent;
         f nindent
       end
    | MetaVariable uni_id ->
       begin
         Debug.printf "%sMetaVariable - %d\n" indent uni_id;
         f nindent
       end

    | MultiSet r ->
       begin
         Debug.printf "%sMultiSet - %s\n" indent (Kind.to_string r.ms_kind);
         f nindent;
       end

    | _ -> failwith "print: unsupported env"
  in
  let rec dig env f =
    if is_root env then begin
      let { er = er; _ } = env in
      print_ er f ""
    end else begin
      let pr = get_parent_env env in
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
