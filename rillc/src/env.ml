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
  | InComplete
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
      | Function of Meta_level.t
      | Variable
      | Other

    let to_string = function
      | Module -> "module"
      | Class -> "class"
      | Function _ -> "function"
      | Variable -> "variable"
      | Other -> "other"
  end

type 'ast env_t = {
  env_id                        : EnvId.t;
  parent_env                    : 'ast env_t option;
  context_env                   : 'ast env_t;
  ns_env                        : 'ast env_t;
  module_env                    : 'ast env_t option;
  er                            : 'ast env_record_t;

  name                          : Id_string.t;
  mutable mangled_name          : string option;

  mutable state                 : checked_state_t;
  mutable closed                : bool;
  mutable generics_constraints  : Lifetime.constraint_t list;
  mutable meta_level            : Meta_level.t;
  mutable rel_node              : 'ast option;
  mutable callee_when_exit      : 'ast list;    (* TODO: rename *)
  nest_level                    : NestLevel.t;
  is_instantiated               : bool;

  (* information for error message *)
  loc                           : Loc.t;
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
   | LifetimeVariable of lifetime_record

   | Unknown

 and 'ast type_info_t = 'ast env_t Type_info.t
 and 'ast error_msg_t = ('ast type_info_t, 'ast env_t) Error_msg.t
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

   ms_instanced_args_pre_caches     : (string, ('ast env_t list * 'ast error_msg_t list)) Hashtbl.t;
 }


 and 'ast template_record = {
   tl_name          : Id_string.t;
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
   mutable fn_generics_vals         : Lifetime.t list;
   mutable fn_template_vals         : ('ast type_info_t Ctfe_value.t) list;

   mutable fn_param_kinds           : 'ast function_param_kind_t list;
   mutable fn_return_type           : 'ast type_info_t;
   mutable fn_is_auto_return_type   : bool;
   mutable fn_detail                : 'ast function_record_var;

   mutable fn_has_constraints       : bool;
   mutable fn_specialized_levels    : int list;
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
   fn_spec_param_envs     : 'ast env_t option list;
   fn_spec_force_inline   : bool;
 }


 (*
  *
  *)
 and 'ast class_record = {
   cls_name             : Id_string.t;
   mutable cls_generics_vals    : Lifetime.t list;
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
     var_name             : Id_string.t;
     mutable var_lifetime : Lifetime.t;
     mutable var_type     : 'ast type_info_t;
     mutable var_detail   : 'ast variable_record_var;
   }
 and 'ast variable_record_var =
   | VarRecordNormal of 'ast variable_record_normal
   | VarUndef

 and 'ast variable_record_normal = unit

 and lifetime_record = Lifetime.t


let undef () =
  let rec e = {
    env_id = EnvId.undef;
    parent_env = None;
    context_env = e;
    ns_env = e;
    module_env = None;
    er = Unknown;

    name = Id_string.Pure "<undef>";
    mangled_name = None;

    state = InComplete;
    closed = false;
    generics_constraints = [];
    meta_level = Meta_level.Meta;
    rel_node = None;
    callee_when_exit = [];

    nest_level = NestLevel.zero;
    is_instantiated = false;

    loc = None;
  } in
  e


let get_id env =
  env.env_id

let get_module_env env =
  env.module_env

let get_module_env_id env =
  env |> get_module_env |> Option.map get_id

let get_env_record env =
  let { er = er; _ } = env in
  er

let get_lookup_table env =
  let ns_env = env.ns_env in
  match get_env_record ns_env with
  | Root (r) -> r
  | Module (r, _) -> r
  | Function (r, _) -> r
  | Class (r, _) -> r
  | Scope (r) -> r
  | _ -> failwith "has no lookup table"

let get_symbol_table env =
  let lt = get_lookup_table env in
  lt.scope


let string_of_env_record env =
  let er = get_env_record env in
  match er with
  | Root _ -> "root"
  | MultiSet _ -> "multiset"
  | Template _ -> "template"
  | Module _ -> "module"
  | Function _ -> "function"
  | Class _ -> "class"
  | Variable _ -> "variable"
  | Scope _ -> "scope"
  | MetaVariable _ -> "meta_variable"
  | LifetimeVariable _ -> "lifetime_variable"
  | Unknown -> "unknown"

let get_name env =
  env.name

let get_scope_lifetime ?(aux_count=0) sub_nest env =
  let ctx_env = env.context_env in
  Lifetime.LtDynamic (ctx_env.env_id, env.nest_level, sub_nest, aux_count)

let is_closed e =
  e.closed

let overlap_closed_info closed env =
  env.closed <- env.closed || closed;
  env.ns_env.closed <- env.ns_env.closed || closed

let is_root e =
  let { er = er; _ } = e in
  match er with
    Root _  -> true
  | _       -> false

let get_parent_env_opt e =
  let { parent_env = opt_penv } = e in
  opt_penv

let get_parent_env e =
  if is_root e then
    failwith "root env has no parent env"
  else
    match get_parent_env_opt e with
      Some penv -> penv
    | None -> failwith ""

let rec kind_of_env e =
  match get_env_record e with
  | Root _ -> Kind.Other
  | Module _ -> Kind.Other
  | Function _ -> Kind.Function e.meta_level
  | Class _ -> Kind.Class
  | Scope _ -> Kind.Other
  | _ -> failwith ""

module EnvSet = Set.Make(EnvId)

(* find symbol only on the env *)
let find_on_env e id_name =
  let lt = get_lookup_table e in
  Hashtbl.find_option lt.scope (Id_string.to_string id_name)

(* find symbol only on the env and imports *)
let rec find_all_on_env ?(checked_env=EnvSet.empty) env id_name =
  let ns_env = env.ns_env in
  if EnvSet.mem ns_env.env_id checked_env then
    failwith "[ERR] recursive package search";

  (**)
  let envs =
    match find_on_env ns_env id_name with
    | Some e -> [e]
    | None -> []
  in

  (* find some envs from import tables *)
  let external_envs =
    let search_env_in_module ?(opt_force_priv=None) envs (mod_env, priv) =
      let m_priv = Option.default priv opt_force_priv in
      Debug.printf "-> %s / %s (%s)"
                   (Id_string.to_string (get_name mod_env))
                   (match priv with
                    | ModPrivate -> "private"
                    | ModPublic -> "public")
                   (match m_priv with
                    | ModPrivate -> "private"
                    | ModPublic -> "public");
      match m_priv with
      | ModPrivate ->
         (* if the module is private, find symbols from only the imported module *)
         begin
           match find_on_env mod_env id_name with
           | Some e -> e :: envs
           | None -> envs
         end
      | ModPublic ->
         (* if the module is public, find symbols recursively in the module *)
         let res =
           find_all_on_env ~checked_env:(EnvSet.add ns_env.env_id checked_env)
                           mod_env id_name
         in
         res @ envs
    in
    let search_env_in_module_top envs (mod_env, priv) =
      search_env_in_module ~opt_force_priv:(Some ModPublic)
                           envs (mod_env, priv)
    in
    let lt = get_lookup_table ns_env in
    List.fold_left search_env_in_module_top [] lt.imported_mods
  in
  let target_envs = envs @ external_envs in
  (* remove duplicates *)
  List.sort_unique (fun a b -> compare (get_id a) (get_id b)) target_envs

(* find symbol recursively *)
let rec lookup' ?(exclude=[]) env id_name acc =
  let ns_env = env.ns_env in
  let skip e exk =
    if (kind_of_env e) = exk then
      if is_root e then e
      else (get_parent_env e)
    else e
  in
  let e = List.fold_left skip ns_env exclude in
  let target = find_all_on_env e id_name in
  match target with
  | [] -> if is_root e then
            ([], e :: acc)
          else
            let penv = get_parent_env e in
            lookup' ~exclude:exclude penv id_name (e :: acc)
  | xs -> (xs, e :: acc)

let rec lookup ?(exclude=[]) e id_name =
  let (res, history) = lookup' ~exclude:exclude e id_name [] in
  (res, List.rev history)


(*  *)
type dup_check_t =
  | DupCheckNone
  | DupCheckOnEnv
  | DupCheckDeep

let add_inner_env_with_callback ~dup_check ~f target_env id_name =
  let checked_dup =
    match dup_check with
    | DupCheckNone ->
       None
    | DupCheckOnEnv ->
       find_on_env target_env id_name
    | DupCheckDeep ->
       None (* TODO: implement *)
  in
  match checked_dup with
  | None ->
     let env = f () in
     let sym_tbl = get_symbol_table target_env in
     Hashtbl.add sym_tbl (Id_string.to_string id_name) env;
     Ok ()
  | Some e ->
     Bad (`Duplicated e)

(* Add a env 'e' which is named 'name' to the env 'target_env',
   if result of dup_check is Ok.

   'dup_check' is a flag to check duplication.
   DupCheckNone -> always returns 'Ok ()'
   DupCheckOnEnv -> if a env which has a name same as given 'name' is found on
                    'target_env', returns 'Bad (`Duplicated found_env)'.
                    Otherwise, returns 'Ok ()'.
   DupCheckDeep -> if a env which has a name same as given 'name' is found on
                   'target_env' and parents of it, returns 'Bad (`Duplicated found_env)'.
                   Otherwise, returns 'Ok ()'.
 *)
let add_inner_env ?(dup_check=DupCheckOnEnv) target_env id_name e =
  add_inner_env_with_callback ~dup_check:dup_check
                              ~f: (fun () -> e)
                              target_env id_name

let import_module ~privacy env mod_env =
  let lt = get_lookup_table env in
  lt.imported_mods <- (mod_env, privacy) :: lt.imported_mods


let empty_lookup_table ?(init=8) () =
  {
    scope = Hashtbl.create init;
    imported_mods = [];
  }

let calc_nest_level ?(ext_nest_level=None) env =
  NestLevel.add env.nest_level
                (Option.default (NestLevel.zero) ext_nest_level)

(* create an env as new context.
 * it will be used for functions, classes and so on... *)
let create_context_env ?(is_instantiated=false)
                       ?(ext_nest_level=None)
                       ?(meta_level=Meta_level.Meta)
                       parent_env name er loc =
  let cur_id = EnvUniqId.generate () in

  let opt_mod_env = match parent_env.er with
    | Module _ -> Some parent_env
    | _ -> parent_env.module_env
  in
  let rec e = {
    env_id = EnvId.E (cur_id, Some parent_env.env_id);
    parent_env = Some parent_env;
    context_env = e;    (* self reference *)
    ns_env = e;         (* self reference *)
    module_env = opt_mod_env;
    er = er;

    name = name;
    mangled_name = None;

    state = InComplete;
    closed = false;
    generics_constraints = [];
    meta_level = meta_level;
    rel_node = None;
    callee_when_exit = [];

    nest_level = NestLevel.add (NestLevel.add parent_env.nest_level NestLevel.one)
                               (Option.default (NestLevel.zero) ext_nest_level);
    is_instantiated = is_instantiated;

    loc = loc;
  } in
  e

(* create an env which is a part of the parent_env.
 * it will be used for block, if_scope and so on... *)
let create_scoped_env ?(has_ns=true)
                      ?(is_instantiated=false)
                      ?(ext_nest_level=None)
                      parent_env name er loc =
  let cur_id = EnvUniqId.generate () in

  let opt_mod_env = match parent_env.er with
    | Module _ -> Some parent_env
    | _ -> parent_env.module_env
  in
  let rec e = {
    env_id = EnvId.E (cur_id, Some parent_env.env_id);
    parent_env = Some parent_env;
    context_env = parent_env.context_env;
    ns_env = e; (* self reference *)
    module_env = opt_mod_env;
    er = er;

    name = name;
    mangled_name = None;

    state = InComplete;
    closed = parent_env.closed;
    generics_constraints = [];
    meta_level = Meta_level.Meta;
    rel_node = None;
    callee_when_exit = [];

    nest_level = NestLevel.add (NestLevel.add parent_env.nest_level NestLevel.one)
                               (Option.default (NestLevel.zero) ext_nest_level);
    is_instantiated = is_instantiated;

    loc = loc;
  } in
  if has_ns then e else { e with ns_env = parent_env.ns_env }

(**)
let is_checked e =
  let { state = s; _ } = e in
  s = Checking || s = Complete

let is_complete e =
  let { state = s; _ } = e in
  s = Complete

let is_incomplete e =
  not (is_complete e)

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
  let cur_id = EnvUniqId.generate () in
  let rec e = {
    env_id = EnvId.E (cur_id, None);
    parent_env = None;
    context_env = e;        (* self reference *)
    ns_env = e;             (* self reference *)
    module_env = None;
    er = Root (tbl);

    name = Id_string.Pure "<root>";
    mangled_name = None;

    state = Complete;
    closed = false;
    generics_constraints = [];
    meta_level = Meta_level.Meta;
    rel_node = None;
    callee_when_exit = [];

    nest_level = NestLevel.zero;
    is_instantiated = false;

    loc = None;
  } in
  e



(**)
let get_generics_vals env =
  let er = get_env_record env in
  match er with
  | Function (_, r) -> r.fn_generics_vals
  | Class (_, r) -> r.cls_generics_vals
  | _ -> []

let append_callee_when_exit env node =
  (* LIFO *)
  env.callee_when_exit <- node :: env.callee_when_exit

let get_callee_funcs_when_scope_exit env =
  env.callee_when_exit

let get_callee_funcs_when_context_exit env =
  let ctx_env = env.context_env in
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

let collect_dependee_envs env =
  let rec collect env acc =
    let nacc = env :: acc in
    match env.parent_env with
    | Some c -> collect c nacc
    | None -> nacc
  in
  collect env [] |> List.rev

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
    (* return (env, false) if env is found. otherwise return (new_env, true) *)
    let find_or_add env id_name k =
      let oe = find_on_env env id_name in
      match oe with
      (* Found *)
      | Some ({ er = (MultiSet { ms_kind = k; _ }); _} as e) ->
         (e, false)
      | Some _ ->
         failwith "[ERR] suitable multienv is not found"

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
               ms_instanced_args_pre_caches = Hashtbl.create 0;
             }
         in
         let e = create_scoped_env env id_name er None in
         let _ = add_inner_env env id_name e in
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
        fn_generics_vals = [];
        fn_template_vals = [];
        fn_param_kinds = [];
        fn_return_type = Type_info.undef_ty;
        fn_is_auto_return_type = false;
        fn_detail = FnUndef;
        fn_has_constraints = false;
        fn_specialized_levels = [];
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

    let update_template_specs env ~has_constraints ~specialized_levels =
      let er = get_record env in
      er.fn_has_constraints <- has_constraints;
      er.fn_specialized_levels <- specialized_levels

    let has_constraints env =
      let er = get_record env in
      er.fn_has_constraints

    let specialized_levels env =
      let er = get_record env in
      er.fn_specialized_levels
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
         cls_generics_vals = [];
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

module LifetimeVariableOp =
  struct
    let as_lifetime env =
      let er = get_env_record env in
      match er with
      | LifetimeVariable lt -> lt
      | _ -> failwith ""
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
         let name = get_name env |> Id_string.to_string in
         Debug.printf "%sFunction - %s\n" indent name;
         print_table lt.scope indent;
         f nindent;
       end
    | Class (lt, r) ->
       begin
         let name = Id_string.to_string r.cls_name in
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
    | Unknown ->
       Debug.printf "%sUnknown\n" indent;
       f nindent;

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
  match e.module_env with
  | Some mod_env ->
     let mr = ModuleOp.get_record mod_env in
     mr.mod_pkg_names @ [mr.mod_name]
  | _ ->
     (* TODO: fix *)
     ["<tmp>"]
