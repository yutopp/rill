type checked_state =
    Incomplete
  | Checking
  | Complete

module Kind =
  struct
    type t =
        Module
      | Class
      | Function
      | MultiSet of t
  end

type env =
    Root of name_env_mapping
  | MultiSet of env * multiset_record

  | Module of env_common_record * module_record
  | Function of env_common_record

 and name_env_mapping = (string, env) Hashtbl.t

 and multiset_record = {
   ms_kind          : Kind.t;
   mutable ms_candidates    : env list
 }

 and env_common_record = {
   parent   : env;
   scope    : name_env_mapping
 }

 and module_record = {
   mod_name     : string;
 }


type t = env

let empty_table () =
  Hashtbl.create 10

let create_common parent_env =
  {
    parent = parent_env;
    scope = empty_table ();
  }

let create_root_env () =
  let tbl = empty_table () in
  Root (tbl)

let get_common_record e =
  match e with
  | Module (r, _) -> r
  | _ -> failwith ""

let get_symbol_table (e : env) : name_env_mapping =
  match e with
    Root (t) -> t
  | _ -> let r = get_common_record e in r.scope

let is_root e =
  match e with
    Root _ -> true
  | _ -> false

let parent_env e =
  if is_root e then
    failwith "root env has no parent env"
  else
    match e with
      MultiSet (penv, _) -> penv
    | _ -> let r = get_common_record e in r.parent

(* *)
let find e name =
  let t = get_symbol_table e in
  try
    Some (Hashtbl.find t name)
  with
    Not_found -> None

(*  *)
let rec lookup e name =
  let target = find e name in
  match target with
    Some _ as te -> te
  | None -> if is_root e then
              None
            else
              let penv = parent_env e in
              lookup penv name

(*  *)
let add (name : string) (e : env) (target_env : env) =
  let t = get_symbol_table target_env in
  Hashtbl.add t name e

let find_or_create_multi_env env name k =
  let oe = find env name in
  match oe with
  (* *)
  | Some ((MultiSet (_, {ms_kind = k; _})) as e) ->
     e

  (* *)
  | None ->
     MultiSet (env, {
                  ms_kind = k;
                  ms_candidates = [];
                })

  | _ -> failwith ""

module MultiSetOp =
  struct
    let add_candidates menv env =
      match menv with
      | MultiSet (_, record) ->
         begin
           (* add env to lists as candidates *)
           record.ms_candidates <- env :: record.ms_candidates;
           ()
         end
      | _ -> failwith ""
  end
