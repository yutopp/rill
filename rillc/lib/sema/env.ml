(*
 * Copyright yutopp 2019 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

type t = {
  parent : t option;
  name : string;
  visibility : visibility_t;
  mutable scope : scope_t option;
  mutable ty_sc : Typing.Scheme.t;
  mutable implicits : Typing.Type.t list;
  mutable predicates : Typing.Pred.cond_t list;
  trans : Trans.t;
  kind : kind_t;
  lookup_space : lookup_space_t;
  mutable deps : t list;
  mutable has_self : bool;
}

and scope_t = {
  values : ((string, t) Hashtbl.t[@printer fun fmt _ -> fprintf fmt ""]);
  types : ((string, t) Hashtbl.t[@printer fun fmt _ -> fprintf fmt ""]);
  meta : ((string, t) Hashtbl.t[@printer fun fmt _ -> fprintf fmt ""]);
}

and visibility_t = Public | Private

and kind_t = N | Ty | Val | M | Trait | Impl | Alias of t | KindScope

and lookup_space_t = LkGlobal | LkLocal [@@deriving show]

type namespace_t = NamespaceValue | NamespaceType | NamespaceMeta

type inserted_status_t = InsertedNew | InsertedHiding

let create name ~parent ~visibility ~ty_sc ~kind ~lookup_space =
  {
    parent;
    name;
    visibility;
    scope = None;
    ty_sc;
    implicits = [];
    predicates = [];
    trans = ();
    kind;
    lookup_space;
    deps = [];
    has_self = false;
  }

let name env = env.name

let update_ty_sc env ~ty_sc = env.ty_sc <- ty_sc

let type_sc_of env =
  let (Typing.Scheme.ForAll { implicits; vars; ty }) = env.ty_sc in
  let (Typing.Pred.Pred { ty; _ }) = ty in
  let ty = Typing.Pred.Pred { conds = env.predicates; ty } in
  Typing.Scheme.ForAll { implicits = env.implicits; vars; ty }

let predicates env = env.predicates

let append_implicits env implicits = env.implicits <- implicits @ env.implicits

let append_predicates env predicates =
  env.predicates <- predicates @ env.predicates

let set_has_self env flag = env.has_self <- flag

let has_self env = env.has_self

let rec w_of env = match env.kind with Alias aenv -> w_of aenv | _ -> env.kind

let register_deps_mod env dep_mod = env.deps <- dep_mod :: env.deps

let list_deps env = env.deps

let assume_scope env =
  match env.scope with
  | Some s -> s
  | None ->
      let s =
        {
          values = Hashtbl.create (module String);
          types = Hashtbl.create (module String);
          meta = Hashtbl.create (module String);
        }
      in
      env.scope <- Some s;
      s

let insert_with_hiding table ~key ~data =
  match Hashtbl.add table ~key ~data with
  | `Ok -> InsertedNew
  | `Duplicate ->
      Hashtbl.set table ~key ~data;
      InsertedHiding

let insert_type penv tenv =
  let scope = assume_scope penv in
  insert_with_hiding scope.types ~key:tenv.name ~data:tenv

let insert_value penv tenv =
  let scope = assume_scope penv in
  insert_with_hiding scope.values ~key:tenv.name ~data:tenv

let insert_meta penv tenv =
  let scope = assume_scope penv in
  insert_with_hiding scope.meta ~key:tenv.name ~data:tenv

let insert_impl penv w tenv =
  match w with
  | Ty | Trait -> insert_type penv tenv
  | Val -> insert_value penv tenv
  | M -> insert_meta penv tenv
  | _ -> failwith "insert_impl"

let insert penv tenv = insert_impl penv (w_of tenv) tenv

let collect_all env =
  let scope = assume_scope env in
  let envs_ty = Hashtbl.data scope.types in
  let envs_val = Hashtbl.data scope.values in
  let envs_meta = Hashtbl.data scope.meta in
  List.join [ envs_ty; envs_val; envs_meta ]

let collect_substances env =
  let envs = collect_all env in
  List.filter envs ~f:(fun env ->
      match env.kind with Alias _ -> false | _ -> true)

let find_value env name : t option =
  match env.scope with
  | Some scope -> Hashtbl.find scope.values name
  | None -> None

let find_type env name =
  match env.scope with
  | Some scope -> Hashtbl.find scope.types name
  | None -> None

let find_meta env name =
  match env.scope with
  | Some scope -> Hashtbl.find scope.meta name
  | None -> None

let find ~ns env name =
  match ns with
  | NamespaceValue -> find_value env name
  | NamespaceType -> find_type env name
  | NamespaceMeta -> find_meta env name

let find_multi env name =
  let nss = [ NamespaceValue; NamespaceType; NamespaceMeta ] in
  let envs =
    List.concat_map nss ~f:(fun ns -> find ~ns env name |> Option.to_list)
  in
  match envs with [] -> None | xs -> Some xs

let meta_keys env =
  match env.scope with Some scope -> Hashtbl.keys scope.meta | None -> []

let rec lookup_impl find_f env name =
  let rec lookup' env name history depth =
    match find_f env name with
    | Some e -> Ok (e, depth)
    | None -> (
        match env.parent with
        | Some penv ->
            let depth =
              let (major, minor) = depth in
              match env.kind with
              | KindScope -> (major, minor + 1)
              | _ -> (major + 1, minor)
            in
            lookup' penv name (env :: history) depth
        | None -> Error (history |> List.rev) )
  in
  lookup' env name [] (0, 0)

let lookup_multi env name = lookup_impl find_multi env name

let lookup_value env name = lookup_impl (find ~ns:NamespaceValue) env name

let lookup_type env name = lookup_impl (find ~ns:NamespaceType) env name

let lookup_meta env name = lookup_impl (find ~ns:NamespaceMeta) env name

let lookup_self_type env =
  let rec lookup' env history =
    match env.parent with
    | Some e -> (
        let (Typing.Scheme.ForAll { implicits; vars; ty }) = type_sc_of e in
        match e.kind with
        | Trait -> Ok e
        | Impl -> Ok e
        | _ -> lookup' e (env :: history) )
    | None -> Error (history |> List.rev)
  in
  lookup' env []
