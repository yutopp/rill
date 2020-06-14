(*
 * Copyright yutopp 2019 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module Package = Common.Package

type t = {
  parent : t option;
  name : string;
  visibility : visibility_t;
  mutable scope : scope_t option;
  ty : Typing.Type.t;
  ty_w : wrap_t;
}

and scope_t = {
  values : ((string, t) Hashtbl.t[@printer fun fmt _ -> fprintf fmt ""]);
  types : ((string, t) Hashtbl.t[@printer fun fmt _ -> fprintf fmt ""]);
  meta : ((string, t) Hashtbl.t[@printer fun fmt _ -> fprintf fmt ""]);
}

and visibility_t = Public | Private

and wrap_t = N | Ty | Val of Typing.Type.t | M of Mod.t | Alias of t
[@@deriving show]

type inserted_status_t = InsertedNew | InsertedHiding

let create name ~parent ~visibility ~ty ~ty_w =
  { parent; name; visibility; scope = None; ty; ty_w }

let type_of env = env.ty

let rec w_of env = match env.ty_w with Alias aenv -> w_of aenv | _ -> env.ty_w

let mod_of env = match w_of env with M m -> m | _ -> failwith "[ICE]"

let rec root_mod_of ~scoped env =
  (* Do NOT take alias values *)
  match env.ty_w with
  | M m -> m
  | Alias aenv when not scoped -> root_mod_of ~scoped aenv
  | _ -> (
      match env.parent with
      | Some penv -> root_mod_of ~scoped penv
      | None -> failwith "[ICE] could not find root mod" )

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
  | Ty -> insert_type penv tenv
  | Val _ -> insert_value penv tenv
  | M _ -> insert_meta penv tenv
  | _ -> failwith "insert_impl"

let insert penv tenv = insert_impl penv (w_of tenv) tenv

let find_type env name =
  match env.scope with
  | Some scope -> Hashtbl.find scope.types name
  | None -> None

let collect_all env =
  let scope = assume_scope env in
  let envs_ty = Hashtbl.data scope.types in
  let envs_val = Hashtbl.data scope.values in
  let envs_meta = Hashtbl.data scope.meta in
  List.join [ envs_ty; envs_val; envs_meta ]

let collect_aliases env =
  let envs = collect_all env in
  List.filter envs ~f:(fun env ->
      match env.ty_w with Alias _ -> true | _ -> false)

let find_value env name =
  match env.scope with
  | Some scope -> Hashtbl.find scope.values name
  | None -> None

let find_meta env name =
  match env.scope with
  | Some scope -> Hashtbl.find scope.meta name
  | None -> None

let meta_keys env =
  match env.scope with Some scope -> Hashtbl.keys scope.meta | None -> []

let rec lookup_impl find env name =
  let rec lookup' env name history =
    match find env name with
    | Some e -> Ok e
    | None -> (
        match env.parent with
        | Some penv -> lookup' penv name (env :: history)
        | None -> Error (history |> List.rev) )
  in
  lookup' env name []

let lookup_type env name = lookup_impl find_type env name

let lookup_value env name = lookup_impl find_value env name
