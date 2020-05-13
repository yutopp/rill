(*
 * Copyright yutopp 2019 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

type t = {
  parent : t option; [@sexp.opaque]
  name : string;
  mutable scope : scope_t option;
  ty_w : wrap_t;
}

and scope_t = {
  values : (string, t) Hashtbl.t; [@sexp.opaque]
  types : (string, t) Hashtbl.t; [@sexp.opaque]
}

and wrap_t = T of Typing.Type.t [@@deriving sexp_of]

let create name ~parent ~ty_w = { parent; name; scope = None; ty_w }

let rec type_of env = match env.ty_w with T ty -> ty

let assume_scope env =
  match env.scope with
  | Some s -> s
  | None ->
      let s =
        {
          values = Hashtbl.create (module String);
          types = Hashtbl.create (module String);
        }
      in
      env.scope <- Some s;
      s

let insert_type penv tenv =
  let scope = assume_scope penv in
  let _ = Hashtbl.add scope.types ~key:tenv.name ~data:tenv in
  ()

let insert_value penv tenv =
  let scope = assume_scope penv in
  let _ = Hashtbl.add scope.values ~key:tenv.name ~data:tenv in
  ()

let find_type env name =
  match env.scope with
  | Some scope -> Hashtbl.find scope.types name
  | None -> None

let find_value env name =
  match env.scope with
  | Some scope -> Hashtbl.find scope.values name
  | None -> None

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
