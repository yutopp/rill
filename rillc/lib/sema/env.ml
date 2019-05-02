(*
 * Copyright yutopp 2019 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

module Kind = struct
  type t =
    | Package
    | Module
    | Function
    | Var
    | Type of Type.t
    | Scope
end

type t = {
  parent: t option;
  name: string;
  table: (string, t) Hashtbl.t;
  kind: Kind.t;
  tysc: Type.Scheme.t option;
}

let create ?tysc name k p  =
  {
    parent = p;
    name = name;
    table = Hashtbl.create (module String);
    kind = k;
    tysc = tysc;
  }

let insert penv c =
  (* TODO: check duplication *)
  let _ = Hashtbl.add penv.table ~key:c.name ~data:c in
  penv

let delete penv name =
  (* TODO: check existance *)
  let _ = Hashtbl.remove penv.table name in
  penv

let find env name =
  Hashtbl.find env.table name

let rec lookup env name =
  let rec lookup' env name history =
    match find env name with
    | Some e -> Ok e
    | None ->
       begin match env.parent with
       | Some penv -> lookup' penv name (env :: history)
       | None -> Error (history |> List.rev)
       end
  in
  lookup' env name []

let show env subst =
  let s = match env.tysc with
    | Some tysc ->
       let tysc' = Typer.subst_tysc subst tysc in
       let s = Type.Scheme.sexp_of_t tysc' in
       Sexp.to_string_hum s
    | None ->
       "<NONE>"
  in
  Stdio.printf "Env: ty = %s\n" s
