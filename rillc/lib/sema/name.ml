(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

type t = Typing.Type.t Path.t [@@deriving show]

let to_leyer env inv_subst : 'a Path.Name.t option =
  let (Typing.Scheme.ForAll { implicits; vars; ty }) = Env.type_sc_of env in
  let params = implicits @ vars in
  let generics_vars =
    List.map params ~f:(fun param -> Typing.Subst.subst_type inv_subst param)
  in

  let kind =
    match env.Env.kind with
    | Env.M -> Some Path.Name.Module
    | Env.Ty | Env.Impl | Env.Trait -> Some Path.Name.Type
    | Env.Val -> Some (Path.Name.Var (Typing.Pred.to_type ty))
    | Env.N | Env.KindScope | Env.Alias _ -> None
  in
  let has_self = env.Env.has_self in
  Option.map kind ~f:(fun k ->
      let l =
        Path.Name.{ name = env.Env.name; kind = k; generics_vars; has_self }
      in
      l)

let to_nested_chain' env subst : 'a Path.t =
  let rec f env leaf_layers =
    match env.Env.kind with
    | Env.Alias aenv -> f aenv leaf_layers
    | _ -> (
        let l_opt = to_leyer env subst in
        let leaf_layers =
          match l_opt with Some l -> l :: leaf_layers | None -> leaf_layers
        in
        match env.Env.parent with
        (* *)
        | None ->
            let pkg_tag =
              (* TODO *)
              Group.Pkg_tag.create ~name:"" ~version:""
            in
            Path.create ~pkg_tag leaf_layers
        (* *)
        | Some penv -> f penv leaf_layers )
  in
  f env []

let to_nested_chain env : 'a Path.t =
  let subst = Typing.Subst.create_generic () in
  to_nested_chain' env subst

let to_chains' env subst : 'a Path.t =
  match env.Env.lookup_space with
  | Env.LkLocal ->
      let l_opt = to_leyer env subst in
      let l = Option.value_exn ~message:"[ICE]" l_opt in

      let pkg_tag =
        (* TODO *)
        Group.Pkg_tag.create ~name:"" ~version:""
      in
      Path.create ~pkg_tag [ l ]
  | Env.LkGlobal -> to_nested_chain' env subst

let to_chains env : 'a Path.t =
  let subst = Typing.Subst.create_generic () in
  to_chains' env subst

let to_string ~to_s path = Path.to_string ~to_s path
