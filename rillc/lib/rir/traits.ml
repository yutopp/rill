(*
 * Copyright yutopp 2018 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module StringMap = Map.M (String)

module Impl = struct
  type t = {
    name : Typing.Type.t Path.t;
    mutable members :
      (Typing.Type.t Path.t StringMap.t
      [@printer fun fmt _ -> fprintf fmt ""]);
  }
  [@@deriving show]

  let create ~name =
    let members = Map.empty (module String) in
    { name; members }

  let add_member trait tag name =
    let { members; _ } = trait in
    let tag = Symbol.to_generic_layer tag in
    let members = Map.add_exn members ~key:tag ~data:name in
    trait.members <- members;
    ()

  let find impl name =
    let { members; _ } = impl in
    let id = Symbol.to_generic_layer name in
    Map.find_exn members id

  let to_string ~indent trait =
    let buf = Buffer.create 256 in
    Buffer.add_string buf (String.make indent ' ');

    let { name; members; _ } = trait in
    Buffer.add_string buf
      (Printf.sprintf "Impl: name = '%s'\n"
         (Path.to_string ~to_s:Typing.Type.to_string name));

    Map.iteri members ~f:(fun ~key ~data ->
        Buffer.add_string buf (String.make (indent + 2) ' ');
        Buffer.add_string buf
          (Printf.sprintf "%s -> '%s'\n" key
             (Path.to_string ~to_s:Typing.Type.to_string data)));

    Buffer.contents buf
end

type t = {
  (* name of trait *)
  name : Typing.Type.t Path.t;
  (* impl for *)
  mutable impls : (Impl.t StringMap.t[@printer fun fmt _ -> fprintf fmt ""]);
}
[@@deriving show]

let create ~name =
  let impls = Map.empty (module String) in
  { name; impls }

let add_impl trait impl =
  let { impls; _ } = trait in
  let id = Symbol.to_signatured_id impl.Impl.name in

  let impls = Map.add_exn impls ~key:id ~data:impl in
  trait.impls <- impls;

  ()

let find trait name =
  let { impls; _ } = trait in
  let id = Symbol.to_signatured_id' name in
  Map.find_exn impls id

let to_string ~indent trait =
  let buf = Buffer.create 256 in
  Buffer.add_string buf (String.make indent ' ');

  let { name; impls; _ } = trait in
  Buffer.add_string buf
    (Printf.sprintf "Trait: name = '%s'\n"
       (Path.to_string ~to_s:Typing.Type.to_string name));

  Map.iteri impls ~f:(fun ~key ~data ->
      Buffer.add_string buf (Impl.to_string ~indent:(indent + 2) data));

  Buffer.contents buf
