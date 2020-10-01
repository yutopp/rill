(*
 * Copyright yutopp 2018 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module Span = Common.Span
module Type = Typing.Type

type t = {
  kind : value_kind_t;
  ty : (Type.t[@printer fun fmt _ -> fprintf fmt ""]);
  span : (Span.t[@printer fun fmt _ -> fprintf fmt ""]);
}

and value_kind_t =
  | Call of placeholder_t * placeholder_t list
  | Cast of placeholder_t
  | Index of placeholder_t * placeholder_t
  | Ref of placeholder_t
  | Deref of placeholder_t
  | Construct
  | RVal of value_r_t
  | LVal of placeholder_t
  | Undef

and value_r_t =
  | ValueBool of bool
  | ValueInt of int
  | ValueString of string
  | ValueUnit
  | ValueArrayElem of placeholder_t list

and inst_t =
  | Let of string * t * Typing.Type.mutability_t
  | Assign of { lhs : t; rhs : t }
  | TerminatorPoint of terminator_t

and terminator_t =
  | Jump of string
  | Cond of placeholder_t * string * string
  | Ret

and placeholder_t =
  | PlaceholderVar of { name : string }
  | PlaceholderParam of { index : int; name : string }
  | PlaceholderGlobal of { name : string }
  | PlaceholderGlobal2 of {
      mutable name : Typing.Type.t Common.Chain.Nest.t;
      mutable dispatch : bool;
    }
[@@deriving show]

type alloc_t = AllocLit | AllocStack [@@deriving show]

let to_string_place_holder holder =
  match holder with
  | PlaceholderVar { name } -> Printf.sprintf "%%%s" name
  | PlaceholderParam { name; index } -> Printf.sprintf "Param[%s;%d]" name index
  | PlaceholderGlobal { name } -> Printf.sprintf "Global[%s]" name
  | PlaceholderGlobal2 { name; dispatch } ->
      Printf.sprintf "Global2[%s](%b)"
        (Common.Chain.Nest.to_string ~to_s:Typing.Type.to_string name)
        dispatch

let to_string_value value =
  match value with
  | ValueBool b -> Bool.to_string b
  | ValueInt i -> Int.to_string i
  | ValueString s -> String.to_string s
  | ValueUnit -> "()"
  | ValueArrayElem elems ->
      let elems = List.map elems ~f:to_string_place_holder in
      Printf.sprintf "[%s]" (String.concat ~sep:", " elems)

let to_string_term term =
  match term with
  | { kind = Call (recv, args); _ } ->
      let recv = to_string_place_holder recv in
      let args = List.map args ~f:to_string_place_holder in
      Printf.sprintf "call %s (%s)" recv (String.concat ~sep:", " args)
  | { kind = Cast recv; _ } ->
      let recv = to_string_place_holder recv in
      Printf.sprintf "cast %s" recv
  | { kind = Index (elems, index); _ } ->
      let elems = to_string_place_holder elems in
      let index = to_string_place_holder index in
      Printf.sprintf "%s[%s]" elems index
  | { kind = Ref elem; _ } ->
      let elem = to_string_place_holder elem in
      Printf.sprintf "&%s" elem
  | { kind = Deref elem; _ } ->
      let elem = to_string_place_holder elem in
      Printf.sprintf "*%s" elem
  | { kind = Construct; _ } -> Printf.sprintf "construct"
  | { kind = RVal value; _ } ->
      Printf.sprintf "rval(%s)" (to_string_value value)
  | { kind = LVal var; _ } ->
      let var = to_string_place_holder var in
      Printf.sprintf "lval(%s)" var
  | { kind = Undef; _ } -> "undef"

let to_string_termi ~indent termi =
  let buf = Buffer.create 64 in
  Buffer.add_string buf (String.make indent ' ');
  let () =
    match termi with
    | Jump label -> Buffer.add_string buf (Printf.sprintf "jump %s" label)
    | Cond (cond, t_label, e_label) ->
        let cond = to_string_place_holder cond in
        Buffer.add_string buf
          (Printf.sprintf "cond %s, then=%s, else=%s" cond t_label e_label)
    | Ret -> Buffer.add_string buf "ret"
  in
  Buffer.contents buf

let to_string_inst ~indent inst =
  let buf = Buffer.create 64 in
  Buffer.add_string buf (String.make indent ' ');
  let () =
    match inst with
    | Let (name, ({ ty; _ } as term), mut) ->
        Buffer.add_string buf
          (Printf.sprintf "let %s %s : %s = %s"
             (Typing.Type.to_string_mut mut)
             name (Typing.Type.to_string ty) (to_string_term term))
    | Assign { lhs; rhs } ->
        Buffer.add_string buf
          (Printf.sprintf "assign %s = %s" (to_string_term lhs)
             (to_string_term rhs))
    | TerminatorPoint termi ->
        Buffer.add_string buf
          (Printf.sprintf "point %s" (to_string_termi ~indent:0 termi))
  in
  Buffer.contents buf

module BB = struct
  type t = {
    name : string;
    mutable insts_rev : inst_t list;
    mutable terminator : terminator_t option;
  }
  [@@deriving show]

  let create name : t = { name; insts_rev = []; terminator = None }

  let append_inst bb inst =
    bb.insts_rev <- inst :: bb.insts_rev;
    match inst with
    | TerminatorPoint termi when Option.is_none bb.terminator ->
        (* memoize first terminator *)
        bb.terminator <- Some termi
    | _ -> ()

  let get_insts bb = List.rev bb.insts_rev

  let get_terminator_opt bb = bb.terminator

  let get_successors bb =
    let t = get_terminator_opt bb in
    match t with
    | Some (Jump n) -> [ n ]
    | Some (Cond (_, t, e)) -> [ t; e ]
    | Some Ret -> []
    | None -> []

  let to_string ~indent bb =
    let buf = Buffer.create 256 in
    Buffer.add_string buf (String.make indent ' ');

    Buffer.add_string buf (Printf.sprintf "BB: name=%s\n" bb.name);

    let insts = get_insts bb in
    List.iter insts ~f:(fun inst ->
        let s = to_string_inst ~indent:(indent + 2) inst in
        Buffer.add_string buf s;
        Buffer.add_char buf '\n');

    let () =
      let termi = get_terminator_opt bb in
      let s =
        Option.value_map termi ~default:"NONE"
          ~f:(to_string_termi ~indent:(indent + 2))
      in
      Buffer.add_string buf s;
      Buffer.add_char buf '\n'
    in

    Buffer.contents buf
end
