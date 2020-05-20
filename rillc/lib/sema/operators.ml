(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module Ast = Syntax.Ast
module Span = Common.Span

module Assoc = struct
  type t = { precedence : int; assoc : assoc_t }

  and assoc_t = Left | Right
end

type oo = Bottom | Op of (string * Ast.t * Span.t) | Top of Ast.t
[@@deriving show]

let sss o = match o with Bottom -> "$" | Op (tok, _, _) -> tok | Top _ -> "V"

module AssocTable = struct
  type t = { table : (string, Assoc.t) Hashtbl.t }

  let create () : t = { table = Hashtbl.create (module String) }

  let set t op p = Hashtbl.set t.table ~key:op ~data:p

  let preority_in t op =
    match op with
    | Bottom -> 0
    | Top _ -> 999
    | Op (tok, _, _) -> (
        let assoc = Hashtbl.find_exn t.table tok in
        match assoc.Assoc.assoc with
        | Assoc.Left -> assoc.Assoc.precedence
        | Assoc.Right -> assoc.Assoc.precedence )

  let preority_st t op =
    match op with
    | Bottom -> 0
    | Top _ -> 999
    | Op (tok, _, _) -> (
        let assoc = Hashtbl.find_exn t.table tok in
        match assoc.Assoc.assoc with
        | Assoc.Left -> assoc.Assoc.precedence + 1
        | Assoc.Right -> assoc.Assoc.precedence - 1 )
end

type t = { bin_op : AssocTable.t }

let preset : t =
  let bin_op =
    let t = AssocTable.create () in
    AssocTable.set t "*" Assoc.{ precedence = 110; assoc = Left };
    AssocTable.set t "+" Assoc.{ precedence = 100; assoc = Left };
    AssocTable.set t "-" Assoc.{ precedence = 100; assoc = Left };
    AssocTable.set t "==" Assoc.{ precedence = 40; assoc = Left };
    AssocTable.set t "=" Assoc.{ precedence = 10; assoc = Right };
    t
  in
  { bin_op }

(* Assume that all bin op is a left associated unbalanced tree *)
let rec reconstruct ast =
  match ast with
  | Ast.{ kind = ExprGrouping expr; _ } ->
      let nodes = flatten expr [] in
      let nodes = reorder ~ops:preset nodes in

      let stack = Stack.create () in
      let rec f nodes =
        match nodes with
        | x :: xs ->
            let a =
              match x with
              | Top s ->
                  Stack.push stack s;
                  f xs
              | Op (_, op, span) ->
                  let rhs = Stack.pop_exn stack in
                  let lhs = Stack.pop_exn stack in
                  let e = Ast.{ kind = ExprBinaryOp { op; lhs; rhs }; span } in
                  Stack.push stack e;
                  f xs
              | _ -> ()
            in
            a
        | [] -> ()
      in
      f (List.rev nodes);
      Stack.pop_exn stack
  | _ -> failwith ""

(* e.g. <<1 + 2> * 3> [] -> <1 + 2> [*; 3] -> 1 [+; 2; *; 3] *)
and flatten ast elems =
  match ast with
  | Ast.{ kind = ExprBinaryOp { op; lhs; rhs }; span; _ } ->
      let tok =
        match op with Ast.{ kind = ID tok; _ } -> tok | _ -> failwith ""
      in
      flatten lhs (Op (tok, op, span) :: Top rhs :: elems)
  | lhs -> Top lhs :: elems

and reorder ~ops nodes =
  let rec f nodes stack output =
    let (stack_top, stack_rest) =
      match stack with x :: xs -> (x, xs) | [] -> failwith ""
    in
    match nodes with
    | x :: xs ->
        let p_in = AssocTable.preority_in ops.bin_op x in
        let p_st = AssocTable.preority_st ops.bin_op stack_top in
        (*[%loga.debug "%s(%d) > %s(%d)" (sss x) p_in (sss stack_top) p_st];*)
        if p_in > p_st then (* *)
          f xs (x :: stack) output
        else (* *)
          f nodes stack_rest (stack_top :: output)
    | [] ->
        let rec app stack output =
          match stack with x :: xs -> app xs (x :: output) | [] -> output
        in
        app stack output
  in
  let stack = [ Bottom ] in
  let output = f nodes stack [] in
  output
