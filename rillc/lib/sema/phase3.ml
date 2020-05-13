(*
 * Copyright yutopp 2018 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module Span = Common.Span
module Diagnostics = Common.Diagnostics
module TAst = Phase2.TAst

module NAst = struct
  type t = {
    kind : kind_t;
    ty : (Typing.Type.t[@printer fun fmt _ -> fprintf fmt ""]);
    span : (Span.t[@sexp.opaque] [@printer fun fmt _ -> fprintf fmt ""]);
  }

  and kind_t =
    | Module of t list
    | Func of { name : string; kind : func_kind_t }
    | Let of { name : string; expr : t }
    | Return of string
    | Call of { name : string; args : string list }
    | LitBool of bool
    | LitInt of { value : int; bits : int; signed : bool }
    | LitString of string
    | LitUnit
    | ID of string
    | Seq of t list

  and func_kind_t = FuncKindDecl | FuncKindDef of t | FuncKindExtern of string
  [@@deriving sexp_of, to_yojson, show]
end

type ctx_t = { ds : Diagnostics.t; subst : Typing.Subst.t }

let context ~ds ~subst = { ds; subst }

let fresh_id =
  let i = ref 0 in
  let f () =
    let v = !i in
    i := v + 1;
    Printf.sprintf "%d" v
  in
  f

(*
> let k = insert_let (analyze "1 + 2 + 3"); k (fun id -> T(id))

> let k = insert_let {
    let k = insert_let (analyze "1 + 2"); k (fun id -> BinOp("+", id, "3"))
  }; k (fun id -> T(id))

> let k = insert_let {
    let k = insert_let BinOp("+", I(1), I(2)); k (fun id -> BinOp("+", id, "3"))
  }; k (fun id -> T(id))

> let k = insert_let {
    let k gen =
      let fresh_id = fresh();
      let let_stmt = Let(fresh_id, BinOp("+", I(1), I(2))
      let next = gen fresh_id in
      Seq [ let_stmt; next]
    in
    k (fun id -> BinOp("+", id, "3"))
  }; k (fun id -> T(id))

> let k = insert_let {
    Seq [ let_stmt; BinOp("+", id, "3")]
  }; k (fun id -> T(id))

let k = insert_let (let k = insert_let (analyze "1 + 2"); k (fun id -> id + "3"))

*)
let insert_let k_form gen =
  match k_form with
  | NAst.{ kind = ID id; _ } -> gen id
  | NAst.{ span; ty; _ } -> (
      let new_id = fresh_id () in
      let let_stmt =
        NAst.{ kind = Let { name = new_id; expr = k_form }; ty; span }
      in
      match gen new_id with
      | NAst.{ kind = Seq nodes; ty; span } ->
          NAst.{ kind = Seq (let_stmt :: nodes); ty; span }
      | NAst.{ ty; span; _ } as node ->
          NAst.{ kind = Seq [ let_stmt; node ]; ty; span } )

(* Currently K-normalize *)
let rec normalize ~ctx ast =
  match ast with
  (* *)
  | TAst.{ kind = Module nodes; ty; span; _ } ->
      let nodes' = List.map nodes ~f:(normalize ~ctx) in
      NAst.{ kind = Module nodes'; ty; span }
  (* *)
  | TAst.{ kind = DeclExternFunc { name; extern_name }; ty; span; _ } ->
      NAst.{ kind = Func { name; kind = FuncKindExtern extern_name }; ty; span }
  (* *)
  | TAst.{ kind = DefFunc { name; body }; ty; span; _ } ->
      let body' = normalize ~ctx body in
      NAst.{ kind = Func { name; kind = FuncKindDef body' }; ty; span }
  (* *)
  | TAst.{ kind = StmtSeq nodes; ty; span; _ } ->
      let node' = List.map nodes ~f:(normalize ~ctx) in
      NAst.{ kind = Seq node'; ty; span }
  (* *)
  | TAst.{ kind = StmtExpr expr; ty; span; _ } ->
      let k = insert_let (normalize ~ctx expr) in
      (* TODO: fix *)
      k (fun _id -> NAst.{ kind = LitUnit; ty; span })
  (* *)
  | TAst.{ kind = ExprCall (r, args); ty; span; _ } ->
      let rk = insert_let (normalize ~ctx r) in
      let rec bind xs args k =
        match args with
        | [] ->
            k (fun r' ->
                NAst.{ kind = Call { name = r'; args = List.rev xs }; ty; span })
        | a :: args ->
            let k' = insert_let (normalize ~ctx a) in
            k' (fun id -> bind (id :: xs) args k)
      in
      bind [] args rk
  (* *)
  | TAst.{ kind = ID s; ty; span; _ } -> NAst.{ kind = ID s; ty; span }
  (* *)
  | TAst.{ kind = LitString v; ty; span; _ } ->
      NAst.{ kind = LitString v; ty; span }
  (* *)
  | TAst.{ kind; _ } ->
      let s = TAst.sexp_of_kind_t kind |> Sexp.to_string_mach in
      failwith (Printf.sprintf "Not supported stmt/expr node (phase3): %s" s)
