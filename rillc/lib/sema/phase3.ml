(*
 * Copyright yutopp 2018 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module Span = Common.Span
module TAst = Phase2.TAst
module StringMap = Map.M (String)

module NAst = struct
  type t = {
    kind : kind_t;
    ty : (Typing.Pred.t[@printer fun fmt _ -> fprintf fmt ""]);
    span : (Span.t[@sexp.opaque] [@printer fun fmt _ -> fprintf fmt ""]);
  }

  and kind_t =
    | Module of { nodes : t }
    | Import of { pkg : string; mods : string list }
    | Func of {
        name : Typing.Type.t Path.t;
        kind : func_kind_t;
        ty_sc : Typing.Scheme.t;
      }
    | Static of {
        name : Typing.Type.t Path.t;
        kind : var_kind_t;
        ty_sc : Typing.Scheme.t;
      }
    | Struct of { name : Typing.Type.t Path.t; ty_sc : Typing.Scheme.t }
    | DefSeq of t list
    (* *)
    | Let of { mut : Typing.Type.mutability_t; name : string; expr : t }
    | Return of string
    | Call of { name : var_ref_t; args : var_ref_t list }
    | Index of { name : var_ref_t; index : var_ref_t }
    | Ref of { name : var_ref_t }
    | Deref of { name : var_ref_t }
    | Cast of { name : var_ref_t }
    | Construct
    | If of { cond : var_ref_t; t : t; e_opt : t option }
    | Loop of t
    | Break
    | LitBool of bool
    | LitInt of int
    | LitString of string
    | LitUnit
    | LitArrayElem of var_ref_t list
    | Assign of { lhs : t; rhs : t }
    | Undef
    | Var of var_ref_t
    | Seq of t list
    | StmtDispatchTable of {
        trait_name : Typing.Type.t Path.t;
        for_ty : Typing.Type.t;
        mapping : (Typing.Type.t Path.Name.t * Typing.Type.t Path.t) list;
      }

  and func_kind_t = FuncKindDecl | FuncKindDef of t | FuncKindExtern of string

  and var_kind_t = VarKindExtern of string

  and var_ref_t =
    | VarLocal of { name : string; label : string }
    | VarGlobal of { name : string }
    | VarGlobal2 of { nest : Typing.Type.t Path.t }
    | VarParam of { index : int; name : string }
  [@@deriving show, yojson_of]
end

type ctx_t = { ds : Diagnostics.t; subst : Typing.Subst.t }

let context ~ds ~subst = { ds; subst }

module Env = struct
  type t = (string, NAst.var_ref_t) Hashtbl.t

  let create () : t = Hashtbl.create (module String)

  let insert_alias env original target =
    Hashtbl.set env ~key:original ~data:target

  let find_alias_opt env original : NAst.var_ref_t option =
    Hashtbl.find env original
end

let fresh_id =
  let i = ref 0 in
  let f () =
    let v = !i in
    i := v + 1;
    let id = Printf.sprintf "%d" v in
    id
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
let insert_let' ?label mut k_form gen =
  match k_form with
  | NAst.{ kind = Var (VarLocal _ as v); _ } -> gen v
  | NAst.{ kind = Var (VarGlobal _ as v); _ } -> gen v
  | NAst.{ span; ty; _ } -> (
      let new_id = fresh_id () in
      let let_stmt =
        NAst.{ kind = Let { mut; name = new_id; expr = k_form }; ty; span }
      in
      let new_v =
        let label = Option.value label ~default:"generated" in
        NAst.VarLocal { name = new_id; label }
      in
      match gen new_v with
      | NAst.{ kind = Seq nodes; ty; span } ->
          NAst.{ kind = Seq (let_stmt :: nodes); ty; span }
      | NAst.{ ty; span; _ } as node ->
          NAst.{ kind = Seq [ let_stmt; node ]; ty; span } )

let insert_let ?label k_form gen =
  insert_let' ?label Typing.Type.MutImm k_form gen

let insert_let_mut ?label k_form gen =
  insert_let' ?label Typing.Type.MutMut k_form gen

(* Currently K-normalize *)
let rec normalize ~ctx ~env ast =
  match ast with
  (* *)
  | TAst.{ kind = Module { stmts }; ty; span; _ } ->
      let nodes = normalize ~ctx ~env stmts in
      NAst.{ kind = Module { nodes }; ty; span }
  (* *)
  | TAst.{ kind = Import { pkg; mods }; ty; span; _ } ->
      NAst.{ kind = Import { pkg; mods }; ty; span }
  (* *)
  | TAst.{ kind = DeclExternFunc { name; extern_name; ty_sc }; ty; span; _ } ->
      NAst.
        {
          kind = Func { name; kind = FuncKindExtern extern_name; ty_sc };
          ty;
          span;
        }
  (* *)
  | TAst.
      { kind = DeclExternStaticVar { name; extern_name; ty_sc }; ty; span; _ }
    ->
      NAst.
        {
          kind = Static { name; kind = VarKindExtern extern_name; ty_sc };
          ty;
          span;
        }
  (* *)
  | TAst.{ kind = DeclFunc { name; ty_sc }; ty; span; _ } ->
      NAst.{ kind = Func { name; kind = FuncKindDecl; ty_sc }; ty; span }
  (* *)
  | TAst.{ kind = DefFunc { name; ty_sc; body; _ }; ty; span; _ } ->
      let env = Env.create () in
      let body' = normalize ~ctx ~env body in
      NAst.{ kind = Func { name; kind = FuncKindDef body'; ty_sc }; ty; span }
  (* *)
  | TAst.{ kind = DefStruct { name; ty_sc }; ty; span; _ } ->
      NAst.{ kind = Struct { name; ty_sc }; ty; span }
  (* *)
  | TAst.{ kind = DefSeq nodes; ty; span; _ } ->
      let nodes' = List.map nodes ~f:(normalize ~ctx ~env) in
      NAst.{ kind = DefSeq nodes'; ty; span }
  (* *)
  | TAst.{ kind = StmtSeq nodes; ty; span; _ } ->
      let node' = List.map nodes ~f:(normalize ~ctx ~env) in
      NAst.{ kind = Seq node'; ty; span }
  (* *)
  | TAst.{ kind = StmtExpr expr; ty; span; _ } ->
      let k = insert_let (normalize ~ctx ~env expr) in
      k (fun _id -> NAst.{ kind = Undef; ty; span })
  (* *)
  | TAst.{ kind = StmtExprApply expr; ty; span; _ } ->
      let k = insert_let (normalize ~ctx ~env expr) in
      k (fun id -> NAst.{ kind = Var id; ty; span })
  (* *)
  | TAst.{ kind = StmtLet { mut; name; expr }; ty; span; _ } ->
      let k = insert_let' ?label:(Some name) mut (normalize ~ctx ~env expr) in
      k (fun v ->
          Env.insert_alias env name v;
          NAst.{ kind = Undef; ty; span })
  (* *)
  | TAst.{ kind = ExprIf (cond, t, e_opt); ty; span; _ } ->
      let k = insert_let (normalize ~ctx ~env cond) in
      k (fun v_cond ->
          let t_assign =
            let k = insert_let (normalize ~ctx ~env t) in
            k (fun v_id ->
                let ty = t.TAst.ty in
                let span = t.TAst.span in
                NAst.{ kind = Var v_id; ty; span })
          in
          let e_assign_opt =
            Option.map e_opt ~f:(fun e ->
                let k = insert_let (normalize ~ctx ~env e) in
                k (fun v_id ->
                    let ty = e.TAst.ty in
                    let span = e.TAst.span in
                    NAst.{ kind = Var v_id; ty; span }))
          in
          NAst.
            {
              kind = If { cond = v_cond; t = t_assign; e_opt = e_assign_opt };
              ty;
              span;
            })
  (* *)
  | TAst.{ kind = ExprLoop e; ty; span; _ } ->
      let k = insert_let (normalize ~ctx ~env e) in
      let inner =
        k (fun v_id ->
            let ty = e.TAst.ty in
            let span = e.TAst.span in
            NAst.{ kind = Var v_id; ty; span })
      in
      NAst.{ kind = Loop inner; ty; span }
  (* *)
  | TAst.{ kind = ExprBreak; ty; span; _ } -> NAst.{ kind = Break; ty; span }
  (* *)
  | TAst.{ kind = ExprCast e; ty; span; _ } ->
      let k = insert_let (normalize ~ctx ~env e) in
      k (fun v_id -> NAst.{ kind = Cast { name = v_id }; ty; span })
  (* *)
  | TAst.{ kind = ExprAssign { lhs; rhs }; ty; span; _ } ->
      let rhs = normalize ~ctx ~env rhs in
      let lhs = normalize ~ctx ~env lhs in
      NAst.{ kind = Assign { lhs; rhs }; ty; span }
  (* *)
  | TAst.{ kind = ExprCall (r, args); ty; span; _ } ->
      let rk = insert_let (normalize ~ctx ~env r) in
      let rec bind xs args k =
        match args with
        | [] ->
            k (fun r' ->
                NAst.{ kind = Call { name = r'; args = List.rev xs }; ty; span })
        | a :: args ->
            let k' = insert_let (normalize ~ctx ~env a) in
            k' (fun id -> bind (id :: xs) args k)
      in
      bind [] args rk
  (* *)
  | TAst.{ kind = ExprIndex (r, index); ty; span; _ } ->
      let k = insert_let (normalize ~ctx ~env r) in
      k (fun r_id ->
          let k = insert_let (normalize ~ctx ~env index) in
          k (fun index_id ->
              NAst.{ kind = Index { name = r_id; index = index_id }; ty; span }))
  (* *)
  | TAst.{ kind = ExprRef e; ty; span; _ } ->
      let k = insert_let (normalize ~ctx ~env e) in
      k (fun r_id -> NAst.{ kind = Ref { name = r_id }; ty; span })
  (* *)
  | TAst.{ kind = ExprDeref e; ty; span; _ } ->
      let k = insert_let (normalize ~ctx ~env e) in
      k (fun r_id -> NAst.{ kind = Deref { name = r_id }; ty; span })
  (* *)
  | TAst.{ kind = ExprStruct; ty; span; _ } ->
      NAst.{ kind = Construct; ty; span }
  (* *)
  | TAst.{ kind = Var { name; ref_type }; ty; span; _ } ->
      let nast =
        match ref_type with
        | TAst.RefTypeLocal ->
            let r =
              match Env.find_alias_opt env name with
              | Some r -> r
              | None -> failwith (Printf.sprintf "[ICE] %s" name)
            in
            NAst.{ kind = Var r; ty; span }
        | TAst.RefTypeGlobal ->
            let r = NAst.VarGlobal { name } in
            NAst.{ kind = Var r; ty; span }
        | TAst.RefTypeLocalArg index ->
            let r = NAst.VarParam { index; name } in
            NAst.{ kind = Var r; ty; span }
      in
      nast
  (* *)
  | TAst.{ kind = Var2 { chain }; ty; span; _ } ->
      let nast =
        match chain with
        (* local *)
        | Path.{ paths = []; last; _ } ->
            let Path.Name.{ name; _ } = last in
            let r =
              match Env.find_alias_opt env name with
              | Some r -> r
              | None -> failwith (Printf.sprintf "[ICE] %s" name)
            in
            NAst.{ kind = Var r; ty; span }
        (* global *)
        | _ ->
            let r = NAst.VarGlobal2 { nest = chain } in
            NAst.{ kind = Var r; ty; span }
      in
      nast
  (* *)
  | TAst.{ kind = LitBool v; ty; span; _ } ->
      NAst.{ kind = LitBool v; ty; span }
  (* *)
  | TAst.{ kind = LitInt v; ty; span; _ } -> NAst.{ kind = LitInt v; ty; span }
  (* *)
  | TAst.{ kind = LitString v; ty; span; _ } ->
      NAst.{ kind = LitString v; ty; span }
  (* *)
  | TAst.{ kind = LitUnit; ty; span; _ } -> NAst.{ kind = LitUnit; ty; span }
  (* *)
  | TAst.{ kind = LitArrayElem elems; ty; span; _ } ->
      let rec bind xs elems k =
        match elems with
        | [] -> NAst.{ kind = LitArrayElem (List.rev xs); ty; span }
        | e :: es ->
            let k' = insert_let (normalize ~ctx ~env e) in
            k' (fun id -> bind (id :: xs) es k)
      in
      bind [] elems (fun _ -> failwith "")
  (* *)
  | TAst.
      { kind = StmtDispatchTable { trait_name; for_ty; mapping }; ty; span; _ }
    ->
      NAst.
        { kind = StmtDispatchTable { trait_name; for_ty; mapping }; ty; span }
