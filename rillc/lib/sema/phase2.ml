(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module Span = Common.Span
module Diagnostics = Common.Diagnostics
module Ast = Syntax.Ast
module TopAst = Phase1.TopAst

module TAst = struct
  type t = { kind : kind_t; ty : Typing.Type.t; span : (Span.t[@sexp.opaque]) }

  and kind_t =
    | Module of t list
    | DeclExternFunc of { name : string; extern_name : string }
    | DefFunc of { name : string; body : t }
    | DefParamVar of { name : string; index : int }
    | StmtSeq of t list
    | StmtExpr of t
    | StmtLet of { name : string; expr : t }
    | ExprIf of t * t * t option
    | ExprCall of t * t list
    | ID of string
    | LitBool of bool
    | LitInt of int
    | LitString of string
  [@@deriving sexp_of]
end

type ctx_t = {
  ds : Diagnostics.t;
  mutable subst : Typing.Subst.t;
  builtin : Builtin.t;
}

let context ~ds ~subst ~builtin = { ds; subst; builtin }

type result_t = (TopAst.t, Diagnostics.Elem.t) Result.t

let rec into_typed_tree ~ctx ast : (TAst.t, Diagnostics.Elem.t) Result.t =
  let open Result.Let_syntax in
  match ast with
  (* *)
  | TopAst.{ kind = Module nodes; span; env } ->
      let%bind nodes_rev =
        List.fold_result nodes ~init:[] ~f:(fun mapped node ->
            match into_typed_tree ~ctx node with
            | Ok node' -> Ok (node' :: mapped)
            | Error d ->
                Diagnostics.append ctx.ds d;
                (* skip inserting *)
                Ok mapped)
      in
      (* TODO: module type? *)
      let ty = Typing.Type.{ ty = Unit; span } in
      Ok TAst.{ kind = Module (List.rev nodes_rev); ty; span }
  (* *)
  | TopAst.{ kind = Decl decl; span; env } -> declare ~ctx ~env decl
  (* *)
  | TopAst.{ kind = Def def; span; env } -> define ~ctx ~env def

and declare ~ctx ~env ast : (TAst.t, Diagnostics.Elem.t) Result.t =
  let open Result.Let_syntax in
  match ast with
  (* *)
  | Ast.{ kind = DeclExternFunc { name; params; ret_ty; symbol_name }; span } ->
      (* TODO: check that there are no holes *)
      let f_ty = Typing.Subst.subst_type ctx.subst (Env.type_of env) in
      let%bind t_sym = analyze ~ctx ~env symbol_name in
      let%bind extern_name =
        match t_sym with
        | TAst.{ kind = LitString v; _ } -> Ok v
        | TAst.{ ty; span; _ } ->
            let e =
              new Common.Reasons.internal_error
                ~message:"Not supported decl node (phase2)"
            in
            let elm = Diagnostics.Elem.error ~span e in
            Error elm
      in

      Ok TAst.{ kind = DeclExternFunc { name; extern_name }; ty = f_ty; span }
  (* *)
  | Ast.{ span; _ } ->
      let e =
        new Common.Reasons.internal_error
          ~message:"Not supported decl node (phase2)"
      in
      let elm = Diagnostics.Elem.error ~span e in
      Error elm

and define ~ctx ~env ast : (TAst.t, Diagnostics.Elem.t) Result.t =
  let open Result.Let_syntax in
  match ast with
  (* *)
  | Ast.{ kind = DefFunc { name; params; ret_ty; body }; span } ->
      (* TODO: check that there are no holes *)
      let f_ty = Typing.Subst.subst_type ctx.subst (Env.type_of env) in

      let t_param_vars_decls =
        List.mapi params ~f:(fun index param ->
            match param with
            | Ast.{ kind = ParamDecl { name; ty_spec }; span } ->
                let venv =
                  match Env.find_value env name with
                  | Some v -> v
                  | None -> failwith "[ICE] Maybe phase1_1 didn't run normally"
                in
                (* NOTE: a type is already checked at phase1_1 *)
                (* TODO: Distinguish params and decls *)
                TAst.
                  {
                    kind = DefParamVar { name; index };
                    span;
                    ty = Env.type_of venv;
                  }
            | _ -> failwith "")
      in
      let%bind t_body = analyze ~ctx ~env body in

      let t_seq =
        (* parameter delcs -> body *)
        let seq = t_param_vars_decls @ [ t_body ] in
        let ty = Typing.Type.{ ctx.builtin.Builtin.unit_ with span } in
        TAst.{ kind = StmtSeq seq; ty; span = t_body.span }
      in
      Ok TAst.{ kind = DefFunc { name; body = t_seq }; ty = f_ty; span }
  (* *)
  | Ast.{ span; _ } ->
      let e =
        new Common.Reasons.internal_error
          ~message:"Not supported def node (phase2)"
      in
      let elm = Diagnostics.Elem.error ~span e in
      Error elm

and analyze ~ctx ~env ast : (TAst.t, Diagnostics.Elem.t) Result.t =
  let open Result.Let_syntax in
  match ast with
  (* *)
  | Ast.{ kind = StmtExpr expr; span } ->
      let%bind t_expr = analyze ~ctx ~env expr in
      let ty = Typing.Type.{ t_expr.TAst.ty with span } in
      Ok TAst.{ kind = StmtExpr t_expr; ty; span }
  (* *)
  | Ast.{ kind = StmtLet decl; span } ->
      let (attr, name, ty_spec, expr) =
        match decl with
        | Ast.{ kind = VarDecl { attr; name; ty_spec; expr }; _ } ->
            (attr, name, ty_spec, expr)
        | _ -> failwith ""
      in
      let%bind spec_ty =
        match ty_spec with
        | Some ty_spec -> lookup_type ~ctx ~env ty_spec
        | None ->
            (* infer *)
            let ty = Typing.Subst.fresh_ty ~span ctx.subst in
            Ok ty
      in
      (* CANNOT reference self *)
      let%bind t_expr = analyze ~ctx ~env expr in
      let%bind subst = Typer.unify ~span ctx.subst spec_ty t_expr.TAst.ty in

      let venv = Env.create name ~parent:(Some env) ~ty_w:(Env.T spec_ty) in
      Env.insert_value env venv;

      ctx.subst <- subst;

      Ok TAst.{ kind = StmtLet { name; expr = t_expr }; ty = spec_ty; span }
  (* *)
  | Ast.{ kind = ExprGrouping _; _ } as expr ->
      let expr' = Operators.reconstruct expr in
      analyze ~ctx ~env expr'
  (* *)
  | Ast.{ kind = ExprBlock nodes; span } ->
      let t_nodes_rev =
        List.fold_until nodes ~init:[]
          ~f:(fun ps node ->
            match analyze ~ctx ~env node with
            | Ok t_node -> Continue_or_stop.Continue (t_node :: ps)
            | Error d ->
                Diagnostics.append ctx.ds d;
                (* stop evaluation (cannot forward reference in seq scope) *)
                Continue_or_stop.Stop ps)
          ~finish:Fn.id
      in
      let ty =
        let last_ty =
          match t_nodes_rev with
          | n :: _ -> n.TAst.ty
          | _ -> ctx.builtin.Builtin.unit_
        in
        Typing.Type.{ last_ty with span }
      in
      let t_nodes = List.rev t_nodes_rev in
      (* TODO:fix env *)
      Ok TAst.{ kind = StmtSeq t_nodes; ty; span }
  (* *)
  | Ast.{ kind = ExprIf (cond, t, e_opt); span; _ } ->
      let%bind t_cond = analyze ~ctx ~env cond in
      let%bind subst =
        Typer.unify ~span ctx.subst t_cond.TAst.ty ctx.builtin.Builtin.bool_
      in

      let ctx' = { ctx with subst } in
      let%bind t_t = analyze ~ctx:ctx' ~env t in

      let%bind (t_e_opts, e_ty) =
        match e_opt with
        | Some e ->
            let%bind t_e = analyze ~ctx:ctx' ~env e in
            Ok (Some t_e, t_e.TAst.ty)
        | None -> Ok (None, ctx.builtin.Builtin.unit_)
      in

      let%bind subst = Typer.unify ~span ctx'.subst t_t.TAst.ty e_ty in
      ctx.subst <- subst;

      let ty = Typing.Type.{ t_t.TAst.ty with span } in
      Ok TAst.{ kind = ExprIf (t_cond, t_t, t_e_opts); ty; span }
  (* *)
  | Ast.{ kind = ExprBinaryOp { op; lhs; rhs }; span; _ } ->
      analyze_call ~ctx ~env ~span op [ lhs; rhs ]
  (* *)
  | Ast.{ kind = ExprCall (r, args); span; _ } ->
      analyze_call ~ctx ~env ~span r args
  (* *)
  | Ast.{ kind = ID name; span; _ } ->
      let%bind venv =
        Env.lookup_value env name
        |> Result.map_error ~f:(fun _trace ->
               let e = new Common.Reasons.id_not_found ~name in
               let elm = Diagnostics.Elem.error ~span e in
               elm)
      in
      let ty = Env.type_of venv in
      let ty = Typing.Type.{ ty with span } in
      Ok TAst.{ kind = ID name; span; ty }
  (* *)
  | Ast.{ kind = LitBool v; span; _ } ->
      let ty = Typing.Type.{ ctx.builtin.Builtin.bool_ with span } in
      Ok TAst.{ kind = LitBool v; ty; span }
  (* *)
  | Ast.{ kind = LitInt (value, bits, signed); span; _ } ->
      (* TODO: fix i32 *)
      let ty = Typing.Type.{ ctx.builtin.Builtin.i32_ with span } in
      Ok TAst.{ kind = LitInt value; ty; span }
  (* *)
  | Ast.{ kind = LitString v; span; _ } ->
      let ty = Typing.Type.{ ctx.builtin.Builtin.string_ with span } in
      Ok TAst.{ kind = LitString v; ty; span }
  (* *)
  | Ast.{ kind; span; _ } ->
      let s = Ast.sexp_of_kind_t kind |> Sexp.to_string_mach in
      let e =
        new Common.Reasons.internal_error
          ~message:
            (Printf.sprintf "Not supported stmt/expr node (phase2): %s" s)
      in
      let elm = Diagnostics.Elem.error ~span e in
      Error elm

and analyze_call ~ctx ~env ~span recv args =
  let open Result.Let_syntax in
  let%bind t_recv = analyze ~ctx ~env recv in
  let%bind t_args =
    List.fold_result args ~init:[] ~f:(fun ax arg ->
        let%bind t_arg = analyze ~ctx ~env arg in
        Ok (t_arg :: ax))
    |> Result.map ~f:List.rev
  in
  let recv_ty = t_recv.TAst.ty in

  let ret_ty = Typing.Subst.fresh_ty ~span ctx.subst in
  let fn_ty =
    let args_ty = List.map t_args ~f:(fun arg -> arg.TAst.ty) in
    Typing.Type.{ ty = Func (args_ty, ret_ty); span }
  in
  [%Loga.debug "Func receiver ty = %s" (Typing.Type.to_string recv_ty)];
  [%Loga.debug "Func evaled ty = %s" (Typing.Type.to_string fn_ty)];

  let%bind subst = Typer.unify ~span ctx.subst recv_ty fn_ty in
  ctx.subst <- subst;

  Ok TAst.{ kind = ExprCall (t_recv, t_args); ty = ret_ty; span }

and lookup_type ~ctx ~env ast : (Typing.Type.t, Diagnostics.Elem.t) Result.t =
  let open Result.Let_syntax in
  match ast with
  | Ast.{ kind = ID name; span } ->
      let%bind env =
        Env.lookup_type env name
        |> Result.map_error ~f:(fun _trace ->
               let e = new Common.Reasons.id_not_found ~name in
               let elm = Diagnostics.Elem.error ~span e in
               elm)
      in
      Ok (Env.type_of env)
  (* *)
  | Ast.{ span; _ } -> failwith ""
