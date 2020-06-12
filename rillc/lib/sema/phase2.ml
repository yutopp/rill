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
  type t = { kind : kind_t; ty : Typing.Type.t; span : Span.t }

  and kind_t =
    | Module of t list
    | Import of { pkg : string; mods : string list }
    | DeclExternFunc of { name : string; extern_name : string }
    | DefFunc of { name : string; body : t }
    | StmtSeq of t list
    | StmtExpr of t
    | StmtLet of { mut : Typing.Type.mutability_t; name : string; expr : t }
    | ExprIf of t * t * t option
    | ExprLoop of t
    | ExprBreak
    | ExprAssign of { lhs : t; rhs : t }
    | ExprCall of t * t list
    | ExprIndex of t * t
    | ExprRef of t
    | Var of string
    | VarParam of int
    | LitBool of bool
    | LitInt of int
    | LitString of string
    | LitUnit
    | LitArrayElem of t list
  [@@deriving show]
end

type ctx_t = {
  ds : Diagnostics.t;
  mutable subst : Typing.Subst.t;
  builtin : Builtin.t;
  exec_ctx : exec_ctx_t option;
}

and exec_ctx_t = ExecCtxFunc of { return : Typing.Type.t }

let context ~ds ~subst ~builtin = { ds; subst; builtin; exec_ctx = None }

let context_merge dst src = dst.subst <- src.subst

type result_t = (TopAst.t, Diagnostics.Elem.t) Result.t

let rec into_typed_tree ~ctx ast : (TAst.t, Diagnostics.Elem.t) Result.t =
  let open Result.Let_syntax in
  match ast with
  (* *)
  | TopAst.{ kind = Module { nodes; env }; span } ->
      (* Imported aliases *)
      let aliases = Env.collect_aliases env in
      let external_nodes =
        aliases |> List.map ~f:create_external_nodes |> List.join
      in

      (* Nodes *)
      let%bind nodes =
        List.fold_result nodes ~init:[] ~f:(fun mapped node ->
            match into_typed_tree ~ctx node with
            | Ok node' -> Ok (node' :: mapped)
            | Error d ->
                Diagnostics.append ctx.ds d;
                (* skip inserting *)
                Ok mapped)
        |> Result.map ~f:List.rev
      in

      (* TODO: module type? *)
      let binding_mut = Typing.Type.MutImm in
      let ty = Typing.Type.{ ty = Unit; binding_mut; span } in
      Ok TAst.{ kind = Module (List.join [ external_nodes; nodes ]); ty; span }
  (* *)
  | TopAst.{ kind = WithEnv { node; env }; span } -> with_env ~ctx ~env node
  (* *)
  | TopAst.{ kind = PassThrough { node }; span } -> pass_through ~ctx node

and with_env ~ctx ~env ast : (TAst.t, Diagnostics.Elem.t) Result.t =
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
  | Ast.{ kind = DefFunc { name; params; ret_ty; body }; span } ->
      (* TODO: check that there are no holes *)
      let f_ty = Typing.Subst.subst_type ctx.subst (Env.type_of env) in
      let (params_tys, ret_ty) = Typing.Type.assume_func_ty f_ty in

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
                let v_ty = Env.type_of venv in
                let t_param = TAst.{ kind = VarParam index; ty = v_ty; span } in
                let mut = v_ty.Typing.Type.binding_mut in

                let ty = ctx.builtin.Builtin.unit_ in
                TAst.{ kind = StmtLet { mut; name; expr = t_param }; ty; span }
            | _ -> failwith "")
      in

      let ctx' =
        { ctx with exec_ctx = Some (ExecCtxFunc { return = ret_ty }) }
      in
      let%bind t_body = analyze ~ctx:ctx' ~env body in
      context_merge ctx ctx';

      (* TODO: check that subst has no holes *)
      let%bind subst = Typer.unify ~span ctx.subst ret_ty t_body.TAst.ty in
      ctx.subst <- subst;

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
          ~message:"Not supported def node (phase2, with_env)"
      in
      let elm = Diagnostics.Elem.error ~span e in
      Error elm

and pass_through ~ctx ast =
  match ast with
  (* *)
  | Ast.{ kind = Import { pkg; mods }; span; _ } ->
      let stringify node =
        match node with
        | Ast.{ kind = ID s; _ } -> s
        | Ast.{ kind = IDWildcard; _ } -> "*"
        | _ -> failwith "[ICE]"
      in
      let pkg = stringify pkg in
      let mods = List.map mods ~f:stringify in
      let binding_mut = Typing.Type.MutImm in
      let ty = Typing.Type.{ ty = Unit; binding_mut; span } in
      Ok TAst.{ kind = Import { pkg; mods }; ty; span }
  (* *)
  | Ast.{ span; _ } ->
      let e =
        new Common.Reasons.internal_error
          ~message:"Not supported def node (phase2, pass_through)"
      in
      let elm = Diagnostics.Elem.error ~span e in
      Error elm

and analyze ~ctx ~env ast : (TAst.t, Diagnostics.Elem.t) Result.t =
  let open Result.Let_syntax in
  match ast with
  (* *)
  | Ast.{ kind = StmtExpr expr; span } ->
      let%bind t_expr = analyze ~ctx ~env expr in
      let ty = Typing.Type.{ ctx.builtin.Builtin.unit_ with span } in
      Ok TAst.{ kind = StmtExpr t_expr; ty; span }
  (* *)
  | Ast.{ kind = StmtLet decl; span } ->
      let (attr, name, ty_spec, expr) =
        match decl with
        | Ast.{ kind = VarDecl { attr; name; ty_spec; expr }; _ } ->
            (attr, name, ty_spec, expr)
        | _ -> failwith ""
      in
      let%bind var_ty =
        match ty_spec with
        | Some ty_spec -> Phase1_1.lookup_type ~env ty_spec
        | None ->
            (* infer *)
            let ty = Typing.Subst.fresh_ty ~span ctx.subst in
            Ok ty
      in
      let binding_mut = Mut.mutability_of attr in
      let var_ty = Typing.Type.{ var_ty with binding_mut } in
      (* CANNOT reference self *)
      let%bind t_expr = analyze ~ctx ~env expr in
      let%bind subst = Typer.unify ~span ctx.subst var_ty t_expr.TAst.ty in

      let visibility = Env.Public in
      let venv =
        Env.create name ~parent:(Some env) ~visibility ~ty:var_ty
          ~ty_w:(Env.Val var_ty)
      in
      Env.insert env venv;

      ctx.subst <- subst;

      let ty = Typing.Type.{ ctx.builtin.Builtin.unit_ with span } in
      let mut = binding_mut in
      Ok TAst.{ kind = StmtLet { mut; name; expr = t_expr }; ty; span }
  (* *)
  | Ast.{ kind = ExprGrouping _; _ } as expr ->
      let expr' = Operators.reconstruct expr in
      analyze ~ctx ~env expr'
  (* *)
  | Ast.{ kind = ExprBlock []; span } ->
      let ty =
        let last_ty = ctx.builtin.Builtin.unit_ in
        Typing.Type.{ last_ty with span }
      in
      let t_node = TAst.{ kind = LitUnit; ty; span } in
      Ok TAst.{ kind = StmtSeq [ t_node ]; ty; span }
  (* *)
  | Ast.{ kind = ExprBlock nodes; span } ->
      (* stop evaluation if failed (cannot forward reference in seq scope) *)
      let%bind t_nodes_rev =
        let env =
          (* scope *)
          let visibility = Env.Private in
          let ty = Typing.Subst.fresh_ty ~span ctx.subst in
          Env.create "" ~parent:(Some env) ~visibility ~ty ~ty_w:Env.N
        in
        List.fold_result nodes ~init:[] ~f:(fun ps node ->
            let%bind t_node = analyze ~ctx ~env node in
            Ok (t_node :: ps))
      in
      let ty =
        let n = List.hd_exn t_nodes_rev in
        let last_ty = n.TAst.ty in
        Typing.Type.{ last_ty with span }
      in
      let t_nodes = List.rev t_nodes_rev in
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
  | Ast.{ kind = ExprLoop e; span; _ } ->
      let%bind t_e = analyze ~ctx ~env e in

      let ty =
        let last_ty = ctx.builtin.Builtin.unit_ in
        Typing.Type.{ last_ty with span }
      in
      let%bind subst = Typer.unify ~span ctx.subst ty t_e.TAst.ty in
      ctx.subst <- subst;

      Ok TAst.{ kind = ExprLoop t_e; ty; span }
  (* *)
  | Ast.{ kind = ExprBreak; span; _ } ->
      let ty =
        let last_ty = ctx.builtin.Builtin.unit_ in
        Typing.Type.{ last_ty with span }
      in
      Ok TAst.{ kind = ExprBreak; ty; span }
  (* *)
  | Ast.{ kind = ExprAssign { lhs; rhs }; span; _ } ->
      let%bind t_args = analyze_args ~ctx ~env [ lhs; rhs ] in
      let (t_lhs, t_rhs) =
        match t_args with [ a; b ] -> (a, b) | _ -> failwith "[ICE]"
      in
      let%bind () =
        match t_lhs.TAst.ty.Typing.Type.binding_mut with
        | Typing.Type.MutMut -> Ok ()
        | Typing.Type.MutImm ->
            let e = new Reasons.cannot_assign in
            let elm = Diagnostics.Elem.error ~span e in
            Error elm
      in

      (* TODO: fix type? *)
      let binding_mut = Typing.Type.MutImm in
      let ty = Typing.Type.{ ty = Unit; binding_mut; span } in
      Ok TAst.{ kind = ExprAssign { lhs = t_lhs; rhs = t_rhs }; ty; span }
  (* *)
  | Ast.{ kind = ExprBinaryOp { op; lhs; rhs }; span; _ } ->
      analyze_call ~ctx ~env ~span op [ lhs; rhs ]
  (* *)
  | Ast.{ kind = ExprCall (r, args); span; _ } ->
      analyze_call ~ctx ~env ~span r args
  (* *)
  | Ast.{ kind = ExprIndex (r, index); span; _ } ->
      (* TODO: index trait *)
      let%bind t_r = analyze ~ctx ~env r in
      (* TODO: fix type inference *)
      let elem_ty =
        match Typing.Subst.subst_type ctx.subst t_r.TAst.ty with
        | Typing.Type.{ ty = Array { elem; n }; _ } ->
            let binding_mut = t_r.TAst.ty.Typing.Type.binding_mut in
            Typing.Type.{ elem with binding_mut }
        | _ -> failwith "[ICE] not supported"
      in

      let index_ty = ctx.builtin.Builtin.i32_ in
      let%bind t_index = analyze ~ctx ~env index in
      let%bind subst = Typer.unify ~span ctx.subst index_ty t_index.TAst.ty in
      ctx.subst <- subst;

      Ok TAst.{ kind = ExprIndex (t_r, t_index); ty = elem_ty; span }
  (* *)
  | Ast.{ kind = ExprRef (attr, e); span; _ } ->
      (* TODO: ref trait *)
      let%bind t_r = analyze ~ctx ~env e in

      let mut = Mut.mutability_of attr in
      let%bind () =
        match (mut, t_r.TAst.ty.Typing.Type.binding_mut) with
        | (Typing.Type.MutMut, Typing.Type.MutMut) | (Typing.Type.MutImm, _) ->
            Ok ()
        | (Typing.Type.MutMut, _) ->
            let e = new Reasons.cannot_reference_mut in
            let elm = Diagnostics.Elem.error ~span e in
            Error elm
      in

      let ty =
        let elem_ty = t_r.TAst.ty in
        Typing.Type.{ (ctx.builtin.Builtin.pointer_ mut elem_ty) with span }
      in
      Ok TAst.{ kind = ExprRef t_r; ty; span }
  (* *)
  | Ast.{ kind = ID name; span; _ } ->
      let%bind venv =
        Env.lookup_value env name
        |> Result.map_error ~f:(fun _trace ->
               let candidates = [] (* TODO *) in
               let e = new Common.Reasons.id_not_found ~name ~candidates in
               let elm = Diagnostics.Elem.error ~span e in
               elm)
      in
      let ty = Env.type_of venv in
      let ty = Typing.Type.{ ty with span } in
      Ok TAst.{ kind = Var name; span; ty }
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
  | Ast.{ kind = LitArrayElems elems; span; _ } ->
      let%bind t_elems =
        List.fold_result elems ~init:[] ~f:(fun rev_t_elems elem ->
            let%bind t_elem = analyze ~ctx ~env elem in
            Ok (t_elem :: rev_t_elems))
        |> Result.map ~f:List.rev
      in

      let elem_ty = Typing.Subst.fresh_ty ~span ctx.subst in
      let%bind subst =
        List.fold_result t_elems ~init:ctx.subst ~f:(fun subst t_elem ->
            let span = t_elem.TAst.span in
            let binding_mut = Typing.Type.MutImm in
            let actual_elem_ty =
              Typing.Type.{ t_elem.TAst.ty with binding_mut }
            in
            Typer.unify ~span subst elem_ty actual_elem_ty)
      in
      ctx.subst <- subst;

      let ty =
        let n = List.length elems in
        Typing.Type.{ (ctx.builtin.Builtin.array_ elem_ty n) with span }
      in
      Ok TAst.{ kind = LitArrayElem t_elems; ty; span }
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

and analyze_args ~ctx ~env args =
  let open Result.Let_syntax in
  List.fold_result args ~init:[] ~f:(fun ax arg ->
      let%bind t_arg = analyze ~ctx ~env arg in
      Ok (t_arg :: ax))
  |> Result.map ~f:List.rev

and analyze_call ~ctx ~env ~span recv args =
  let open Result.Let_syntax in
  let%bind t_recv = analyze ~ctx ~env recv in
  let%bind t_args = analyze_args ~ctx ~env args in
  let recv_ty = t_recv.TAst.ty in

  let ret_ty = Typing.Subst.fresh_ty ~span ctx.subst in
  let fn_ty =
    let linkage = Typing.Subst.fresh_linkage ctx.subst in
    let args_ty = List.map t_args ~f:(fun arg -> arg.TAst.ty) in
    let binding_mut = Typing.Type.MutImm in
    Typing.Type.
      {
        ty = Func { params = args_ty; ret = ret_ty; linkage };
        binding_mut;
        span;
      }
  in
  [%loga.debug "Func receiver ty = %s" (Typing.Type.to_string recv_ty)];
  [%loga.debug "Func evaled ty = %s" (Typing.Type.to_string fn_ty)];

  let%bind subst = Typer.unify ~span ctx.subst recv_ty fn_ty in
  ctx.subst <- subst;

  Ok TAst.{ kind = ExprCall (t_recv, t_args); ty = ret_ty; span }

and create_external_nodes env =
  [%loga.debug "Env = %s" (Env.show env)];
  let actual_root_mod = Env.root_mod_of ~scoped:false env in
  let env_kind = Env.w_of env in
  let ty = Env.type_of env in

  (* TODO: support import module *)
  match env_kind with
  | Env.Val _ ->
      let nodes =
        match ty with
        (* external func *)
        | Typing.Type.{ ty = Func { params; ret; linkage }; span; _ } ->
            [%loga.debug
              "External func = %s: %s" env.Env.name (Typing.Type.show ty)];
            let node =
              match linkage with
              (* *)
              | Typing.Type.LinkageC extern_name ->
                  let name = env.Env.name (* TODO: fix *) in
                  TAst.{ kind = DeclExternFunc { name; extern_name }; ty; span }
              (* *)
              | Typing.Type.LinkageRillc -> failwith ""
              (* *)
              | Typing.Type.LinkageVar _ -> failwith ""
            in
            [ node ]
        (* external val *)
        | Typing.Type.{ ty; _ } -> []
      in
      nodes
  (* unsupported *)
  | _ -> failwith "[ICE]"
