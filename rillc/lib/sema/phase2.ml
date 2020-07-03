(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module Span = Common.Span
module Ast = Syntax.Ast
module TopAst = Phase1.TopAst

module TAst = struct
  type t = { kind : kind_t; ty : Typing.Type.t; span : Span.t }

  and kind_t =
    | Module of { nodes : t list }
    | Import of { pkg : string; mods : string list }
    | DeclExternFunc of {
        name : Typing.Type.t Common.Chain.Nest.t;
        ty_sc : Typing.Scheme.t;
        extern_name : string;
      }
    | DeclExternStaticVar of {
        name : Typing.Type.t Common.Chain.Nest.t;
        ty_sc : Typing.Scheme.t;
        extern_name : string;
      }
    | DeclFunc of {
        name : Typing.Type.t Common.Chain.Nest.t;
        ty_sc : Typing.Scheme.t;
      }
    | DefFunc of {
        name : Typing.Type.t Common.Chain.Nest.t;
        ty_sc : Typing.Scheme.t;
        body : t;
      }
    | DefStruct of {
        name : Typing.Type.t Common.Chain.Nest.t;
        ty_sc : Typing.Scheme.t;
      }
    (* *)
    | StmtSeq of t list
    | StmtExpr of t
    | StmtExprApply of t
    | StmtLet of { mut : Typing.Type.mutability_t; name : string; expr : t }
    | ExprIf of t * t * t option
    | ExprLoop of t
    | ExprBreak
    | ExprCast of t
    | ExprAssign of { lhs : t; rhs : t }
    | ExprCall of t * t list
    | ExprIndex of t * t
    | ExprRef of t
    | ExprDeref of t
    | ExprStruct
    | Var of { name : string; ref_type : ref_t }
    | Var2 of { chain : Typing.Type.t Common.Chain.t }
    | LitBool of bool
    | LitInt of int
    | LitString of string
    | LitUnit
    | LitArrayElem of t list

  and ref_t = RefTypeGlobal | RefTypeLocal | RefTypeLocalArg of int
  [@@deriving show]
end

module Ctx = struct
  type ctx_t = {
    ds : Diagnostics.t;
    mutable subst : Typing.Subst.t;
    builtin : Builtin.t;
    exec_ctx : exec_ctx_t option;
  }

  and exec_ctx_t = ExecCtxFunc of { return : Typing.Type.t }

  let create ~ds ~subst ~builtin = { ds; subst; builtin; exec_ctx = None }

  let merge dst src = dst.subst <- src.subst

  let append_diagnostic ctx d = Diagnostics.append ctx.ds d

  let update_subst ctx subst = ctx.subst <- subst
end

type result_t = (TopAst.t, Diagnostics.Elem.t) Result.t

let rec into_typed_tree ~ctx ast : (TAst.t, Diagnostics.Elem.t) Result.t =
  let open Result.Let_syntax in
  match ast with
  (* *)
  | TopAst.{ kind = Module { nodes }; span } ->
      (* Nodes *)
      let%bind nodes =
        List.fold_result nodes ~init:[] ~f:(fun mapped node ->
            match into_typed_tree ~ctx node with
            | Ok node' -> Ok (node' :: mapped)
            | Error d ->
                Ctx.append_diagnostic ctx d;
                (* skip inserting *)
                Ok mapped)
        |> Result.map ~f:List.rev
      in

      (* TODO: module type? *)
      let binding_mut = Typing.Type.MutImm in
      let ty = Typing.Type.{ ty = Unit; binding_mut; span } in
      Ok TAst.{ kind = Module { nodes }; ty; span }
  (* *)
  | TopAst.{ kind = WithEnv { node; env }; span } -> with_env ~ctx ~env node
  (* *)
  | TopAst.{ kind = PassThrough { node }; span } -> pass_through ~ctx node

and with_env ~ctx ~env ast : (TAst.t, Diagnostics.Elem.t) Result.t =
  let open Result.Let_syntax in
  match ast with
  (* *)
  | Ast.{ kind = DeclExternFunc { params; ret_ty; symbol_name; _ }; span } ->
      (* TODO: check that there are no holes *)
      let ty_sc = Env.type_sc_of env in

      let%bind t_sym = analyze ~ctx ~env symbol_name in
      let%bind extern_name =
        match t_sym with
        | TAst.{ kind = LitString v; _ } -> Ok v
        | TAst.{ ty; span; _ } ->
            let e =
              new Diagnostics.Reasons.internal_error
                ~message:"Not supported decl node (phase2)"
            in
            let elm = Diagnostics.Elem.error ~span e in
            Error elm
      in

      let ty = ctx.Ctx.builtin.Builtin.unit_ in
      let name = Phase1_1.to_nested_chain env [] in
      Ok TAst.{ kind = DeclExternFunc { name; ty_sc; extern_name }; ty; span }
  (* *)
  | Ast.{ kind = DeclExternStaticVar { attr; name; _ }; span } ->
      (* TODO: check that there are no holes *)
      let ty_sc = Env.type_sc_of env in

      let extern_name = name in

      let ty = ctx.Ctx.builtin.Builtin.unit_ in
      let name = Phase1_1.to_nested_chain env [] in
      Ok
        TAst.
          { kind = DeclExternStaticVar { name; ty_sc; extern_name }; ty; span }
  (* *)
  | Ast.{ kind = DefFunc { name; params; ret_ty; body; _ }; span } ->
      (* TODO: check that there are no holes *)
      let f_ty_sc = Env.type_sc_of env in
      let f_ty = Typing.Scheme.raw_ty f_ty_sc in
      let (params_tys, ret_ty) = Typing.Type.assume_func_ty f_ty in

      let t_param_vars_decls =
        List.mapi params ~f:(fun index param ->
            match param with
            | Ast.{ kind = ParamDecl { name; ty_spec; _ }; span } ->
                let venv =
                  match Env.find_value env name with
                  | Some v -> v
                  | None -> failwith "[ICE] Maybe phase1_1 didn't run normally"
                in
                (* NOTE: a type is already checked at phase1_1 *)
                (* TODO: Distinguish params and decls *)
                let v_ty_sc = Env.type_sc_of venv in
                let v_ty = Typing.Scheme.assume_has_no_generics v_ty_sc in
                let ref_type = TAst.RefTypeLocalArg index in
                let t_param =
                  TAst.{ kind = Var { name; ref_type }; ty = v_ty; span }
                in
                let mut = v_ty.Typing.Type.binding_mut in

                let ty = ctx.Ctx.builtin.Builtin.unit_ in
                TAst.{ kind = StmtLet { mut; name; expr = t_param }; ty; span }
            | _ -> failwith "[ICE]")
      in

      let ctx' =
        Ctx.{ ctx with exec_ctx = Some (ExecCtxFunc { return = ret_ty }) }
      in
      let%bind t_body =
        let env =
          (* scope *)
          let visibility = Env.Private in
          let ty_sc = Typing.Scheme.of_ty ret_ty in
          Env.create "" ~parent:(Some env) ~visibility ~ty_sc
            ~kind:Env.KindScope ~lookup_space:Env.LkLocal
        in
        analyze ~ctx:ctx' ~env body
      in
      Ctx.merge ctx ctx';

      (* TODO: check that subst has no holes *)
      let%bind subst =
        Typer.unify ~span ctx.Ctx.subst ~from:ret_ty ~to_:t_body.TAst.ty
      in
      Ctx.update_subst ctx subst;

      let t_seq =
        (* parameter delcs -> body *)
        let seq = t_param_vars_decls @ [ t_body ] in
        let ty = Typing.Type.{ ctx.Ctx.builtin.Builtin.unit_ with span } in
        TAst.{ kind = StmtSeq seq; ty; span = t_body.span }
      in

      let ty = ctx.Ctx.builtin.Builtin.unit_ in
      (* TODO: fix *)
      let name =
        match name with
        | "main" ->
            let c = Common.Chain.Nest.create () in
            let n =
              Common.Chain.Layer.
                { name = "main"; kind = Var; generics_vars = [] }
            in
            Common.Chain.Nest.join_rev c n
        | _ -> Phase1_1.to_nested_chain env []
      in
      Ok
        TAst.
          { kind = DefFunc { name; ty_sc = f_ty_sc; body = t_seq }; ty; span }
  (* *)
  | Ast.{ kind = DefStruct _; span } ->
      (* TODO: check that there are no holes *)
      let ty_sc = Env.type_sc_of env in

      let ty = ctx.Ctx.builtin.Builtin.unit_ in
      let name = Phase1_1.to_nested_chain env [] in
      Ok TAst.{ kind = DefStruct { name; ty_sc }; ty; span }
  (* *)
  | Ast.{ span; _ } ->
      let e =
        new Diagnostics.Reasons.internal_error
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
        new Diagnostics.Reasons.internal_error
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

      let ty = Typing.Type.{ ctx.Ctx.builtin.Builtin.unit_ with span } in
      let%bind subst =
        Typer.unify ~span ctx.Ctx.subst ~from:ty ~to_:t_expr.TAst.ty
      in
      Ctx.update_subst ctx subst;

      Ok TAst.{ kind = StmtExpr t_expr; ty; span }
  (* *)
  | Ast.{ kind = StmtExprApply expr; span } ->
      let%bind t_expr = analyze ~ctx ~env expr in

      let ty = Typing.Type.{ t_expr.TAst.ty with span } in
      Ok TAst.{ kind = StmtExprApply t_expr; ty; span }
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
        | Some ty_spec -> lookup_type ~env ~ctx ty_spec
        | None ->
            (* infer *)
            let ty = Typing.Subst.fresh_ty ~span ctx.Ctx.subst in
            Ok ty
      in
      let binding_mut = Mut.mutability_of attr in
      let var_ty = Typing.Type.{ var_ty with binding_mut } in
      (* CANNOT reference self *)
      let%bind t_expr = analyze ~ctx ~env expr in
      let%bind subst =
        Typer.unify ~span ctx.Ctx.subst ~from:var_ty ~to_:t_expr.TAst.ty
      in
      Ctx.update_subst ctx subst;

      let visibility = Env.Public in
      let venv =
        let ty_sc = Typing.Scheme.of_ty var_ty in
        Env.create name ~parent:(Some env) ~visibility ~ty_sc ~kind:Env.Val
          ~lookup_space:Env.LkLocal
      in
      let () =
        match Env.insert env venv with
        | Env.InsertedHiding ->
            (* TODO: add warning *)
            ()
        | _ -> ()
      in

      let ty = Typing.Type.{ ctx.Ctx.builtin.Builtin.unit_ with span } in
      let mut = binding_mut in
      Ok TAst.{ kind = StmtLet { mut; name; expr = t_expr }; ty; span }
  (* *)
  | Ast.{ kind = ExprGrouping _; _ } as expr ->
      let expr' = Operators.reconstruct expr in
      analyze ~ctx ~env expr'
  (* *)
  | Ast.{ kind = ExprBlock []; span } ->
      let ty =
        let last_ty = ctx.Ctx.builtin.Builtin.unit_ in
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
          let ty = Typing.Subst.fresh_ty ~span ctx.Ctx.subst in
          let ty_sc = Typing.Scheme.of_ty ty in
          Env.create "" ~parent:(Some env) ~visibility ~ty_sc
            ~kind:Env.KindScope ~lookup_space:Env.LkLocal
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
        Typer.unify ~span ctx.Ctx.subst ~from:t_cond.TAst.ty
          ~to_:ctx.Ctx.builtin.Builtin.bool_
      in

      let ctx' = Ctx.{ ctx with subst } in
      let%bind t_t = analyze ~ctx:ctx' ~env t in

      let%bind (t_e_opts, e_ty) =
        match e_opt with
        | Some e ->
            let%bind t_e = analyze ~ctx:ctx' ~env e in
            Ok (Some t_e, t_e.TAst.ty)
        | None -> Ok (None, ctx.Ctx.builtin.Builtin.unit_)
      in

      let%bind subst =
        Typer.unify ~span ctx'.Ctx.subst ~from:t_t.TAst.ty ~to_:e_ty
      in
      Ctx.update_subst ctx' subst;
      Ctx.merge ctx ctx';

      let ty = Typing.Type.{ t_t.TAst.ty with span } in
      Ok TAst.{ kind = ExprIf (t_cond, t_t, t_e_opts); ty; span }
  (* *)
  | Ast.{ kind = ExprLoop e; span; _ } ->
      let%bind t_e = analyze ~ctx ~env e in

      let ty =
        let last_ty = ctx.Ctx.builtin.Builtin.unit_ in
        Typing.Type.{ last_ty with span }
      in
      let%bind subst =
        Typer.unify ~span ctx.Ctx.subst ~from:ty ~to_:t_e.TAst.ty
      in
      Ctx.update_subst ctx subst;

      Ok TAst.{ kind = ExprLoop t_e; ty; span }
  (* *)
  | Ast.{ kind = ExprBreak; span; _ } ->
      let ty =
        let last_ty = ctx.Ctx.builtin.Builtin.unit_ in
        Typing.Type.{ last_ty with span }
      in
      Ok TAst.{ kind = ExprBreak; ty; span }
  (* *)
  | Ast.{ kind = ExprAs { expr; ty_expr }; span; _ } ->
      let%bind ty = lookup_type ~env ~ctx ty_expr in

      let%bind t_expr = analyze ~ctx ~env expr in
      let%bind () =
        can_cast_to ~subst:ctx.Ctx.subst ~src:t_expr.TAst.ty ~dst:ty
      in

      Ok TAst.{ kind = ExprCast t_expr; ty; span }
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
        | _ -> failwith "[ICE] assign: not determined mut"
      in

      let%bind subst =
        let lhs_ty = t_lhs.TAst.ty in
        let rhs_ty = t_rhs.TAst.ty in
        Typer.unify ~span ctx.Ctx.subst ~from:lhs_ty ~to_:rhs_ty
      in
      Ctx.update_subst ctx subst;

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
        match Typing.Subst.subst_type ctx.Ctx.subst t_r.TAst.ty with
        | Typing.Type.{ ty = Array { elem; n }; _ } ->
            let binding_mut = t_r.TAst.ty.Typing.Type.binding_mut in
            Typing.Type.{ elem with binding_mut }
        | _ -> failwith "[ICE] not supported"
      in

      let index_ty = ctx.Ctx.builtin.Builtin.i32_ in
      let%bind t_index = analyze ~ctx ~env index in
      let%bind subst =
        Typer.unify ~span ctx.Ctx.subst ~from:index_ty ~to_:t_index.TAst.ty
      in
      Ctx.update_subst ctx subst;

      Ok TAst.{ kind = ExprIndex (t_r, t_index); ty = elem_ty; span }
  (* *)
  | Ast.{ kind = ExprRef (attr, e); span; _ } ->
      (* TODO: ref trait *)
      let%bind t_r = analyze ~ctx ~env e in

      (* coarsening *)
      let mut = Mut.mutability_of attr in
      let%bind () =
        match (mut, t_r.TAst.ty.Typing.Type.binding_mut) with
        (* OK: &mutable <- mutable value *)
        | (Typing.Type.MutMut, Typing.Type.MutMut)
        (* OK: &immutable <- immutable value *)
        | (Typing.Type.MutImm, Typing.Type.MutImm)
        (* OK: &immutable <- mutable value *)
        | (Typing.Type.MutImm, Typing.Type.MutMut) ->
            Ok ()
        (* NG: &mutable <- immutable value *)
        | (Typing.Type.MutMut, Typing.Type.MutImm) ->
            let e = new Reasons.cannot_reference_mut in
            let elm = Diagnostics.Elem.error ~span e in
            Error elm
        | _ -> failwith "[ICE] ref: not determined mut"
      in

      let ty =
        let elem_ty = t_r.TAst.ty in
        Typing.Type.{ (ctx.Ctx.builtin.Builtin.pointer_ mut elem_ty) with span }
      in
      Ok TAst.{ kind = ExprRef t_r; ty; span }
  (* *)
  | Ast.{ kind = ExprDeref e; span; _ } ->
      (* TODO: deref trait *)
      let%bind t_r = analyze ~ctx ~env e in

      let elem_ty = Typing.Subst.fresh_ty ~span ctx.Ctx.subst in
      let elem_mut = Typing.Subst.fresh_mut ctx.Ctx.subst in
      let ptr_ty =
        Typing.Type.
          { (ctx.Ctx.builtin.Builtin.pointer_ elem_mut elem_ty) with span }
      in
      let%bind subst =
        Typer.unify ~span ctx.Ctx.subst ~from:ptr_ty ~to_:t_r.TAst.ty
      in
      Ctx.update_subst ctx subst;

      let ty =
        let binding_mut = Typing.Subst.subst_mut subst elem_mut in
        Typing.Type.{ elem_ty with binding_mut; span }
      in
      Ok TAst.{ kind = ExprDeref t_r; ty; span }
  (* *)
  | Ast.{ kind = ExprStruct { path }; span; _ } ->
      let%bind ty =
        lookup_type ~env ~ctx path
        |> Result.map ~f:(Typing.Subst.subst_type ctx.Ctx.subst)
      in
      let%bind () =
        match ty with
        | Typing.Type.{ ty = Struct _; _ } -> Ok ()
        | _ ->
            let e = new Reasons.not_struct_type in
            let elm = Diagnostics.Elem.error ~span e in
            Error elm
      in
      Ok TAst.{ kind = ExprStruct; span; ty }
  (* *)
  | Ast.{ kind = Path { root; elems }; span; _ } as p ->
      let%bind (env, vars, ty, _) =
        Phase1_1.lookup_path ~env ~subst:ctx.Ctx.subst p
      in
      let%bind () =
        match Env.w_of env with Env.Val -> Ok () | _ -> failwith "[ICE]"
      in

      let () =
        (* debug log *)
        if List.length vars > 0 then
          [%loga.error
            "%s"
              ( vars
              |> List.map ~f:(fun (v, nv) ->
                     Printf.sprintf "%s -> %s" (Typing.Type.to_string v)
                       ( nv
                       |> Option.map ~f:Typing.Type.to_string
                       |> Option.value ~default:"NONE" ))
              |> String.concat ~sep:"," )]
      in

      let chain = Phase1_1.to_chains' env vars in
      Ok TAst.{ kind = Var2 { chain }; span; ty }
  (* *)
  | Ast.{ kind = ID name; span; _ } as i ->
      let path = Ast.{ kind = Path { root = i; elems = [] }; span } in
      analyze ~ctx ~env path
  (* *)
  | Ast.{ kind = LitBool v; span; _ } ->
      let ty = Typing.Type.{ ctx.Ctx.builtin.Builtin.bool_ with span } in
      Ok TAst.{ kind = LitBool v; ty; span }
  (* *)
  | Ast.{ kind = LitInt (value, bits, signed); span; _ } ->
      (* defauled to i32 *)
      let ty = Typing.Type.{ ctx.Ctx.builtin.Builtin.i32_ with span } in
      Ok TAst.{ kind = LitInt value; ty; span }
  (* *)
  | Ast.{ kind = LitString v; span; _ } ->
      let ty = Typing.Type.{ ctx.Ctx.builtin.Builtin.string_ with span } in
      Ok TAst.{ kind = LitString v; ty; span }
  (* *)
  | Ast.{ kind = LitArrayElems elems; span; _ } ->
      let%bind t_elems =
        List.fold_result elems ~init:[] ~f:(fun rev_t_elems elem ->
            let%bind t_elem = analyze ~ctx ~env elem in
            Ok (t_elem :: rev_t_elems))
        |> Result.map ~f:List.rev
      in

      let elem_ty = Typing.Subst.fresh_ty ~span ctx.Ctx.subst in
      let%bind subst =
        List.fold_result t_elems ~init:ctx.Ctx.subst ~f:(fun subst t_elem ->
            let span = t_elem.TAst.span in
            let binding_mut = Typing.Type.MutImm in
            let actual_elem_ty =
              Typing.Type.{ t_elem.TAst.ty with binding_mut }
            in
            Typer.unify ~span subst ~from:elem_ty ~to_:actual_elem_ty)
      in
      Ctx.update_subst ctx subst;

      let ty =
        let n = List.length elems in
        Typing.Type.{ (ctx.Ctx.builtin.Builtin.array_ elem_ty n) with span }
      in
      Ok TAst.{ kind = LitArrayElem t_elems; ty; span }
  (* *)
  | Ast.{ kind; span; _ } ->
      let s = Ast.sexp_of_kind_t kind |> Sexp.to_string_mach in
      let e =
        new Diagnostics.Reasons.internal_error
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

  let ret_ty = Typing.Subst.fresh_ty ~span ctx.Ctx.subst in
  let fn_ty =
    let linkage = Typing.Subst.fresh_linkage ctx.Ctx.subst in
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

  let%bind subst = Typer.unify ~span ctx.Ctx.subst ~from:recv_ty ~to_:fn_ty in
  Ctx.update_subst ctx subst;

  Ok TAst.{ kind = ExprCall (t_recv, t_args); ty = ret_ty; span }

and lookup_type ~env ~ctx ast =
  let builtin = ctx.Ctx.builtin in
  Phase1_1.lookup_type ~env builtin ast

and can_cast_to ~subst ~src ~dst =
  let src = Typing.Subst.subst_type subst src in
  let dst = Typing.Subst.subst_type subst dst in
  (* TODO: impl *)
  Ok ()
