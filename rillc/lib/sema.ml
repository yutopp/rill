(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

module Span = Common.Span
module Diagnostics = Common.Diagnostics
module Ast = Syntax.Ast

(*
module type BODY = sig
  type t
end

module Module (B : BODY) = struct
  type t = {
    toplevels: (string, B.t) Hashtbl.t;
  }
end
 *)

module Type = struct
  type var_t = int
  [@@deriving sexp]

  let next_var (v : var_t) : var_t =
    v + 1

  module Primitive = struct
    type t =
      | Var of var_t
      | Unit
      | Int
      | String
      | Func of t list * t
      | Bottom
    [@@deriving sexp]
  end
  include Primitive

  module Scheme = struct
    type t =
      | Scheme of var_t list * Primitive.t
    [@@deriving sexp]

    let of_ty ty =
      Scheme ([], ty)
  end
end

module Typer = struct
  type t = {
    mutable id: Type.var_t;
  }

  let create () =
    {
      id = 0;
    }

  let fresh_var typer : Type.var_t =
    let v = typer.id in
    typer.id <- Type.next_var typer.id;
    v

  let fresh_ty typer : Type.t =
    let v = fresh_var typer in
    Type.Var v

  module Subst = struct
    module IntMap = Map.M(Int)

    type t =
      (Type.t IntMap.t * int IntMap.t)

    let create () : t =
      let ty_subst = Map.empty (module Int) in
      let ki_subst = Map.empty (module Int) in
      (ty_subst, ki_subst)

    let rec subst_type (subst : t) ty =
      let (ty_subst, ki_subst) = subst in

      match ty with
      | Type.Var uni_id ->
         begin match Map.find ty_subst uni_id with
         | Some ty' -> subst_type subst ty'
         | None -> ty
         end

      | Type.Func (params, ret) ->
         let params' = List.map ~f:(subst_type subst) params in
         let ret' = subst_type subst ret in
         Type.Func (params', ret')

      | alt ->
         alt

    let subst_tysc (subst : t) tysc =
      match tysc with
      | Type.Scheme.(Scheme ([], tys)) ->
         let tys' = subst_type subst tys in
         Type.Scheme.Scheme ([], tys')

      | _ ->
         failwith ""

    let rec unify subst lhs_ty rhs_ty =
      let (ty_subst, ki_subst) = subst in

      let s_lhs_ty = subst_type subst lhs_ty in
      let s_rhs_ty = subst_type subst rhs_ty in
      match (s_lhs_ty, s_rhs_ty) with
      | Type.(Var a, Var b) when a <> b ->
         let ty_subst = Map.add_exn ty_subst ~key:a ~data:s_rhs_ty in
         (ty_subst, ki_subst)

      | Type.(Func (a_params, a_ret), Func (b_params, b_ret)) ->
         begin match (List.length a_params, List.length b_params) with
         | (an, bn) when an = bn ->
            let subst' =
              List.fold_left ~f:(fun subst (a_param, b_param) ->
                               unify subst a_param b_param)
                             ~init:subst
                             (List.zip_exn a_params b_params)
            in
            unify subst' a_ret b_ret

         | _ ->
            failwith ""
         end

      | Type.(Var v, ty')
      | Type.(ty', Var v) ->
         let ty_subst = Map.add_exn ty_subst ~key:v ~data:ty' in
         (ty_subst, ki_subst)

      | (lhs, rhs) when Poly.equal lhs rhs ->
         subst

      | _ ->
         failwith ""
  end
end

module Env = struct
  type t = {
    parent: t option;
    name: string;
    table: (string, t) Hashtbl.t;
    kind: kind;
    tysc: Type.Scheme.t option;
  }

  and kind =
    | Package
    | Module
    | Function
    | Var
    | Type of Type.t
    | Scope

  let create ?tysc name k p  =
    {
      parent = p;
      name = name;
      table = Hashtbl.create (module String);
      kind = k;
      tysc = tysc;
    }

  let insert p c =
    (* TODO: check duplication *)
    let _ = Hashtbl.add p.table ~key:c.name ~data:c in
    p

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
         let tysc' = Typer.Subst.subst_tysc subst tysc in
         let s = Type.Scheme.sexp_of_t tysc' in
         Sexp.to_string_hum s
      | None ->
         "<NONE>"
    in
    Stdio.printf "Env: ty = %s\n" s

end

module Context = struct
  type t = {
    env: Env.t;
  }

  let empty () =
    let env = Env.create "" Env.Module None in
    {
      env = env;
    }

  let create_env c =
    c

  let get_env ctx =
    ctx.env

  let set_env ctx env =
    {ctx with env = env}
end

(*

let rec sem_fold' nodes ctx span =
  let (ctx', nodes_res_rev) =
    List.fold_left nodes
                   ~init:(ctx, [])
                   ~f:(fun (c, rs) n ->
                        match sem' n c with
                        | Ok (n', c') -> (c', (Either.First n') :: rs)
                        | Error errs -> (c, (Either.Second errs) :: rs))
  in
  match List.exists nodes_res_rev ~f:Either.is_second with
  | true  ->
     let ds = nodes_res_rev |> List.filter_map ~f:Either.Second.to_option |> List.rev in
     let reason = Diagnostics.Multiple ds in
     Error (Diagnostics.create ~reason ~span ~phase:Diagnostics.PhaseSema)
  | false ->
     Ok (nodes_res_rev |> List.filter_map ~f:Either.First.to_option |> List.rev, ctx')




and sem' node ctx : ((Hir.t * Context.t), Diagnostics.t) Result.t =
  match node with
  | Ast.{kind = Module nodes; span} ->
     let env = Context.get_env ctx in

     let kind = Env.Module in
     let menv = Env.create "" kind (Some env) in
     begin match sem_fold' nodes ctx span with
     | Ok (nodes', ctx') ->
        let env = Env.insert env menv in
        let ctx' = Context.set_env ctx' env in
        Ok (Hir.{kind = Module nodes'; ty = Ty.Module; span}, ctx')
     | Error d ->
        Error d
     end

  | Ast.{kind = FunctionDeclStmt {name; params; ret_ty}; span} ->
     let env = Context.get_env ctx in

     let kind = Env.Function in
     let fenv = Env.create name kind (Some env) in
     Ok (Hir.{
           kind = Empty; (* Forward decls are no longer required *)
           ty = Ty.Function;
           span;
         }, ctx)

  | Ast.{kind = ExternFunctionDeclStmt {name; params; ret_ty; symbol_name}; span} ->
     let env = Context.get_env ctx in

     let kind = Env.Function in
     let fenv = Env.create name kind (Some env) in
     let env = Env.insert env fenv in
     let ctx' = Context.set_env ctx env in
     Ok (Hir.{
           kind = ExternFunctionDeclStmt {name};
           ty = Ty.Function;
           span;
         }, ctx')

  | Ast.{kind = FunctionDefStmt {name; params; ret_ty; body}; span} ->
     let fctx = Context.create_env ctx in
     begin match sem' body fctx with
     | Ok (body', fctx') ->
        Ok (Hir.{
              kind = FunctionDefStmt {name; body = body'};
              ty = Ty.Function;
              span;
            }, ctx)
     | Error d ->
        Error d
     end

  | Ast.{kind = StmtExpr expr; span} ->
     begin match sem' expr ctx with
     | Ok (expr', ctx') ->
        Ok (Hir.{kind = StmtExpr expr'; ty = Ty.Unit; span}, ctx')
     | Error d ->
        Error d
     end

  | Ast.{kind = ExprCompound exprs; span} ->
     begin match sem_fold' exprs ctx span with
     | Ok (exprs', ctx') ->
        Ok (Hir.{kind = ExprCompound exprs'; ty = Ty.Module; span}, ctx')
     | Error d ->
        Error d
     end

  | Ast.{kind = ExprCall (r, args); span} ->
     begin match sem' r ctx with
     | Ok (r', ctx') ->
        begin match sem_fold' args ctx' span with
        | Ok (args', ctx') ->
           Ok (Hir.{kind = ExprCall (r', args'); ty = Ty.Unit; span}, ctx')
        | Error d ->
           Error d
        end
     | Error d ->
        Error d
     end

  | Ast.{kind = LitString s; span} ->
     Ok (Hir.{kind = LitString s; ty = Ty.String; span}, ctx)

  | Ast.{kind = ID name; span} ->
     let env = Context.get_env ctx in
     let env_res = Env.lookup env name in
     begin match env_res with
     | Ok env ->
        Ok (Hir.{kind = ID name; ty = Env.ty env; span}, ctx)
     | Error history ->
        let reason = Diagnostics.Id_not_found name in
        Error (Diagnostics.create ~reason ~span ~phase:Diagnostics.PhaseSema)
     end

  | k ->
     failwith @@
       Printf.sprintf "Unknown node: %s"
                      (k |> Ast.sexp_of_t |> Sexp.to_string_hum ~indent:2)

let sem node =
  let node = Sema_forward_ref_solver.g node in
  sem' node (Context.empty ())
 *)

module Module = struct
  type t = {
    env: Env.t;
    toplevels: (string, body_t) Hashtbl.t;
    typer: Typer.t
  }
  and body_t =
    (Ast.t * Env.t)

  let create menv =
    {
      env = menv;
      toplevels = Hashtbl.create (module String);
      typer = Typer.create ();
    }

  let insert m name body =
    (* TODO: check duplication *)
    let _ = Hashtbl.add m.toplevels ~key:name ~data:body in
    ()

  let show m subst =
    Hashtbl.iteri ~f:(fun ~key ~data ->
                    Stdio.printf "Key: %s\n" key;
                    let (_, env) = data in
                    Env.show env subst;
                    ())
                  m.toplevels
end

let rec collect_toplevels m dm ast penv : Diagnostics.Multi.t =
  match ast with
  | Ast.{kind = FunctionDeclStmt {name; params; ret_ty; _}; _}
  | Ast.{kind = ExternFunctionDeclStmt {name; params; ret_ty; _}; _}
  | Ast.{kind = FunctionDefStmt {name; params; ret_ty; _}; _} ->
     (* a,... -> ret *)
     let params_tys = List.map ~f:(fun _ -> Typer.fresh_ty m.Module.typer) params in
     let ret_ty = Typer.fresh_ty m.Module.typer in
     let fn_ty = Type.Primitive.Func (params_tys, ret_ty) in
     let fn_tysc = Type.Scheme.of_ty fn_ty in

     let kind = Env.Function in
     let fenv = Env.create name kind (Some penv) ~tysc:fn_tysc in

     (* parameters *)
     List.iter ~f:(fun (param, ty) ->
                 let name = match param with
                   | Ast.{kind = ParamDecl {name; _}; _} -> name
                   | _ -> failwith ""
                 in
                 let kind = Env.Var in
                 let venv = Env.create name kind (Some fenv) ~tysc:(Type.Scheme.of_ty ty) in
                 let _ = Env.insert fenv venv in
                 ())
               (List.zip_exn params params_tys);

     (* register the function to the parent env *)
     let _ = Env.insert penv fenv in

     let () = Module.insert m name (ast, fenv) in

     dm

  | _ ->
     failwith ""

let collect_toplevels dm ast penv =
  match ast with
  | Ast.{kind = Module nodes; span} ->
     let kind = Env.Module in
     let menv = Env.create "" kind (Some penv) in

     let m = Module.create menv in
     let dm =
       List.fold_left ~f:(fun dm' node ->
                        collect_toplevels m dm node menv
                      )
                      ~init:dm
                      nodes
     in

     let _ = Env.insert penv menv in

     (m, dm)

  | _ ->
     failwith ""

let solve_type ast env subst =
  let open Result.Let_syntax in
  match ast with
  | Ast.{kind = ID name; span; _} ->
     let%bind e =
       Env.lookup env name
       |> Result.map_error ~f:(fun trace ->
                             let reason = Diagnostics.Id_not_found name in
                             let d =
                               Diagnostics.create ~phase:Diagnostics.PhaseSema
                                                  ~span
                                                  ~reason
                             in
                             d
                           )
     in
     begin match e.Env.kind with
     | Env.Type ty ->
        let ty' = Typer.Subst.subst_type subst ty in
        ty' |> return

     | _ ->
        failwith ""
     end

  | _ ->
     failwith ""

let type_toplevels m (ast, env) penv subst =
  match ast with
  | Ast.{kind = FunctionDeclStmt {name; params; ret_ty = Some ret_ty_spec; _}; _}
  | Ast.{kind = ExternFunctionDeclStmt {name; params; ret_ty = Some ret_ty_spec; _}; _}
  | Ast.{kind = FunctionDefStmt {name; params; ret_ty = Some ret_ty_spec; _}; _} ->
     let (params_tys, ret_ty) =
       let tysc = Option.value_exn env.Env.tysc in
       (* TODO: check generics *)
       let Type.Scheme.Scheme (_, ty) = tysc in
       match ty with
       | Type.Func (params, ret) -> (params, ret)
       | _ -> failwith "[ICE]"
     in
     let subst =
       List.fold_left
         ~f:(fun subst (param, param_ty) ->
           let ty_spec =
             match param with
             | Ast.{kind = ParamDecl {ty_spec; _}; _} -> ty_spec
             | _ -> failwith "[ICE]"
           in
           match solve_type ty_spec penv subst with
           | Ok ty ->
              Typer.Subst.unify subst param_ty ty
           | Error d ->
              (* TODO: fix *)
              failwith (Diagnostics.to_string d)
         )
         ~init:subst
         (List.zip_exn params params_tys)
     in
     let subst = match solve_type ret_ty_spec penv subst with
       | Ok ty ->
          Typer.Subst.unify subst ret_ty ty
       | Error d ->
          (* TODO: fix *)
          failwith (Diagnostics.to_string d)
     in
     subst

  | _ ->
     subst

let unify_toplevels m : Typer.Subst.t =
  let subst = Typer.Subst.create () in
  let menv = m.Module.env in

  let subst =
    Hashtbl.fold ~f:(fun ~key ~data subst ->
                   type_toplevels m data menv subst)
                 ~init:subst
                 m.Module.toplevels

  in
  subst

let show_module m (subst : Typer.Subst.t) =
  Module.show m subst
