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

module Env = struct
  type t = {
    parent: t option;
    name: string;
    table: (string, t) Hashtbl.t;
    kind: kind;
    ty: Hir.Ty.t;
  }

  and kind =
    | Module
    | Function
    | Scope

  let create name k p =
    {
      parent = p;
      name = name;
      table = Hashtbl.create (module String);
      kind = k;
      ty = Hir.Ty.Module;
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

  let ty env =
    env.ty
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
