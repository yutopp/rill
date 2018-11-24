(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

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

let rec sem node =
  let node = Sema_forward_ref_solver.g node in
  sem' node (Context.empty ())

and sem_fold' nodes ctx =
  let ((ctx', errs_stack), nodes') =
    List.fold_map nodes
                  ~init:(ctx, [])
                  ~f:(fun (c, es) n -> match sem' n c with
                                       | Ok (n', c') -> ((c', es), n')
                                       | Error errs -> ((c, errs :: es), Hir.{kind = Empty; ty = Ty.Unknown})
                     )
  in
  match errs_stack with
  | [] -> Ok (nodes', ctx')
  | _  -> Error (errs_stack |> List.rev |> List.concat)

and sem' node ctx : ((Hir.t * Context.t), Diagnostics.t list) Result.t =
  match node.Ast.kind with
  | Ast.Module nodes ->
     let env = Context.get_env ctx in
     let menv = Env.create "" Env.Module (Some env) in
     begin match sem_fold' nodes ctx with
     | Ok (nodes', ctx') ->
        let env = Env.insert env menv in
        let ctx' = Context.set_env ctx' env in
        Ok (Hir.{kind = Module nodes'; ty = Ty.Module}, ctx')
     | Error errs ->
        Error errs
     end

  | Ast.FunctionDeclStmt {name; params; ret_ty} ->
     let env = Context.get_env ctx in
     let fenv = Env.create "" Env.Module (Some env) in
     Ok (Hir.{
           kind = FunctionDeclStmt {name};
           ty = Ty.Function;
         }, ctx)

  | Ast.ExternFunctionDeclStmt {name; params; ret_ty; symbol_name} ->
     let env = Context.get_env ctx in
     let fenv = Env.create name Env.Module (Some env) in
     let env = Env.insert env fenv in
     let ctx' = Context.set_env ctx env in
     Ok (Hir.{
           kind = FunctionDeclStmt {name};
           ty = Ty.Function;
         }, ctx')

  | Ast.FunctionDefStmt {name; params; ret_ty; body} ->
     let fctx = Context.create_env ctx in
     begin match sem' body fctx with
     | Ok (body', fctx') ->
        Ok (Hir.{
              kind = FunctionDefStmt {name; body = body'};
              ty = Ty.Function;
            }, ctx)
     | Error errs ->
        Error errs
     end

  | Ast.StmtExpr expr ->
     begin match sem' expr ctx with
     | Ok (expr', ctx') ->
        Ok (Hir.{kind = StmtExpr expr'; ty = Ty.Unit}, ctx')
     | Error errs ->
        Error errs
     end

  | Ast.ExprCompound exprs ->
     begin match sem_fold' exprs ctx with
     | Ok (exprs', ctx') ->
        Ok (Hir.{kind = ExprCompound exprs'; ty = Ty.Module}, ctx')
     | Error errs ->
        Error errs
     end

  | Ast.ExprCall (r, args) ->
     begin match sem' r ctx with
     | Ok (expr', ctx') ->
        Ok (Hir.{kind = StmtExpr expr'; ty = Ty.Unit}, ctx')
     | Error errs ->
        Error errs
     end

  | Ast.LitString s ->
     Ok (Hir.{kind = LitString s; ty = Ty.String}, ctx)

  | Ast.ID name ->
     let env = Context.get_env ctx in
     let env_res = Env.lookup env name in
     begin match env_res with
     | Ok env ->
        Ok (Hir.{kind = ID name; ty = Env.ty env}, ctx)
     | Error history ->
        let d =
          Diagnostics.create ~reason:(Diagnostics.Id_not_found (name))
                             ~span:node.Ast.span
        in
        Error [d]
     end

  | k ->
     failwith @@
       Printf.sprintf "Unknown node: %s"
                      (k |> Ast.sexp_of_kind |> Sexp.to_string_hum ~indent:2)
