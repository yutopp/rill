(*
 * Copyright yutopp 2019 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

module Span = Common.Span
module Diagnostics = Common.Diagnostics
module Ast = Syntax.Ast

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

     let kind = Env.Kind.Function in
     let fenv = Env.create name kind (Some penv) ~tysc:fn_tysc in

     (* parameters *)
     List.iter ~f:(fun (param, ty) ->
                 let name = match param with
                   | Ast.{kind = ParamDecl {name; _}; _} -> name
                   | _ -> failwith ""
                 in
                 let kind = Env.Kind.Var in
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
     let kind = Env.Kind.Module in
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
       |> Result.map_error
            ~f:(fun trace ->
              let reason = new Common.Reasons.id_not_found ~name in
              let d =
                Diagnostics.create ~phase:Diagnostics.PhaseSema
                                   ~span
                                   ~reason
              in
              d
            )
     in
     begin match e.Env.kind with
     | Env.Kind.Type ty ->
        let ty' = Typer.subst_type subst ty in
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
              begin match Typer.unify subst param_ty ty with
              | Ok v -> v
              | Error _ ->
                 (* TODO: fix *)
                 failwith ""
              end

           | Error d ->
              (* TODO: fix *)
              failwith (Diagnostics.to_string d)
         )
         ~init:subst
         (List.zip_exn params params_tys)
     in
     let subst = match solve_type ret_ty_spec penv subst with
       | Ok ty ->
          begin match Typer.unify subst ret_ty ty with
          | Ok v -> v
          | Error _ ->
             (* TODO: fix *)
             failwith ""
          end
       | Error d ->
          (* TODO: fix *)
          failwith (Diagnostics.to_string d)
     in
     subst

  | _ ->
     subst

let unify_toplevels m : Typer.t =
  let subst = m.Module.typer in
  let menv = m.Module.env in

  let subst =
    Hashtbl.fold ~f:(fun ~key ~data subst ->
                   type_toplevels m data menv subst)
                 ~init:subst
                 m.Module.toplevels

  in
  subst

let show_module m (subst : Typer.t) =
  Module.iteri ~f:(fun ~key ~data ->
                 Stdio.printf "Key: %s\n" key;
                 let (_, env) = data in
                 Env.show env subst;
                 ())
               m
