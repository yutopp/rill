(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

let rec g node =
  let (node_opt, forward_refs) = g' node [] in
  match node_opt with
  | Some (Ast.{kind = Module nodes; _} as node') ->
     Ast.{node' with kind = Ast.Module (forward_refs @ nodes)}
  | Some node' ->
     node'
  | None ->
     node

and g' node forward_refs : (Ast.t option * Ast.t list)=
  match node.Ast.kind with
  | Ast.Module nodes ->
     let (forward_refs', nodes') =
       List.fold_map nodes
                     ~init:forward_refs
                     ~f:(fun refs n -> let (n', refs') = g' n refs in (refs', n'))
     in
     let nodes' = List.filter_map nodes' ~f:(fun x -> x) in
     (Some Ast.{node with kind = Ast.Module nodes'}, forward_refs')

  | Ast.ExternFunctionDeclStmt _ ->
     (None, node :: forward_refs)

  | Ast.FunctionDefStmt {name; ret_ty; params; _} ->
     (* Create a declaration for this definition *)
     let decl =
       Ast.{
         kind = Ast.FunctionDeclStmt {name; ret_ty; params};
         span = node.span;
       }
     in
     (Some node, decl :: forward_refs)

  | k ->
     failwith @@
       Printf.sprintf "Unknown node: %s"
                      (k |> Ast.sexp_of_kind |> Sexp.to_string_hum ~indent:2)
