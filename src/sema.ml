
let make_default_env () =
  Env.create_root_env ()


module AstContext =
  struct
    type t = Env.t
    type prev_ast_t = Ast.ast
  end

module TaggedAst = Nodes.Make(AstContext)


let rec pre_fetch node parent_env =
  match node with
  | Ast.Module (inner, _) ->
     begin
       let name = "module" in   (* "module" is a temporary name *)
       let env = Env.Module (Env.create_common parent_env, {
                                Env.mod_name = name;
                              }) in
       Env.add name env parent_env;

       let res_node = pre_fetch inner env in
       TaggedAst.Module (res_node, env)
     end

  | Ast.StatementList (nodes) ->
     let tagged_nodes = nodes |> List.map (fun n -> pre_fetch n parent_env) in
     TaggedAst.StatementList tagged_nodes

  | Ast.FunctionDefStmt (name, parameters, body, _) ->
     begin
       let base_env = Env.find_or_create_multi_env parent_env name Env.Kind.Function in
       let fenv = Env.Function (Env.create_common parent_env) in
       Env.MultiSetOp.add_candidates base_env fenv;

       TaggedAst.FunctionDefStmt (name, parameters, TaggedAst.PrevPassNode body, fenv)
     end

  | _ -> failwith ""


let rec analyze node env =
  pre_fetch node env
