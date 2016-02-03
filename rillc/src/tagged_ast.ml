module AstContext =
  struct
    type 'a current_ctx_t = ('a Env.env_t) option
    type 'a prev_ctx_t = Ast.ast
  end

module TaggedAst = Nodes.Make(AstContext)

include TaggedAst
