module Context =
  struct
    type t = unit
    type prev_ast_t = unit
  end

include Nodes.Make(Context)
