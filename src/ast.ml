module Context =
  struct
    type 'a t = unit
    type 'a prev_ast_t = unit
  end

include Nodes.Make(Context)
