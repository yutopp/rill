module Context =
  struct
    type 'a current_ctx_t = unit
    type 'a prev_ctx_t = unit
  end

include Nodes.Make(Context)
