open! Base

let parse_expr line =
  let ds = Rillc.Diagnostics.create () in
  let lexbuf = Lexing.from_string line in
  let sup = Rillc.Syntax.Supplier.create ~path:"" ~lexbuf in
  let (node, _) =
    Rillc.Syntax.Entry.from_expr ~sup ~ds
    |> Result.map_error ~f:Rillc.Diagnostics.Elem.to_string_human
    |> Result.ok_or_failwith
  in
  node

let%expect_test _ =
  let node = parse_expr "1 + 2 + 3" in
  Rillc.Syntax.Ast.show node |> Stdio.print_string;
  [%expect
    {|
    { Ast.kind =
      Ast.ExprBinaryOp {op = { Ast.kind = (Ast.ID "+"); span =  };
        lhs =
        { Ast.kind =
          Ast.ExprBinaryOp {op = { Ast.kind = (Ast.ID "+"); span =  };
            lhs = { Ast.kind = (Ast.LitInt (1, 32, true)); span =  };
            rhs = { Ast.kind = (Ast.LitInt (2, 32, true)); span =  }};
          span =  };
        rhs = { Ast.kind = (Ast.LitInt (3, 32, true)); span =  }};
      span =  } |}]

let%expect_test _ =
  let node = parse_expr "1 + 2 * 3" in
  let node = Rillc.Sema.Operators.reconstruct node in
  Rillc.Syntax.Ast.show node |> Stdio.print_string;
  [%expect
    {|
    { Ast.kind =
      Ast.ExprBinaryOp {op = { Ast.kind = (Ast.ID "+"); span =  };
        lhs = { Ast.kind = (Ast.LitInt (1, 32, true)); span =  };
        rhs =
        { Ast.kind =
          Ast.ExprBinaryOp {op = { Ast.kind = (Ast.ID "*"); span =  };
            lhs = { Ast.kind = (Ast.LitInt (2, 32, true)); span =  };
            rhs = { Ast.kind = (Ast.LitInt (3, 32, true)); span =  }};
          span =  }};
      span =  } |}]

let%expect_test _ =
  let node = parse_expr "1 * 2 + 3 * 4" in
  let node = Rillc.Sema.Operators.reconstruct node in
  Rillc.Syntax.Ast.show node |> Stdio.print_string;
  [%expect
    {|
    { Ast.kind =
      Ast.ExprBinaryOp {op = { Ast.kind = (Ast.ID "+"); span =  };
        lhs =
        { Ast.kind =
          Ast.ExprBinaryOp {op = { Ast.kind = (Ast.ID "*"); span =  };
            lhs = { Ast.kind = (Ast.LitInt (1, 32, true)); span =  };
            rhs = { Ast.kind = (Ast.LitInt (2, 32, true)); span =  }};
          span =  };
        rhs =
        { Ast.kind =
          Ast.ExprBinaryOp {op = { Ast.kind = (Ast.ID "*"); span =  };
            lhs = { Ast.kind = (Ast.LitInt (3, 32, true)); span =  };
            rhs = { Ast.kind = (Ast.LitInt (4, 32, true)); span =  }};
          span =  }};
      span =  } |}]
