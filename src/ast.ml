type ast =
    Module of ast

  (* statements *)
  | StatementList of ast list
  | ExprStatement of ast

  (* expressions *)
  | BinaryOpExpr of ast * string * ast
  | UnaryOpExpr of string * ast

  (* values *)
  | Id of string
  | Int32Lit of int

  (* error *)
  | Error
