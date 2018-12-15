open! Base

let%expect_test "parser.simple01" =
  let r = Rillc.Parser.parse_from_file "simple.rill" in
  r
  |> [%sexp_of: (Rillc.Ast.t, string) Result.t]
  |> Expect_test_helpers_kernel.print_s;
  [%expect{|
    (Ok (
      (kind (
        Module (
          ((kind (
             ExternFunctionDeclStmt
             (name println)
             (ret_ty ((
               (kind (ID unit))
               (span (
                 Raw
                 ((lnum 1) (cnum 31) (bcnum 31))
                 ((lnum 1) (cnum 35) (bcnum 35)))))))
             (params ((
               (kind (ParamDecl (name s)))
               (span (
                 Raw
                 ((lnum 1) (cnum 19) (bcnum 19))
                 ((lnum 1) (cnum 28) (bcnum 28)))))))
             (symbol_name (
               (kind (LitString println))
               (span (
                 Raw
                 ((lnum 1) (cnum 38) (bcnum 38))
                 ((lnum 1) (cnum 47) (bcnum 47))))))))
           (span (
             Raw
             ((lnum 1) (cnum 0)  (bcnum 0))
             ((lnum 1) (cnum 47) (bcnum 47)))))
          ((kind (
             FunctionDefStmt
             (name main)
             (ret_ty ((
               (kind (ID unit))
               (span (
                 Raw
                 ((lnum 3) (cnum 62) (bcnum 12))
                 ((lnum 3) (cnum 66) (bcnum 16)))))))
             (params ())
             (body (
               (kind (
                 ExprCompound ((
                   (kind (
                     StmtExpr (
                       (kind (
                         ExprCall
                         ((kind (ID println))
                          (span (
                            Raw
                            ((lnum 4) (cnum 73) (bcnum 4))
                            ((lnum 4) (cnum 80) (bcnum 11)))))
                         ((
                           (kind (LitString hogehoge))
                           (span (
                             Raw
                             ((lnum 4) (cnum 81) (bcnum 12))
                             ((lnum 4) (cnum 91) (bcnum 22))))))))
                       (span (
                         Raw
                         ((lnum 4) (cnum 73) (bcnum 4))
                         ((lnum 4) (cnum 92) (bcnum 23)))))))
                   (span (
                     Raw
                     ((lnum 4) (cnum 73) (bcnum 4))
                     ((lnum 4) (cnum 93) (bcnum 24))))))))
               (span (
                 Raw
                 ((lnum 3) (cnum 67) (bcnum 17))
                 ((lnum 5) (cnum 95) (bcnum 1))))))))
           (span (
             Raw
             ((lnum 3) (cnum 50) (bcnum 0))
             ((lnum 5) (cnum 95) (bcnum 1))))))))
      (span (
        Raw
        ((lnum 1) (cnum 0)  (bcnum 0))
        ((lnum 5) (cnum 95) (bcnum 1)))))) |}]

let%expect_test "sema.simple01" =
  let r = Rillc.Parser.parse_from_file "simple.rill" in
  let (Ok ast) = r in
  let r = Rillc.Sema.sem ast in
  r
  |> Result.map ~f:(fun (t, _) -> t)
  |> [%sexp_of: (Rillc.Hir.t, Rillc.Diagnostics.t list) Result.t]
  |> Expect_test_helpers_kernel.print_s;
  [%expect{|
    (Ok (
      (kind (
        Module (
          ((kind (FunctionDeclStmt (name main)))
           (ty Function)
           (span (
             Raw
             ((lnum 3) (cnum 50) (bcnum 0))
             ((lnum 5) (cnum 95) (bcnum 1)))))
          ((kind (FunctionDeclStmt (name println)))
           (ty Function)
           (span (
             Raw
             ((lnum 1) (cnum 0)  (bcnum 0))
             ((lnum 1) (cnum 47) (bcnum 47)))))
          ((kind (
             FunctionDefStmt
             (name main)
             (body (
               (kind (
                 ExprCompound ((
                   (kind (
                     StmtExpr (
                       (kind (
                         ExprCall
                         ((kind (ID println))
                          (ty Module)
                          (span (
                            Raw
                            ((lnum 4) (cnum 73) (bcnum 4))
                            ((lnum 4) (cnum 80) (bcnum 11)))))
                         ((
                           (kind (LitString hogehoge))
                           (ty String)
                           (span (
                             Raw
                             ((lnum 4) (cnum 81) (bcnum 12))
                             ((lnum 4) (cnum 91) (bcnum 22))))))))
                       (ty Unit)
                       (span (
                         Raw
                         ((lnum 4) (cnum 73) (bcnum 4))
                         ((lnum 4) (cnum 92) (bcnum 23)))))))
                   (ty Unit)
                   (span (
                     Raw
                     ((lnum 4) (cnum 73) (bcnum 4))
                     ((lnum 4) (cnum 93) (bcnum 24))))))))
               (ty Module)
               (span (
                 Raw
                 ((lnum 3) (cnum 67) (bcnum 17))
                 ((lnum 5) (cnum 95) (bcnum 1))))))))
           (ty Function)
           (span (
             Raw
             ((lnum 3) (cnum 50) (bcnum 0))
             ((lnum 5) (cnum 95) (bcnum 1))))))))
      (ty Module)
      (span (
        Raw
        ((lnum 1) (cnum 0)  (bcnum 0))
        ((lnum 5) (cnum 95) (bcnum 1)))))) |}]

let%expect_test "sema.simple01" =
  let r = Rillc.Parser.parse_from_file "simple.rill" in
  let (Ok ast) = r in
  let (Ok (tnode, _)) = Rillc.Sema.sem ast in
  let r = Rillc.Rir.KNorm.generate tnode in
  r
  |> [%sexp_of: (Rillc.Rir.KNorm.t, unit) Result.t]
  |> Expect_test_helpers_kernel.print_s;
  [%expect]
