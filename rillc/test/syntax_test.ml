open OUnit2

let test_test_1 test_ctxt =
  assert_equal true true

let rec suite =
  "syntax_suite" >:::
    [
      "test_test_1" >:: test_test_1
    ]

let () =
  run_test_tt_main suite
