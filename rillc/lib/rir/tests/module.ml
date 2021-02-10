(*
 * Copyright yutopp 2021 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

let test_empty_module () =
  let ctx = Rir.Context.create () in
  let m = Rir.Module.create ~ctx in
  ()

let () =
  let open Alcotest in
  run "module"
    [ ("create", [ test_case "empty module" `Quick test_empty_module ]) ]
