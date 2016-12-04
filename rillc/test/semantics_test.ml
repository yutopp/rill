open OUnit2
open Batteries

let test_test_1 test_ctxt =
  let root = Env.make_root_env() in
  let env = Env.create_context_env root Env.Unknown None in
  let envs = Env.collect_dependee_envs env in
  let f a b =
    assert_equal ~cmp:(==) a b;
  in
  List.iter2 f envs [env; root]

let test_lifetime_1 test_ctxt =
  let a_id = Lifetime.Var_id.generate () in
  let a = Lifetime.LtVarPlaceholder (a_id, Loc.dummy) in

  let b_id = Lifetime.Var_id.generate () in
  let b = Lifetime.LtVarPlaceholder (b_id, Loc.dummy) in

  let c_id = Lifetime.Var_id.generate () in
  let c = Lifetime.LtVarPlaceholder (c_id, Loc.dummy) in

  (* <a, b, c> / a: b, b: c *)
  (* live duration: a >= b, b >= c *)
  (* value representation: a <= b, b <= c *)
  let env_id = Env_system.EnvId.E (Env_system.EnvUniqId.generate (), None) in
  let res = Lifetime.convert [a; b; c] [(a, b); (b, c)] env_id in

  let expect =
    [
      Lifetime.LtVar (a_id, Id_string.Pure "", env_id, Int32.of_int 0, [a_id; b_id; c_id], Loc.dummy);
      Lifetime.LtVar (b_id, Id_string.Pure "", env_id, Int32.of_int 1, [b_id; c_id], Loc.dummy);
      Lifetime.LtVar (c_id, Id_string.Pure "", env_id, Int32.of_int 2, [c_id], Loc.dummy)
    ]
  in
  assert_equal expect res

let test_lifetime_2 test_ctxt =
  let a_id = Lifetime.Var_id.generate () in
  let a = Lifetime.LtVarPlaceholder (a_id, Loc.dummy) in

  let b_id = Lifetime.Var_id.generate () in
  let b = Lifetime.LtVarPlaceholder (b_id, Loc.dummy) in

  let c_id = Lifetime.Var_id.generate () in
  let c = Lifetime.LtVarPlaceholder (c_id, Loc.dummy) in

  (* <a, b, c> / a: b, b: c, c: a *)
  (* live duration: a >= b, b >= c, c >= a *)
  (* value representation: a <= b, b <= c, c <= a *)
  let env_id = Env_system.EnvId.E (Env_system.EnvUniqId.generate (), None) in
  let res = Lifetime.convert [a; b; c] [(a, b); (b, c); (c, a)] env_id in

  let expect =
    [
      Lifetime.LtVar (a_id, Id_string.Pure "", env_id, Int32.of_int 0, [b_id; c_id; a_id], Loc.dummy);
      Lifetime.LtVar (b_id, Id_string.Pure "", env_id, Int32.of_int 0, [b_id; c_id; a_id], Loc.dummy);
      Lifetime.LtVar (c_id, Id_string.Pure "", env_id, Int32.of_int 0, [b_id; c_id; a_id], Loc.dummy)
    ]
  in
  assert_equal expect res

let test_lifetime_3 test_ctxt =
  let a_id = Lifetime.Var_id.generate () in
  let a = Lifetime.LtVarPlaceholder (a_id, Loc.dummy) in

  let b_id = Lifetime.Var_id.generate () in
  let b = Lifetime.LtVarPlaceholder (b_id, Loc.dummy) in

  let c_id = Lifetime.Var_id.generate () in
  let c = Lifetime.LtVarPlaceholder (c_id, Loc.dummy) in

  (* <a, b> / a: c, b: c *)
  (* live duration: a >= c, b >= c *)
  (* value representation: a <= c, b <= c *)
  let env_id = Env_system.EnvId.E (Env_system.EnvUniqId.generate (), None) in
  let res = Lifetime.convert [a; b] [(a, c); (b, c)] env_id in

  let expect =
    [
      Lifetime.LtVar (a_id, Id_string.Pure "", env_id, Int32.of_int 1, [a_id; c_id], Loc.dummy);
      Lifetime.LtVar (b_id, Id_string.Pure "", env_id, Int32.of_int 0, [b_id; c_id], Loc.dummy);
    ]
  in
  assert_equal expect res

let test_lifetime_4 test_ctxt =
  let a_id = Lifetime.Var_id.generate () in
  let a = Lifetime.LtVarPlaceholder (a_id, Loc.dummy) in

  let b_id = Lifetime.Var_id.generate () in
  let b = Lifetime.LtVarPlaceholder (b_id, Loc.dummy) in

  (* <a, b> / b: a *)
  (* live duration: b >= a *)
  (* value representation: b <= a *)
  let env_id = Env_system.EnvId.E (Env_system.EnvUniqId.generate (), None) in
  let res = Lifetime.convert [a; b] [(b, a)] env_id in

  Printf.printf "= 4\n";
  res |> List.iter (fun lt -> Printf.printf "-> %s\n" (Lifetime.to_string lt));

  let expect =
    [
      Lifetime.LtVar (a_id, Id_string.Pure "", env_id, Int32.of_int 1, [a_id], Loc.dummy);
      Lifetime.LtVar (b_id, Id_string.Pure "", env_id, Int32.of_int 0, [b_id; a_id], Loc.dummy);
    ]
  in
  assert_equal expect res

let test_lifetime_4_1 test_ctxt =
  let a_id = Lifetime.Var_id.generate () in
  let a = Lifetime.LtVarPlaceholder (a_id, Loc.dummy) in

  let b_id = Lifetime.Var_id.generate () in
  let b = Lifetime.LtVarPlaceholder (b_id, Loc.dummy) in

  (* <b, a> / b: a *)
  (* live duration: b >= a *)
  (* value representation: b <= a *)
  let env_id = Env_system.EnvId.E (Env_system.EnvUniqId.generate (), None) in
  let res = Lifetime.convert [b; a] [(b, a)] env_id in

  Printf.printf "= 4.1\n";
  res |> List.iter (fun lt -> Printf.printf "-> %s\n" (Lifetime.to_string lt));

  let expect =
    [
      Lifetime.LtVar (b_id, Id_string.Pure "", env_id, Int32.of_int 0, [b_id; a_id], Loc.dummy);
      Lifetime.LtVar (a_id, Id_string.Pure "", env_id, Int32.of_int 1, [a_id], Loc.dummy);
    ]
  in
  assert_equal expect res

let test_lifetime_5 test_ctxt =
  let a_id = Lifetime.Var_id.generate () in
  let a = Lifetime.LtVarPlaceholder (a_id, Loc.dummy) in

  let b_id = Lifetime.Var_id.generate () in
  let b = Lifetime.LtVarPlaceholder (b_id, Loc.dummy) in

  let c_id = Lifetime.Var_id.generate () in
  let c = Lifetime.LtVarPlaceholder (c_id, Loc.dummy) in

  (* <a: b, b, c> / a: b *)
  (* live duration: a >= b *)
  (* value representation: a <= b *)
  let env_id = Env_system.EnvId.E (Env_system.EnvUniqId.generate (), None) in
  let res = Lifetime.convert [a; b; c] [(a, b)] env_id in

  Printf.printf "= 5\n";
  res |> List.iter (fun lt -> Printf.printf "-> %s\n" (Lifetime.to_string lt));

  let expect =
    [
      Lifetime.LtVar (a_id, Id_string.Pure "", env_id, Int32.of_int 0, [a_id; b_id], Loc.dummy);
      Lifetime.LtVar (b_id, Id_string.Pure "", env_id, Int32.of_int 2, [b_id], Loc.dummy);
      Lifetime.LtVar (c_id, Id_string.Pure "", env_id, Int32.of_int 1, [c_id], Loc.dummy);
    ]
  in
  assert_equal expect res

let rec suite =
  "semantics_suite" >:::
    [
      "test_test_1" >:: test_test_1;
      "lifetime" >::: [
          "lifetime_1" >:: test_lifetime_1;
          "lifetime_2" >:: test_lifetime_2;
          "lifetime_3" >:: test_lifetime_3;
          "lifetime_4" >:: test_lifetime_4;
          "lifetime_4_1" >:: test_lifetime_4_1;
          "lifetime_5" >:: test_lifetime_5;
        ]
    ]

let () =
  run_test_tt_main suite
