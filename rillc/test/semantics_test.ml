open OUnit2
open Batteries
open Stdint

let test_test_1 test_ctxt =
  let root = Env.make_root_env() in
  let env = Env.create_context_env root Env.Unknown None in
  let envs = Env.collect_dependee_envs env in
  let f a b =
    assert_equal ~cmp:(==) a b;
  in
  List.iter2 f envs [env; root]

let test_class_align =
  let assert_offsets expect result =
    let open Uint32 in
    assert_equal ~printer:(fun l -> l |> List.map to_string |> String.join ",")
                 (expect |> List.map of_int) result
  in

  let test_1 test_ctxt =
    let open Uint32 in
    (*
      class { // (size: 8, align: 4)
        int32 a; // (size: 4, align: 4, offset: 0)
        int32 b; // (size: 4, align: 4, offset: 4)
      }
     *)
    let member_layouts =
      [
        (of_int 4, of_int 4);
        (of_int 4, of_int 4);
      ]
    in
    let (csize, calign, offsets_of) =
      Sema_definitions.calc_class_layouts member_layouts
    in
    assert_equal ~printer:to_string (of_int 4) calign;
    assert_equal ~printer:to_string (of_int 8) csize;
    assert_offsets [0; 4] offsets_of;
  in

  let test_2 test_ctxt =
    let open Uint32 in
    (*
      class { // (size: 16, align: 8)
        int32 a;  // (size: 4, align: 4, offset: 0)
        ...(padding: 4bytes)...
        double b; // (size: 8, align: 8, offset: 8)
      }
     *)
    let member_layouts =
      [
        (of_int 4, of_int 4);
        (of_int 8, of_int 8);
      ]
    in
    let (csize, calign, offsets_of) =
      Sema_definitions.calc_class_layouts member_layouts
    in
    assert_equal ~printer:to_string (of_int 8) calign;
    assert_equal ~printer:to_string (of_int 16) csize;
    assert_offsets [0; 8] offsets_of;
  in

  let test_3 test_ctxt =
    let open Uint32 in
    (*
      class { // (size: 16, align: 8)
        double a; // (size: 8, align: 8, offset: 0)
        int32 b;  // (size: 4, align: 4, offset: 8)
        ...(padding: 4bytes)...
      }
     *)
    let member_layouts =
      [
        (of_int 8, of_int 8);
        (of_int 4, of_int 4);
      ]
    in
    let (csize, calign, offsets_of) =
      Sema_definitions.calc_class_layouts member_layouts
    in
    assert_equal ~printer:to_string (of_int 8) calign;
    assert_equal ~printer:to_string (of_int 16) csize;
    assert_offsets [0; 8] offsets_of;
  in

  let test_4 test_ctxt =
    let open Uint32 in
    (*
      class { // (size: 16, align: 8)
        double a; // (size: 8, align: 8, offset: 0)
        int32 b;  // (size: 4, align: 4, offset: 8)
        char c;   // (size: 1, align: 1, offset: 12)
        ...(padding: 3bytes)...
      }
     *)
    let member_layouts =
      [
        (of_int 8, of_int 8);
        (of_int 4, of_int 4);
        (of_int 1, of_int 1);
      ]
    in
    let (csize, calign, offsets_of) =
      Sema_definitions.calc_class_layouts member_layouts
    in
    assert_equal ~printer:to_string (of_int 8) calign;
    assert_equal ~printer:to_string (of_int 16) csize;
    assert_offsets [0; 8; 12] offsets_of;
  in

  let test_5 test_ctxt =
    let open Uint32 in
    (*
      class { // (size: 24, align: 8)
        int32 a;  // (size: 4, align: 4, offset: 0)
        ...(padding: 4bytes)...
        double b; // (size: 8, align: 8, offset: 8)
        char c;   // (size: 1, align: 1, offset: 16)
        ...(padding: 7bytes)...
      }
     *)
    let member_layouts =
      [
        (of_int 4, of_int 4);
        (of_int 8, of_int 8);
        (of_int 1, of_int 1);
      ]
    in
    let (csize, calign, offsets_of) =
      Sema_definitions.calc_class_layouts member_layouts
    in
    assert_equal ~printer:to_string (of_int 8) calign;
    assert_equal ~printer:to_string (of_int 24) csize;
    assert_offsets [0; 8; 16] offsets_of;
  in

  let test_6 test_ctxt =
    let open Uint32 in
    (*
      class { // (size: 24, align: 8)
        int32 a;  // (size: 4, align: 4, offset: 0)
        ...(padding: 4bytes)...
        double b; // (size: 8, align: 8, offset: 8)
        char c;   // (size: 1, align: 1, offset: 16)
        char d;   // (size: 1, align: 1, offset: 17)
        ...(padding: 6bytes)...
      }
     *)
    let member_layouts =
      [
        (of_int 4, of_int 4);
        (of_int 8, of_int 8);
        (of_int 1, of_int 1);
        (of_int 1, of_int 1);
      ]
    in
    let (csize, calign, offsets_of) =
      Sema_definitions.calc_class_layouts member_layouts
    in
    assert_equal ~printer:to_string (of_int 8) calign;
    assert_equal ~printer:to_string (of_int 24) csize;
    assert_offsets [0; 8; 16; 17] offsets_of;
  in

  let test_7 test_ctxt =
    let open Uint32 in
    (*
      class { // (size: 24, align: 8)
        int32 a;  // (size: 4, align: 4, offset: 0)
        ...(padding: 4bytes)...
        double b; // (size: 8, align: 8, offset: 8)
        char c;   // (size: 1, align: 1, offset: 16)
        char d;   // (size: 1, align: 1, offset: 17)
        ...(padding: 2bytes)...
        int32 e;  // (size: 4, align: 4, offset: 20)
        ...(padding: 4bytes)...
      }
     *)
    let member_layouts =
      [
        (of_int 4, of_int 4);
        (of_int 8, of_int 8);
        (of_int 1, of_int 1);
        (of_int 1, of_int 1);
        (of_int 4, of_int 4);
      ]
    in
    let (csize, calign, offsets_of) =
      Sema_definitions.calc_class_layouts member_layouts
    in
    assert_equal ~printer:to_string (of_int 8) calign;
    assert_equal ~printer:to_string (of_int 24) csize;
    assert_offsets [0; 8; 16; 17; 20] offsets_of;
  in

  [
    "class_align_1"   >:: test_1;
    "class_align_2"   >:: test_2;
    "class_align_3"   >:: test_3;
    "class_align_4"   >:: test_4;
    "class_align_5"   >:: test_5;
    "class_align_6"   >:: test_6;
    "class_align_7"   >:: test_7;
  ]

let test_lifetime =
  let assert_equal_lifetimes expect result =
    let to_s lts =
      lts
      |> List.map Lifetime.to_string
      |> String.join ","
    in
    assert_equal ~printer:to_s expect result
  in

  let test_1 test_ctxt =
    Lifetime.Var_id.reset ();

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
    assert_equal_lifetimes expect res
  in

  let test_2 test_ctxt =
    Lifetime.Var_id.reset ();

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
        Lifetime.LtVar (a_id, Id_string.Pure "", env_id, Int32.of_int 0, [c_id; b_id; a_id], Loc.dummy);
        Lifetime.LtVar (b_id, Id_string.Pure "", env_id, Int32.of_int 0, [c_id; b_id; a_id], Loc.dummy);
        Lifetime.LtVar (c_id, Id_string.Pure "", env_id, Int32.of_int 0, [c_id; b_id; a_id], Loc.dummy)
      ]
    in
    assert_equal_lifetimes expect res
  in

  let test_3 test_ctxt =
    Lifetime.Var_id.reset ();

    let a_id = Lifetime.Var_id.generate () in
    let a = Lifetime.LtVarPlaceholder (a_id, Loc.dummy) in

    let b_id = Lifetime.Var_id.generate () in
    let b = Lifetime.LtVarPlaceholder (b_id, Loc.dummy) in

    let c_id = Lifetime.Var_id.generate () in
    let c = Lifetime.LtVarPlaceholder (c_id, Loc.dummy) in

    (* <a, b> / a: c, b: c / c will be defined at the outer scope *)
    (* live duration: a >= c, b >= c *)
    (* value representation: a <= c, b <= c *)
    let env_id = Env_system.EnvId.E (Env_system.EnvUniqId.generate (), None) in
    let res = Lifetime.convert [a; b] [(a, c); (b, c)] env_id in

    let expect =
      [
        Lifetime.LtVar (a_id, Id_string.Pure "", env_id, Int32.of_int 0, [a_id; c_id], Loc.dummy);
        Lifetime.LtVar (b_id, Id_string.Pure "", env_id, Int32.of_int 1, [b_id; c_id], Loc.dummy);
      ]
    in
    assert_equal_lifetimes expect res
  in

  let test_4 test_ctxt =
    Lifetime.Var_id.reset ();

    let a_id = Lifetime.Var_id.generate () in
    let a = Lifetime.LtVarPlaceholder (a_id, Loc.dummy) in

    let b_id = Lifetime.Var_id.generate () in
    let b = Lifetime.LtVarPlaceholder (b_id, Loc.dummy) in

    (* <a, b> / b: a *)
    (* live duration: b >= a *)
    (* value representation: b <= a *)
    let env_id = Env_system.EnvId.E (Env_system.EnvUniqId.generate (), None) in
    let res = Lifetime.convert [a; b] [(b, a)] env_id in

    let expect =
      [
        Lifetime.LtVar (a_id, Id_string.Pure "", env_id, Int32.of_int 1, [a_id], Loc.dummy);
        Lifetime.LtVar (b_id, Id_string.Pure "", env_id, Int32.of_int 0, [b_id; a_id], Loc.dummy);
      ]
    in
    assert_equal_lifetimes expect res
  in

  let test_4_1 test_ctxt =
    Lifetime.Var_id.reset ();

    let a_id = Lifetime.Var_id.generate () in
    let a = Lifetime.LtVarPlaceholder (a_id, Loc.dummy) in

    let b_id = Lifetime.Var_id.generate () in
    let b = Lifetime.LtVarPlaceholder (b_id, Loc.dummy) in

    (* <b, a> / b: a *)
    (* live duration: b >= a *)
    (* value representation: b <= a *)
    let env_id = Env_system.EnvId.E (Env_system.EnvUniqId.generate (), None) in
    let res = Lifetime.convert [b; a] [(b, a)] env_id in

    let expect =
      [
        Lifetime.LtVar (b_id, Id_string.Pure "", env_id, Int32.of_int 0, [b_id; a_id], Loc.dummy);
        Lifetime.LtVar (a_id, Id_string.Pure "", env_id, Int32.of_int 1, [a_id], Loc.dummy);
      ]
    in
    assert_equal_lifetimes expect res
  in

  let test_5 test_ctxt =
    Lifetime.Var_id.reset ();

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

    let expect =
      [
        Lifetime.LtVar (a_id, Id_string.Pure "", env_id, Int32.of_int 0, [a_id; b_id], Loc.dummy);
        Lifetime.LtVar (b_id, Id_string.Pure "", env_id, Int32.of_int 1, [b_id], Loc.dummy);
        Lifetime.LtVar (c_id, Id_string.Pure "", env_id, Int32.of_int 2, [c_id], Loc.dummy);
      ]
    in
    assert_equal_lifetimes expect res
  in

  [
    "lifetime_1"   >:: test_1;
    "lifetime_2"   >:: test_2;
    "lifetime_3"   >:: test_3;
    "lifetime_4"   >:: test_4;
    "lifetime_4_1" >:: test_4_1;
    "lifetime_5"   >:: test_5;
  ]

let rec suite =
  "semantics_suite" >:::
    [
      "test_test_1" >:: test_test_1;
      "class_align_test" >::: test_class_align;
      "lifetime" >::: test_lifetime;
    ]

let () =
  run_test_tt_main suite
