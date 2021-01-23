(*
 * Copyright yutopp 2021 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

let pkg_tag = Group.Pkg_tag.create ~name:"test" ~version:"test"

let tag = Group.Mod_tag.create ~pkg_tag ~path:"."

let ty_unit =
  Typing.Type.{ ty = Unit; binding_mut = MutImm; span = Common.Span.undef }

let ty_var =
  let var =
    Typing.Type.Var
      {
        var = Common.Fresh.create ~owner:0 ~fresh:0;
        bound = BoundForall;
        label = "label";
      }
  in
  Typing.Type.{ ty = var; binding_mut = MutImm; span = Common.Span.undef }

let test_generic_id_no_vars () =
  let n0 =
    Path.Name.
      { name = "n0"; kind = Module; generics_vars = []; has_self = false }
  in
  let n1 =
    Path.Name.{ name = "n1"; kind = Type; generics_vars = []; has_self = false }
  in
  let n2 =
    Path.Name.
      { name = "n2"; kind = Var ty_unit; generics_vars = []; has_self = false }
  in
  let id = Rir.Symbol.to_generic_id (Path.create ~tag [ n0; n1; n2 ]) in
  Alcotest.(check string) "same lists" "G_n0[m]::n1[t]::n2[v]" id

let test_generic_id_type_vars_determied () =
  let n0 =
    Path.Name.
      {
        name = "n0";
        kind = Module;
        generics_vars = [ ty_unit ];
        has_self = false;
      }
  in
  let n1 =
    Path.Name.
      {
        name = "n1";
        kind = Type;
        generics_vars = [ ty_unit ];
        has_self = false;
      }
  in
  let n2 =
    Path.Name.
      {
        name = "n2";
        kind = Var ty_unit;
        generics_vars = [ ty_unit ];
        has_self = false;
      }
  in
  let id = Rir.Symbol.to_generic_id (Path.create ~tag [ n0; n1; n2 ]) in
  Alcotest.(check string) "same lists" "G_n0[m]::n1[t]::n2[v]" id

let test_generic_id_type_vars () =
  let n0 =
    Path.Name.
      {
        name = "n0";
        kind = Module;
        generics_vars = [ ty_var ];
        has_self = false;
      }
  in
  let n1 =
    Path.Name.
      { name = "n1"; kind = Type; generics_vars = [ ty_var ]; has_self = false }
  in
  let n2 =
    Path.Name.
      {
        name = "n2";
        kind = Var ty_unit;
        generics_vars = [ ty_var ];
        has_self = false;
      }
  in
  let id = Rir.Symbol.to_generic_id (Path.create ~tag [ n0; n1; n2 ]) in
  Alcotest.(check string) "same lists" "G_n0[m]::n1[t]::n2[v]" id

let () =
  let open Alcotest in
  run "symbol"
    [
      ( "test_generic_id",
        [
          test_case "no vars" `Quick test_generic_id_no_vars;
          test_case "determined" `Quick test_generic_id_type_vars_determied;
          test_case "vars" `Quick test_generic_id_type_vars;
        ] );
    ]

(*
let%test_unit "to_generic_id" =


  
  in

  let test_cases =
    [
      (* *)
      (Path.Name.Module, [], {|G_n0[m]|});
      (Path.Name.Type, [], {|G_n0[t]|});
      (Path.Name.Var ty_unit, [], {|G_n0[v]|});
      (* *)
      (Path.Name.Module, [ ty_unit ], {|G_n0[m]|});
      (Path.Name.Type, [ ty_unit ], {|G_n0[t]|});
      (Path.Name.Var ty_unit, [ ty_unit ], {|G_n0[v]|});
      (* *)
      (Path.Name.Module, [ ty_var ], {|G_n0[m]|});
      (Path.Name.Type, [ ty_var ], {|G_n0[t]|});
      (Path.Name.Var ty_unit, [ ty_var ], {|G_n0[v]|});
    ]
  in
  List.iter test_cases ~f:(fun (kind, generics_vars, result) ->
      let n0 =
        Path.Name.{ name = "n0"; kind; generics_vars; has_self = false }
      in
      let id = to_generic_id (Path.create ~tag [ n0 ]) in
      [%test_result: string] id ~expect:result)

 *)
