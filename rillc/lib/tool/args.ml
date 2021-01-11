(*
 * Copyright yutopp 2021 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module Triple = Common.Triple
module Os = Common.Os
module Target_spec = Common.Target_spec

(* TODO: fix *)
let default_host_triple = Triple.Tag_X86_64_unknown_linux_gnu

let default_target_triple = default_host_triple

(* Ad-hoc impl *)
let default_sysroot () =
  (* e.g. /usr/bin/rillc *)
  let path = Os.current_exe () |> String.split ~on:'/' |> List.rev in
  (* e.g. /usr/bin *)
  let dir = List.tl_exn path in
  (* e.g. /usr *)
  List.tl_exn dir |> List.rev |> String.concat ~sep:"/" |> Printf.sprintf "/%s"

let sysroot sysroot =
  match sysroot with Some dir -> dir | None -> default_sysroot ()

let host_triple () = default_host_triple

let target_triple target = target |> Option.value ~default:default_target_triple

let target_sysroot ~sysroot ~triple =
  let (module Target : Triple.PRESET) = Triple.to_triple_preset triple in
  let triple_name = Target.name in
  Os.join_path [ sysroot; "lib"; "rill-lib"; triple_name ]

let target_spec ~target_sysroot =
  let open Result.Let_syntax in
  (* adhoc-impl *)
  let target_spec_path = Os.join_path [ target_sysroot; "target-spec.json" ] in

  let vars = Target_spec.Variables.{ target_sysroot } in
  let%bind target_spec =
    let ch = Stdlib.open_in target_spec_path in
    Exn.protect
      ~f:(fun () -> Target_spec.from_channel ~vars ch)
      ~finally:(fun () -> Stdlib.close_in ch)
  in
  Ok target_spec
