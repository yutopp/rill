(*
 * Copyright yutopp 2021 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

type t = {
  cc : string; [@key "cc"]
  cc_sysroot : string option; [@key "cc_sysroot"] [@yojson.option]
}
[@@yojson.allow_extra_fields] [@@deriving yojson]

let empty () = { cc = "gcc"; cc_sysroot = None }

exception Failed_to_parse_json of string

module Variables = struct
  type t = { target_sysroot : string }

  let default () : t = { target_sysroot = "" }

  let to_hash vars =
    let hash = Hashtbl.create (module String) in
    Hashtbl.add_exn hash ~key:"target_sysroot" ~data:vars.target_sysroot;
    hash

  (*                                      --------- 0 --------*)
  (*                                      --- 1 ---    - 2 -  *)
  let var_pattern = Re.Perl.compile_pat {|(^|[^\\])\$\{(.*?)\}|}

  exception Variable_empty of { pos : int * int }

  exception Variable_not_found of { key : string; pos : int * int }

  let subst ~vars text : string =
    let kvs = to_hash vars in

    let f group =
      let key = Re.Group.get group 2 in
      let pos = Re.Group.offset group 2 in

      let () = if String.equal key "" then raise (Variable_empty { pos }) in
      let value =
        match Hashtbl.find kvs key with
        | Some v -> v
        | None -> raise (Variable_not_found { key; pos })
      in

      let prefix = Re.Group.get group 1 in
      Printf.sprintf "%s%s" prefix value
    in
    Re.replace var_pattern ~f text
end

let read ~f =
  let json = f () in
  try
    let t = t_of_yojson json in
    Ok t
  with Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (Failure what, _) ->
    Error (Failed_to_parse_json what)

let load ~vars ~f =
  let open Result.Let_syntax in
  let%bind spec = read ~f in

  let cc = spec.cc |> Variables.subst ~vars in
  let cc_sysroot = spec.cc_sysroot |> Option.map ~f:(Variables.subst ~vars) in
  Ok { spec with cc; cc_sysroot }

let from_channel ~vars ch : (t, exn) Result.t =
  load ~vars ~f:(fun () -> Yojson.Safe.from_channel ch)

let from_string ~vars str : (t, exn) Result.t =
  load ~vars ~f:(fun () -> Yojson.Safe.from_string str)

let%expect_test _ =
  let vars =
    let d = Variables.default () in
    Variables.{ d with target_sysroot = "hoge" }
  in
  let spec =
    from_string ~vars
      {|
{
  "cc": "${target_sysroot}1234${target_sysroot}\\${target_sysroot}"
}
                 |}
    |> Result.ok_exn
  in
  yojson_of_t spec |> Yojson.Safe.pretty_to_string |> Stdio.print_string;
  [%expect {| { "cc": "hoge1234hoge\\${target_sysroot}" } |}]
