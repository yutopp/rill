(*
 * Copyright yutopp 2019 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

type t = {
  name : string; [@key "name"]
  deps : deps_t; [@key "deps"]
  lib : lib_t; [@key "lib"]
  c_ext : c_ext_t option; [@key "c_ext"] [@yojson.option]
}
[@@yojson.allow_extra_fields]

and deps_t = dep_t list

and dep_t = {
  dep_name : string; [@key "name"]
  dep_spec : dep_spec_t; [@key "spec"]
}

and dep_spec_t = { dep_spec_path : string option [@key "path"] }

and lib_t = { flags : lib_flags_t }

and lib_flags_t = string list

and c_ext_t = {
  c_ext_dir : string; [@key "dir"]
  c_ext_name : string; [@key "name"]
}
[@@deriving yojson]

exception Failed_to_parse_json of string

let from_channel ch =
  let json = Yojson.Safe.from_channel ch in
  try
    let t = t_of_yojson json in
    Ok t
  with Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (Failure what, _) ->
    Error (Failed_to_parse_json what)
