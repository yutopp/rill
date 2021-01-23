(*
 * Copyright yutopp 2021 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

type t = { name : string; version : string } [@@deriving show, eq, yojson_of]

let create ~name ~version : t = { name; version }

let name tag = tag.name

let version tag = tag.version
