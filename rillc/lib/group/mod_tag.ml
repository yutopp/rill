(*
 * Copyright yutopp 2021 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

type t = { pkg_tag : Pkg_tag.t; path : string } [@@deriving show, eq, yojson_of]

let create ~pkg_tag ~path : t = { pkg_tag; path }

let with_path tag ~path = { tag with path }

let path tag = tag.path
