(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

type t = {
  name : Typing.Type.t Path.t;
  ty_sc : (Typing.Scheme.t[@printer fun fmt _ -> fprintf fmt ""]);
  mutable body : body_t option;
}

and body_t = BodyExtern of string [@@deriving show]

let create ~name ~ty_sc = { name; ty_sc; body = None }

let set_extern_form glo ~extern_name = glo.body <- Some (BodyExtern extern_name)
