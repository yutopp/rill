(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

let transform m subst =
  let m = Norm.transform m in
  Stdio.printf "next gen\n";
  let m = Gen.transform m subst in
  m
