(*
 * Copyright yutopp 2016 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

module type GENERIC_COUNTER =
  sig
    type t
    val undef : t
    val generate : unit -> t

    val to_string : t -> string
    val compare : t -> t -> int
  end

module type COUNTER_SIG =
  sig
    type t
    val zero : t
    val one :  t
    val max_int : t

    val add : t -> t -> t
    val to_string : t -> string
    val compare : t -> t -> int
  end

module Counter(C : COUNTER_SIG) : GENERIC_COUNTER =
  struct
    type t = C.t

    let undef = C.zero

    let id_counter = ref C.one

    let generate () =
      let cur_id = !id_counter in
      if cur_id = C.max_int then
        failwith "counter is reached to limit";
      id_counter := C.add !id_counter C.one;
      cur_id

    let to_string id =
      C.to_string id

    let compare a b =
      C.compare a b
  end
