(*
 * Copyright yutopp 2018 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module Counter = Common.Counter

module BBs = struct
  type t = (string, Term.BB.t) Hashtbl.t

  let pp ppf values =
    Hashtbl.iteri values ~f:(fun ~key ~data ->
        Caml.Format.fprintf ppf "@[<1>%s: %s@]@." key (Term.BB.show data))
end

type t = {
  ty : (Typing.Type.t[@printer fun fmt _ -> fprintf fmt ""]);
  bbs : BBs.t;
  mutable bbs_names_rev : string list;
  mutable pre_allocs : pre_alloc_t list;
  fresh_id : (Counter.t[@printer fun fmt _ -> fprintf fmt ""]);
  extern_name : string option;
}

and pre_alloc_t = { p_bb_name : string; p_insts : pre_alloc_inst_t list }

and pre_alloc_inst_t = Term.inst_t * addressable_t

and addressable_t = AddressableT | AddressableF [@@deriving show]

let create_vanilla ?(extern_name = None) ~ty =
  {
    ty;
    bbs = Hashtbl.create (module String);
    bbs_names_rev = [];
    pre_allocs = [];
    fresh_id = Counter.create ();
    extern_name;
  }

let prepare_bb_name f name =
  match Hashtbl.mem f.bbs name with
  | true ->
      (* If already exists, generate a fresh name *)
      let fresh_suffix = Counter.fresh_string f.fresh_id in
      Printf.sprintf "%s%s" name fresh_suffix
  | false -> name

let insert_bb f bb =
  let name = bb.Term.BB.name in
  Hashtbl.add_exn f.bbs ~key:name ~data:bb;
  f.bbs_names_rev <- name :: f.bbs_names_rev;
  ()

let get_ret_ty f =
  let (_, ret_ty) = Typing.Type.assume_func_ty f in
  ret_ty

let gen_local_var f =
  let s = Counter.fresh_string f.fresh_id in
  Printf.sprintf "$_%s" s

let create ?(extern_name = None) ~ty =
  let f = create_vanilla ~extern_name ~ty in
  let () =
    match extern_name with
    | None ->
        let bb = Term.BB.create "entry" in
        insert_bb f bb
    | Some _ -> ()
  in
  f

let get_entry_bb f = Hashtbl.find f.bbs "entry"

let set_pre_allocs func pre_allocs = func.pre_allocs <- pre_allocs

let get_pre_allocs func = func.pre_allocs

let get_bbs func = func.bbs

let fold_bbs func ~init ~f =
  match get_entry_bb func with
  | Some bb ->
      let q = Queue.create () in
      let visited = Hash_set.create (module String) in

      Queue.enqueue q bb;
      Hash_set.add visited bb.Term.BB.name;

      let rec iter acc =
        match Queue.dequeue q with
        | None -> acc
        | Some bb ->
            let succ = Term.BB.get_successors bb in
            List.iter succ ~f:(fun n ->
                let bb = Hashtbl.find_exn func.bbs n in
                if not (Hash_set.mem visited bb.Term.BB.name) then
                  Queue.enqueue q bb;
                Hash_set.add visited bb.Term.BB.name);

            let acc = f acc bb in
            iter acc
      in
      iter init
  | None -> init

let list_reached_bbs func =
  let reached_bbs =
    fold_bbs func ~init:[] ~f:(fun reached_bbs bb ->
        let reached_bbs = bb :: reached_bbs in
        reached_bbs)
    |> List.rev
  in
  reached_bbs
