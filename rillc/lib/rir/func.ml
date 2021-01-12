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
  name : Typing.Type.t Common.Chain.Nest.t;
  ty_sc : (Typing.Scheme.t[@printer fun fmt _ -> fprintf fmt ""]);
  mutable body : body_t option;
}

and body_t = BodyFunc of body_func_t | BodyExtern of string

and body_func_t = {
  bbs : BBs.t;
  mutable bbs_names_rev : string list;
  mutable ret_term : Term.t option;
  mutable pre_allocs : pre_alloc_t list;
  fresh_id : (Counter.t[@printer fun fmt _ -> fprintf fmt ""]);
}

and pre_alloc_t = { p_bb_name : string; p_insts : pre_alloc_inst_t list }

and pre_alloc_inst_t = Term.inst_t * addressable_t

and addressable_t = AddressableT of addressable_extra_t | AddressableF

and addressable_extra_t = { addressable_e_kind : addressable_extra_kind_t }

and addressable_extra_kind_t = AddrKindStandard | AddrKindRet
[@@deriving show]

let create ~name ~ty_sc = { name; ty_sc; body = None }

let failwith_nobody tag f =
  failwith
    (Printf.sprintf "[ICE] no body (%s): %s" tag
       (Common.Chain.Nest.to_string ~to_s:Typing.Type.to_string f.name))

let get_ret_ty f =
  let (Typing.Scheme.ForAll { ty; _ }) = f.ty_sc in
  let (Typing.Pred.Pred { conds; ty }) = ty in
  let (_, ret_ty) = Typing.Type.assume_func_ty ty in
  ret_ty

let prepare_bb_name f name =
  match f.body with
  | Some (BodyFunc fb) -> (
      match Hashtbl.mem fb.bbs name with
      | true ->
          (* If already exists, generate a fresh name *)
          let fresh_suffix = Counter.fresh_string fb.fresh_id in
          Printf.sprintf "%s%s" name fresh_suffix
      | false -> name )
  | _ -> failwith_nobody "prepare_bb_name" f

let insert_bb f bb =
  match f.body with
  | Some (BodyFunc fb) ->
      let name = bb.Term.BB.name in
      Hashtbl.add_exn fb.bbs ~key:name ~data:bb;
      fb.bbs_names_rev <- name :: fb.bbs_names_rev;
      ()
  | _ -> failwith_nobody "insert_bb" f

let set_extern_form func ~extern_name =
  func.body <- Some (BodyExtern extern_name)

let set_body_form func =
  let body =
    {
      bbs = Hashtbl.create (module String);
      bbs_names_rev = [];
      ret_term = None;
      pre_allocs = [];
      fresh_id = Counter.create ();
    }
  in
  func.body <- Some (BodyFunc body)

let gen_local_var f =
  match f.body with
  | Some (BodyFunc fb) ->
      let s = Counter.fresh_string fb.fresh_id in
      Printf.sprintf "$_%s" s
  | _ -> failwith_nobody "gen_local_var" f

let entry_name = "entry"

let get_entry_bb f =
  match f.body with
  | Some (BodyFunc fb) -> Hashtbl.find fb.bbs entry_name
  | _ -> failwith_nobody "get_entry_bb" f

let set_ret_term func term =
  match func.body with
  | Some (BodyFunc fb) -> (
      match term with
      | Term.{ kind = LVal _; _ } -> fb.ret_term <- Some term
      | _ -> failwith "" )
  | _ -> failwith_nobody "set_ret_term" func

let get_ret_term func =
  match func.body with
  | Some (BodyFunc fb) -> fb.ret_term
  | _ -> failwith_nobody "get_ret_term" func

let set_pre_allocs func pre_allocs =
  match func.body with
  | Some (BodyFunc fb) -> fb.pre_allocs <- pre_allocs
  | _ -> failwith_nobody "set_pre_allocs" func

let get_pre_allocs func =
  match func.body with
  | Some (BodyFunc fb) -> fb.pre_allocs
  | _ -> failwith_nobody "get_pre_allocs" func

let get_bbs func =
  match func.body with
  | Some (BodyFunc fb) -> fb.bbs
  | _ -> failwith_nobody "get_bbs" func

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
                let bb = Hashtbl.find_exn (get_bbs func) n in
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

let to_string_pre_alloc ~indent alloc =
  let buf = Buffer.create 256 in
  Buffer.add_string buf (String.make indent ' ');

  let { p_bb_name; p_insts } = alloc in
  Buffer.add_string buf (Printf.sprintf "scope: name=%s\n" p_bb_name);
  List.iter p_insts ~f:(fun p_inst ->
      let (inst, storage) = p_inst in
      let s = Term.to_string_inst ~indent:(indent + 2) inst in
      Buffer.add_string buf s;
      Buffer.add_string buf " :: ";
      Buffer.add_string buf (show_addressable_t storage);
      Buffer.add_char buf '\n');
  Buffer.contents buf

let to_string ~indent func =
  let buf = Buffer.create 256 in
  Buffer.add_string buf (String.make indent ' ');

  let { name; ty_sc; body } = func in
  Buffer.add_string buf
    (Printf.sprintf "Func: name = '%s' :: %s\n"
       (Common.Chain.Nest.to_string ~to_s:Typing.Type.to_string name)
       (Typing.Scheme.to_string ty_sc));

  let () =
    match body with
    | Some (BodyExtern extern_name) -> ()
    | Some (BodyFunc _) ->
        let allocs = get_pre_allocs func in
        List.iter allocs ~f:(fun alloc ->
            let s = to_string_pre_alloc ~indent:(indent + 2) alloc in
            Buffer.add_string buf s);

        let bbs = list_reached_bbs func in
        List.iter bbs ~f:(fun bb ->
            let s = Term.BB.to_string ~indent:(indent + 2) bb in
            Buffer.add_string buf s)
    | None -> ()
  in
  Buffer.contents buf
