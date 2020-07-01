(*
 * Copyright yutopp 2019 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module IntMap = Map.M (Int)
module Counter = Common.Counter

type t = {
  fresh_counter : Counter.t;
  ty_subst : Type.t IntMap.t;
  ki_subst : int IntMap.t;
  mut_subst : Type.mutability_t IntMap.t;
  ln_subst : Type.func_linkage_t IntMap.t;
  struct_tags : struct_tags_t;
}

and struct_tags_t = { st_fresh_counter : Counter.t }

let create_struct_tags () = { st_fresh_counter = Counter.create () }

let create () =
  let ty_subst = Map.empty (module Int) in
  let ki_subst = Map.empty (module Int) in
  let mut_subst = Map.empty (module Int) in
  let ln_subst = Map.empty (module Int) in
  {
    fresh_counter = Counter.create ();
    ty_subst;
    ki_subst;
    mut_subst;
    ln_subst;
    struct_tags = create_struct_tags ();
  }

(* has side effects *)
let fresh_var subst : Type.var_t = Counter.fresh subst.fresh_counter

(* has side effects *)
let fresh_ty ~span subst : Type.t =
  let v = fresh_var subst in
  let binding_mut = Type.MutImm in
  Type.{ ty = Var { var = v }; binding_mut; span }

let fresh_mut subst : Type.mutability_t =
  let v = fresh_var subst in
  Type.MutVar v

let fresh_linkage subst : Type.func_linkage_t =
  let v = fresh_var subst in
  Type.LinkageVar v

(* has side effects *)
let fresh_struct_tag subst =
  let { struct_tags; _ } = subst in
  let v = Counter.fresh struct_tags.st_fresh_counter in
  v

let get_struct_fields_from_name subst name = []

let update_mut subst uni_id mut =
  let { mut_subst; _ } = subst in
  let mut_subst = Map.add_exn mut_subst ~key:uni_id ~data:mut in
  { subst with mut_subst }

let rec subst_mut (subst : t) mut =
  let { mut_subst; _ } = subst in
  match mut with
  | Type.MutVar uni_id ->
      Map.find mut_subst uni_id
      |> Option.value_map ~default:mut ~f:(subst_mut subst)
  | alt -> alt

let rec subst_linkage (subst : t) linkage =
  let { ln_subst; _ } = subst in
  match linkage with
  | Type.LinkageVar uni_id ->
      Map.find ln_subst uni_id
      |> Option.value_map ~default:linkage ~f:(subst_linkage subst)
  | alt -> alt

let update_type subst uni_id mut =
  let { mut_subst; _ } = subst in
  let mut_subst = Map.add_exn mut_subst ~key:uni_id ~data:mut in
  { subst with mut_subst }

let rec subst_type (subst : t) ty : Type.t =
  match ty with
  | Type.{ ty = Var { var = uni_id }; binding_mut; span } -> (
      let { ty_subst; _ } = subst in
      match Map.find ty_subst uni_id with
      | Some ty' ->
          let ty = subst_type subst ty' in
          Type.{ ty with binding_mut }
      | None -> ty )
  | Type.{ ty = Array { elem; n }; _ } as tty ->
      let elem = subst_type subst elem in
      Type.{ tty with ty = Array { elem; n } }
  | Type.{ ty = Func { params; ret; linkage }; _ } as tty ->
      let params = List.map ~f:(subst_type subst) params in
      let ret = subst_type subst ret in
      let linkage = subst_linkage subst linkage in
      Type.{ tty with ty = Func { params; ret; linkage } }
  | Type.{ ty = Pointer { mut; elem }; _ } as tty ->
      let mut = subst_mut subst mut in
      let elem = subst_type subst elem in
      Type.{ tty with ty = Pointer { mut; elem } }
  (* meta *)
  | Type.{ ty = Type inner_ty; _ } as tty ->
      let inner_ty = subst_type subst inner_ty in
      Type.{ tty with ty = Type inner_ty }
  | alt -> alt

let subst_tysc (subst : t) tysc : Scheme.t =
  match tysc with
  | Scheme.(Scheme ([], tys)) ->
      let tys' = subst_type subst tys in
      Scheme.Scheme ([], tys')
  | _ -> failwith "subst_tysc"
