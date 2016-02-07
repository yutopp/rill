type 'env info_t = {
  ti_id     : type_id_ref_t option;
  ti_sort   : 'env type_sort_t;
}

 and 'env type_sort_t =
    UniqueTy of 'env normal_type_t
  | ClassSetTy of 'env
  | FunctionSetTy of 'env
  | Undef
  | NotDetermined of int (*Unification.id_t*) * 'env info_t Unification.t

 and 'env normal_type_t = {
   ty_cenv  : 'env;
 }

and type_id_ref_t = int64   (* type id is represented by int64 *)
module IdType = Int64
let is_type_id_signed = true


let type_sort ty =
  let {
    ti_sort = ts; _
  } = ty in
  ts

let is_unique_ty ty =
  match type_sort ty with
    UniqueTy _ -> true
  | _ -> false

let has_same_class lhs rhs =
  match (type_sort lhs, type_sort rhs) with
  | (UniqueTy lhs_ty_r, UniqueTy rhs_ty_r) ->
     lhs_ty_r.ty_cenv == rhs_ty_r.ty_cenv (* compare envs by reference *)
  | (ClassSetTy lhs_e, ClassSetTy rhs_e) -> lhs_e = rhs_e
  | (FunctionSetTy lhs_e, FunctionSetTy rhs_e) -> lhs_e = rhs_e
  | _ -> false


let is_same lhs rhs =
  match (type_sort lhs, type_sort rhs) with
  | (UniqueTy lhs_r, UniqueTy rhs_r) ->
     begin
       (* TODO: implement correctly *)
       has_same_class lhs rhs
     end
  | _ -> failwith "not supported"


let as_unique ty =
  match type_sort ty with
    UniqueTy r -> r
  | _ -> failwith "as_unique: not unique"


let undef_ty =
  {
    ti_id = None;
    ti_sort = Undef;
  }


module Generator =
  struct
    type 'env id_record_table_t =
        (type_id_ref_t, 'env info_t) Hashtbl.t

    type 'env t = {
      mutable gen_fresh_id  : type_id_ref_t;
      cache_table           : 'env id_record_table_t;
    }


    let default () =
      {
        gen_fresh_id = IdType.zero;
        cache_table = Hashtbl.create 10;
      }

    let make_fresh_id gen =
      let new_id = gen.gen_fresh_id in
      if new_id = IdType.max_int then
        failwith "[ICE] Internal type id is reached to max id...";
      gen.gen_fresh_id <- IdType.succ gen.gen_fresh_id; (* update fresh id *)
      Printf.printf "debug / typeid = new %s / cur %s\n"
                    (IdType.to_string new_id) (IdType.to_string gen.gen_fresh_id);
      new_id

    let generate_type gen ty_s =
      (* TODO: implement cache *)
      let tid = make_fresh_id gen in
      let ty = {
        ti_id = Some tid;
        ti_sort = ty_s;
      } in
      Hashtbl.add gen.cache_table tid ty;

      (ty, tid)

    let find_type_by_cache_id gen t_id =
      try Hashtbl.find gen.cache_table t_id with
      | Not_found ->
         failwith "[ICE] Internal type id is not found"
  end


module Attr =
  struct
    type ref_val =
        Ref
      | Val
  end
