type 'env info_t =
    UniqueTy of 'env record_t
  | ClassSetTy
  | FunctionSetTy of 'env
  | InvalidTy
  | Undef

and 'env record_t = {
  ty_id     : type_id_ref_t option;
  ty_cenv   : 'env;
}

and type_id_ref_t = int64   (* type id is represented by int64 *)
module IdType = Int64
let is_type_id_signed = true


let is_unique_ty ty =
  match ty with
    UniqueTy _ -> true
  | _ -> false

let has_same_class lhs rhs =
  match (lhs, rhs) with
  | (UniqueTy lhs_r, UniqueTy rhs_r) ->
     lhs_r.ty_cenv == rhs_r.ty_cenv (* compare envs by reference *)
  | (ClassSetTy, ClassSetTy) -> true
  | (FunctionSetTy lhs_e, FunctionSetTy rhs_e) -> lhs_e = rhs_e
  | _ -> false


let as_unique ty =
  match ty with
    UniqueTy r -> r
  | _ -> failwith "as_unique: not unique"


module Generator =
  struct
    type 'env id_record_table_t =
        (type_id_ref_t, 'env record_t) Hashtbl.t

    type 'env t = {
      mutable gen_fresh_id  : type_id_ref_t;
      cache_table           : 'env id_record_table_t;
    }


    let default () =
      {
        gen_fresh_id = IdType.zero;
        cache_table = Hashtbl.create 10;
      }

    let dummy_ty = InvalidTy

    let make_fresh_id gen =
      let new_id = gen.gen_fresh_id in
      if new_id = IdType.max_int then
        failwith "[ICE] Internal type id is reached to max id...";
      gen.gen_fresh_id <- IdType.succ gen.gen_fresh_id; (* update fresh id *)
      Printf.printf "debug / typeid = new %s / cur %s\n"
                    (IdType.to_string new_id) (IdType.to_string gen.gen_fresh_id);
      new_id

    let generate_type_with_cache gen env =
      (* TODO: implement cache *)
      let tid = make_fresh_id gen in
      let ty = {
        ty_id = Some tid;
        ty_cenv = env;
      } in
      Hashtbl.add gen.cache_table tid ty;

      (UniqueTy ty, tid)

    let find_type_by_cache_id gen t_id =
      let ty_record = try Hashtbl.find gen.cache_table t_id with
                      | Not_found ->
                         failwith "[ICE] Internal type id is not found"
      in
      UniqueTy ty_record
  end


module Attr =
  struct
    type ref_val =
        Ref
      | Val
  end
