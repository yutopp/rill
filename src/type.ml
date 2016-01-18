type 'env info_t =
    UniqueTy of 'env record_t
  | ClassSetTy
  | FunctionSetTy
  | InvalidTy

and 'env record_t = {
  ty_id     : type_id_ref_t option;
  ty_cenv   : 'env;
}

and type_id_ref_t = int

module Generator =
  struct
    type 'env id_record_table_t =
        (type_id_ref_t, 'env record_t) Hashtbl.t
    type 'env t = {
      mutable gen_fresh_id  : type_id_ref_t;
      gen_table             : 'env id_record_table_t;
    }

    let default () =
      {
        gen_fresh_id = 0;
        gen_table = Hashtbl.create 10;
      }

    let dummy_ty = InvalidTy

    let generate_type gen env =
      let tid = gen.gen_fresh_id in
      let ty = {
        ty_id = Some tid;
        ty_cenv = env;
      } in
      Hashtbl.add gen.gen_table tid ty;
      gen.gen_fresh_id <- gen.gen_fresh_id + 1; (* update fresh id *)

      UniqueTy ty
  end


let is_unique_ty ty =
  match ty with
    UniqueTy _ -> true
  | _ -> false

let has_same_class lhs rhs =
  match (lhs, rhs) with
    (UniqueTy lhs_r, UniqueTy rhs_r) -> lhs_r.ty_cenv = rhs_r.ty_cenv
  | (ClassSetTy, ClassSetTy) -> true
  | (FunctionSetTy, FunctionSetTy) -> true
  | _ -> false


module Attr =
  struct
    type ref_val =
        Ref
      | Val
  end
