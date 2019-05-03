(*
 * Copyright yutopp 2019 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

let tmp_err_wrap r =
  match r with
  | Ok v -> v
  | Error err ->
     match err with
     | Typer.ErrTypeMismatch (a, b) ->
        let s =
          Printf.sprintf "%s = %s"
                         (Type.sexp_of_t a |> Sexp.to_string_hum)
                         (Type.sexp_of_t b |> Sexp.to_string_hum)
        in
        failwith @@ "(TODO) cannot unify: " ^ s

let lookup_var penv name subst =
  let env = Option.value_exn (Env.lookup penv name |> Result.ok) in
  let tysc = Option.value_exn env.Env.tysc in
  match tysc with
  | Type.Scheme.Scheme ([], ty) ->
     (ty, subst)

  (**
     ty = forall.a -> ...
     clone -> ty' = forall.a' -> ...
     unify ty ty' (a -> a')
     returns ty'
   *)
  | Type.Scheme.Scheme (ids, ty) ->
     let new_ids = ids |> List.map ~f:(fun _ -> Typer.fresh_var subst) in
     let subst =
       List.fold_result
         ~f:(fun subst (a, b) -> Typer.unify subst a b)
         ~init:subst
         (List.zip_exn
            (ids |> List.map ~f:(fun i -> Type.Var i))
            (new_ids |> List.map ~f:(fun i -> Type.Var i)))
       |> tmp_err_wrap
     in
     let ty = Typer.subst_type subst ty in
     (ty, subst)

let rec gen_terms builder subst nast penv : (Rir.Term.t * Typer.t) =
  Stdio.printf "->: %s\n"
               (nast |> Norm.NAst.sexp_of_t |> Sexp.to_string_hum ~indent:2);

  match nast with
  | Norm.NAst.{kind = Let {name; expr; body}; span} ->
     let (expr', subst) = gen_terms builder subst expr penv in
     Rir.Builder.build_let builder name expr';

     let tysc = Type.Scheme.of_ty expr'.Rir.Term.ty in
     let kind = Env.Kind.Var in
     let venv = Env.create name kind (Some penv) ~tysc in
     let _ = Env.insert penv venv in

     let (body', subst) = gen_terms builder subst body penv in

     let _ = Env.delete penv name in

     (body', subst)

  | Norm.NAst.{kind = Seq nodes; span} ->
     let ty = Type.Bottom in
     let dummy = Rir.Term.{kind = Undef; ty; span} in

     List.fold_left
       ~f:(fun (_, subst) node ->
         gen_terms builder subst node penv
       )
       ~init:(dummy, subst)
       nodes

  | Norm.NAst.{kind = Call {name; args}; span; _} ->
     let (callee_ty, subst) = lookup_var penv name subst in

     let (args_tys_rev, subst) =
       List.fold_left ~f:(fun (args_tys, subst) arg ->
                        let (ty, subst) = lookup_var penv arg subst in
                        (ty :: args_tys, subst)
                      )
                      ~init:([], subst)
                      args
     in
     let ret_ty = Typer.fresh_ty subst in
     let func_ty = Type.Func (args_tys_rev |> List.rev, ret_ty) in

     let subst = Typer.unify subst callee_ty func_ty |> tmp_err_wrap in

     let ty = Typer.subst_type subst ret_ty in
     let node = Rir.Term.{kind = Call (name, args); ty; span} in
     (node, subst)

  | Norm.NAst.{kind = LitInt {value; bits; signed}; span; _} ->
     let ty = Type.Int in
     let node = Rir.Term.{kind = RVal (ValueInt value); ty; span} in
     (node, subst)

  | Norm.NAst.{kind = LitString s; span; _} ->
     let ty = Type.String in
     let node = Rir.Term.{kind = RVal (ValueString s); ty; span} in
     (node, subst)

  | Norm.NAst.{kind = LitUnit; span; _} ->
     let ty = Type.Unit in
     let node = Rir.Term.{kind = RVal (ValueUnit); ty; span} in
     (node, subst)

  (* others *)
  | k ->
     failwith @@
       Printf.sprintf "Unknown node': %s"
                      (k |> Norm.NAst.sexp_of_t |> Sexp.to_string_hum ~indent:2)

let gen subst builder name nast env =
  match nast with
  (* toplevels *)
  | Norm.NAst.{kind = Func func_kind; span; _} ->
     let s = Rir.Builder.get_current_state builder in

     let f =
       match func_kind with
       | Norm.NAst.FuncKindDef body ->
          let f = Rir.Func.create ~tysc:(Option.value_exn env.Env.tysc) ~extern_name:None in
          let () = Rir.Builder.set_current_func builder f in

          let bb = Rir.Term.BB.create "entry" in
          Rir.Func.insert_bb f bb;
          let () = Rir.Builder.set_current_bb builder bb in

          (* *)
          let (body', _) = gen_terms builder subst body env in
          (* TODO: check all types are substituted *)
          let tysc = Typer.subst_tysc subst (Option.value_exn env.Env.tysc) in
          let f = Rir.Func.update_tysc f tysc in

          f

       | Norm.NAst.FuncKindExtern v ->
          (* TODO: fix (remove temporary function) *)
          let f = Rir.Func.create ~tysc:(Option.value_exn env.Env.tysc) ~extern_name:None in
          let () = Rir.Builder.set_current_func builder f in

          let bb = Rir.Term.BB.create "entry" in
          Rir.Func.insert_bb f bb;
          let () = Rir.Builder.set_current_bb builder bb in

          let (body', subst) = gen_terms builder subst v env in
          let subst = Typer.unify subst body'.Rir.Term.ty (Type.String) |> tmp_err_wrap in
          let name =
            match body' with
            | Rir.Term.{kind = RVal (Rir.Term.ValueString name); _} ->
               name
            | _ ->
               failwith "[ICE]"
          in

          let tysc = Typer.subst_tysc subst (Option.value_exn env.Env.tysc) in
          let f = Rir.Func.create ~tysc ~extern_name:(Some name) in
          f

       | _ ->
          failwith "[ICE] not supported function"
     in

     Stdio.printf "rir = \n%s\n" (Rir.Func.sexp_of_t f |> Sexp.to_string_hum ~indent:2);
     Rir.Builder.register_func_def builder name f;

     let () = Rir.Builder.set_current_state builder s in

     ()

  (* others *)
  | k ->
     failwith @@
       Printf.sprintf "Unknown node': %s"
                      (k |> Norm.NAst.sexp_of_t |> Sexp.to_string_hum ~indent:2)

let gen subst builder ~key ~data =
  let (nast, env) = data in
  let () = gen subst builder key nast env in
  ()

let transform m subst =
  let rir_m = Rir.Module.create () in
  let builder = Rir.Builder.create ~m:rir_m in

  let () = Module.iteri ~f:(gen subst builder) m in

  rir_m
