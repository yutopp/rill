(*
 * Copyright yutopp 2019 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

module Span = Common.Span
module Diagnostics = Common.Diagnostics

let wrap_reason ~span r =
  Result.map_error
    ~f:(fun reason ->
      let d = Diagnostics.create ~reason ~span ~phase:Diagnostics.PhaseSema in
      d)
    r

let lookup_var penv name subst =
  let open Result.Let_syntax in
  let%bind env =
    Env.lookup penv name
    |> Result.map_error ~f:(fun _trace -> new Common.Reasons.id_not_found ~name)
  in
  let tysc = Option.value_exn env.Env.tysc in
  match tysc with
  | Type.Scheme.Scheme ([], ty) ->
     (ty, subst) |> return

  (**
   * 0. ty = forall.a -> ...
   * 1. clone -> ty' = forall.a' -> ...
   * 2. unify ty ty' (params: a -> a')
   * 3. returns ty'
   *)
  | Type.Scheme.Scheme (ids, ty) ->
     let new_ids = ids |> List.map ~f:(fun _ -> Typer.fresh_var subst) in
     let%bind subst =
       List.fold_result
         ~f:(fun subst (a, b) -> Typer.unify subst a b)
         ~init:subst
         (List.zip_exn
            (ids |> List.map ~f:(fun i -> Type.Var i))
            (new_ids |> List.map ~f:(fun i -> Type.Var i)))
     in
     let ty = Typer.subst_type subst ty in
     (ty, subst) |> return

let rec gen_terms builder subst nast penv : ((Rir.Term.t * Typer.t), 'a) Result.t =
  Stdio.printf "->: %s\n"
               (nast |> Norm.NAst.sexp_of_t |> Sexp.to_string_hum ~indent:2);
  let open Result.Let_syntax in
  match nast with
  | Norm.NAst.{kind = Let {name; expr; body}; span} ->
     let%bind (expr', subst) = gen_terms builder subst expr penv in
     Rir.Builder.build_let builder name expr';

     let tysc = Type.Scheme.of_ty expr'.Rir.Term.ty in
     let kind = Env.Kind.Var in
     let venv = Env.create name kind (Some penv) ~tysc in
     let _ = Env.insert penv venv in

     let%bind (body', subst) = gen_terms builder subst body penv in

     let _ = Env.delete penv name in

     (body', subst) |> return

  | Norm.NAst.{kind = Seq nodes; span} ->
     let ty = Type.Bottom in
     let dummy = Rir.Term.{kind = Undef; ty; span} in

     List.fold_result
       ~f:(fun (_, subst) node ->
         gen_terms builder subst node penv
       )
       ~init:(dummy, subst)
       nodes

  | Norm.NAst.{kind = Call {name; args}; span; _} ->
     let%bind (callee_ty, subst) = lookup_var penv name subst |> wrap_reason ~span in

     let%bind (args_tys_rev, subst) =
       List.fold_result ~f:(fun (args_tys, subst) arg ->
                          let%bind (ty, subst) = lookup_var penv arg subst in
                          (ty :: args_tys, subst) |> return
                        )
                        ~init:([], subst)
                        args
       |> wrap_reason ~span
     in
     let ret_ty = Typer.fresh_ty subst in
     let func_ty = Type.Func (args_tys_rev |> List.rev, ret_ty) in

     let%bind subst =
       Typer.unify subst callee_ty func_ty |> wrap_reason ~span
     in

     let ty = Typer.subst_type subst ret_ty in
     let node = Rir.Term.{kind = Call (name, args); ty; span} in
     (node, subst) |> return

  | Norm.NAst.{kind = LitInt {value; bits; signed}; span; _} ->
     let ty = Type.Int in
     let node = Rir.Term.{kind = RVal (ValueInt value); ty; span} in
     (node, subst) |> return

  | Norm.NAst.{kind = LitString s; span; _} ->
     let ty = Type.String in
     let node = Rir.Term.{kind = RVal (ValueString s); ty; span} in
     (node, subst) |> return

  | Norm.NAst.{kind = LitUnit; span; _} ->
     let ty = Type.Unit in
     let node = Rir.Term.{kind = RVal (ValueUnit); ty; span} in
     (node, subst) |> return

  (* others *)
  | k ->
     failwith @@
       Printf.sprintf "Unknown node': %s"
                      (k |> Norm.NAst.sexp_of_t |> Sexp.to_string_hum ~indent:2)

let gen subst builder name nast env dm : Diagnostics.Multi.t =
  match nast with
  (* toplevels *)
  | Norm.NAst.{kind = Func func_kind; span; _} ->
     let s = Rir.Builder.get_current_state builder in

     let (f_opt, dm) =
       match func_kind with
       | Norm.NAst.FuncKindDef body ->
          let f = Rir.Func.create ~tysc:(Option.value_exn env.Env.tysc) ~extern_name:None in
          let () = Rir.Builder.set_current_func builder f in

          let bb = Rir.Term.BB.create "entry" in
          Rir.Func.insert_bb f bb;
          let () = Rir.Builder.set_current_bb builder bb in

          (* *)
          begin match gen_terms builder subst body env with
          | Ok (_body', _) ->
             (* TODO: check all types are substituted *)
             let tysc = Typer.subst_tysc subst (Option.value_exn env.Env.tysc) in
             let f = Rir.Func.update_tysc f tysc in
             (Some f, dm)

          | Error d ->
             (None, Diagnostics.Multi.append dm d)
          end

       | Norm.NAst.FuncKindExtern v ->
          (* TODO: fix (remove temporary function) *)
          let f = Rir.Func.create ~tysc:(Option.value_exn env.Env.tysc) ~extern_name:None in
          let () = Rir.Builder.set_current_func builder f in

          let bb = Rir.Term.BB.create "entry" in
          Rir.Func.insert_bb f bb;
          let () = Rir.Builder.set_current_bb builder bb in

          begin match gen_terms builder subst v env with
            Ok (body', subst) ->
             let subst =
               Typer.unify subst body'.Rir.Term.ty (Type.String) |> wrap_reason ~span
             in
             begin match subst with
             | Ok subst ->
                let name =
                  match body' with
                  | Rir.Term.{kind = RVal (Rir.Term.ValueString name); _} ->
                     name
                  | _ ->
                     failwith "[ICE]"
                in

                let tysc = Typer.subst_tysc subst (Option.value_exn env.Env.tysc) in
                let f = Rir.Func.create ~tysc ~extern_name:(Some name) in
                (Some f, dm)

             | Error d ->
                (None, Diagnostics.Multi.append dm d)
             end

          | Error d ->
             (None, Diagnostics.Multi.append dm d)
          end

       | _ ->
          failwith "[ICE] not supported function"
     in

     let () = match f_opt with
       | Some f ->
          Stdio.printf "rir = \n%s\n" (Rir.Func.sexp_of_t f |> Sexp.to_string_hum ~indent:2);
          Rir.Builder.register_func_def builder name f
       | None ->
          ()
     in

     let () = Rir.Builder.set_current_state builder s in

     dm

  (* others *)
  | k ->
     failwith @@
       Printf.sprintf "Unknown node': %s"
                      (k |> Norm.NAst.sexp_of_t |> Sexp.to_string_hum ~indent:2)

let gen subst builder ~key ~data dm =
  let (nast, env) = data in
  let dm = gen subst builder key nast env dm in
  dm

let transform m subst dm =
  let rir_m = Rir.Module.create () in
  let builder = Rir.Builder.create ~m:rir_m in

  let dm = Module.fold ~f:(gen subst builder) ~init:dm m in

  (rir_m, dm)
