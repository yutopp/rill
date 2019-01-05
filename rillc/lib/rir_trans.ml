(*
 * Copyright yutopp 2018 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

module Ctx = struct
  type t = {
    m: Rir_term.t;
    mutable current_func: Rir_term.Func.t option;
    mutable current_bb: Rir_term.BB.t option;
  }

  let create m =
    {
      m;
      current_func = None;
      current_bb = None;
    }

  let get_module ctx =
    ctx.m

  let set_current_func ctx f =
    ctx.current_func <- Some f

  let get_current_func ctx =
    Option.value_exn ctx.current_func

  let set_current_bb ctx bb =
    ctx.current_bb <- Some bb

  let get_current_bb ctx =
    Option.value_exn ctx.current_bb

  let get_current_state ctx =
    (ctx.current_func, ctx.current_bb)

  let set_current_state ctx s =
    let (cf_opt, cbb_opt) = s in
    cf_opt |> Option.iter ~f:(set_current_func ctx);
    cbb_opt |> Option.iter ~f:(set_current_bb ctx)

  let register_func_def ctx _name f =
    (* TODO: support name *)
    Rir_term.append_func ctx.m f

  let build_let ctx name v =
    let inst = Rir_term.Let (name, v) in
    let bb = get_current_bb ctx in
    Rir_term.BB.append bb inst
end

let rec transform k_form : (Rir_term.t, Diagnostics.t) Result.t =
  match k_form with
  | Rir_k_norm.{kind = Module {nodes}; ty; span} ->
     let m = Rir_term.make_module () in
     let ctx = Ctx.create m in
     let errs = List.fold_left nodes ~init:[] ~f:(fun errs node ->
                                 let r = transform_terms ctx node in
                                 match r with
                                 | Ok _ -> errs
                                 | Error d -> d :: errs
                               ) in
     begin match errs with
     | [] -> Ok (Ctx.get_module ctx)
     | _  ->
        let reason = Diagnostics.Multiple (errs |> List.rev) in
        Error (Diagnostics.create ~reason ~span ~phase:Diagnostics.PhaseRirTrans)
     end

  | Rir_k_norm.{span; _} ->
     let detail = k_form |> Rir_k_norm.sexp_of_t |> Sexp.to_string_hum ~indent:2 in
     let reason = Diagnostics.InternalUnsupportedNode detail in
     Error (Diagnostics.create ~reason ~span ~phase:Diagnostics.PhaseRirTrans)

and transform_terms ctx k_form : (Rir_term.value_t, Diagnostics.t) Result.t =
  match k_form with
  | Rir_k_norm.{kind = FuncDef {name; params; body}; ty; span} ->
     let s = Ctx.get_current_state ctx in

     let f = Rir_term.Func.create name in
     let () = Ctx.set_current_func ctx f in

     let bb = Rir_term.BB.create "entry" in
     Rir_term.Func.insert_bb f bb;
     let () = Ctx.set_current_bb ctx bb in

     let r = transform_terms ctx body in
     if Result.is_ok r then begin
       Ctx.register_func_def ctx name (Ctx.get_current_func ctx)
     end;

     let () = Ctx.set_current_state ctx s in

     r

  | Rir_k_norm.{kind = Seq nodes; ty; span} ->
     List.fold_result nodes
                      ~init:(Rir_term.{kind = Rir_term.Unit; ty; span})
                      ~f:(fun _ node -> transform_terms ctx node)

  | Rir_k_norm.{kind = Let {name; expr; body}; ty; span} ->
     let open Result.Let_syntax in
     let%bind expr' = transform_terms ctx expr in
     Ctx.build_let ctx name expr';
     transform_terms ctx body

  | Rir_k_norm.{kind = Undef; ty; span} ->
     Ok Rir_term.{kind = Rir_term.Unit; ty; span}

  | Rir_k_norm.{kind = Call {name; args}; ty; span} ->
     Ok Rir_term.{kind = Rir_term.Call (name, args); ty; span}

  | Rir_k_norm.{kind = LitString s; ty; span} ->
     Ok Rir_term.{kind = Rir_term.RVal (ValueString s); ty; span}

  | Rir_k_norm.{kind = LitUnit; ty; span} ->
     Ok Rir_term.{kind = Rir_term.RVal ValueUnit; ty; span}

  | Rir_k_norm.{span; _} ->
     let detail = k_form |> Rir_k_norm.sexp_of_t |> Sexp.to_string_hum ~indent:2 in
     let reason = Diagnostics.InternalUnsupportedNode detail in
     Error (Diagnostics.create ~reason ~span ~phase:Diagnostics.PhaseRirTrans)
