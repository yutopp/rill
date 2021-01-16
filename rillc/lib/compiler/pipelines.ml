(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module Mod = Sema.Mod

let wrap_failure ~prev phase_res =
  match phase_res with
  | Ok phase -> Ok phase
  | Error last_error -> Error Mod_handle.{ previous_phase = prev; last_error }

let wrap_failure' ~prev ~subst phase_res =
  match phase_res with
  | Ok (phase, subst) -> (Ok phase, subst)
  | Error last_error ->
      (Error Mod_handle.{ previous_phase = prev; last_error }, subst)

module Phases = struct
  module Parse = struct
    let trans m =
      let open Result.Let_syntax in
      let Mod.{ path; ds; _ } = m in

      (* TODO: check state and ds to restrict compilaction *)
      let%bind (_stete, parsed) = Syntax.parse_from_file ~ds path in
      let phase = Mod_handle.Parsed parsed in
      Ok phase

    let to_parsed mh =
      match Mod_handle.phase_result mh with
      | Ok (Mod_handle.NotParsed as prev) ->
          let phase_result =
            trans (mh |> Mod_handle.inner) |> wrap_failure ~prev
          in
          Mod_handle.update_phase_result mh ~phase_result;
          ()
      | _ -> ()
  end

  module Phase1_collect_toplevels = struct
    let trans ~builtin ~m ~subst ast =
      let open Result.Let_syntax in
      let Mod.{ menv; ds; _ } = m in

      let%bind p1ast =
        let ctx = Sema.Phase1_collect_toplevels.context ~m ~subst ~builtin in
        Sema.Phase1_collect_toplevels.collect_toplevels ~ctx ast
      in

      let phase = Mod_handle.Phase1CollectTopLevels p1ast in
      Ok phase

    let to_analyzed mh ~builtin ~subst =
      match Mod_handle.phase_result mh with
      | Ok (Mod_handle.Parsed ast as prev) ->
          let phase_result =
            trans ~builtin ~m:(mh |> Mod_handle.inner) ~subst ast
            |> wrap_failure ~prev
          in
          Mod_handle.update_phase_result mh ~phase_result;
          ()
      | _ -> ()
  end

  module Phase1_declare_toplevels = struct
    let trans ~builtin ~m ~subst p1ast =
      let open Result.Let_syntax in
      let%bind ctx =
        let ctx = Sema.Phase1_1.context ~m ~subst ~builtin in
        Sema.Phase1_1.declare_toplevels ~ctx p1ast
      in
      let subst = Sema.Phase1_1.(ctx.subst) in

      let phase = Mod_handle.Phase1DeclareTopLevels p1ast in
      Ok (phase, subst)

    let to_analyzed mh ~builtin ~subst =
      match Mod_handle.phase_result mh with
      | Ok (Mod_handle.Phase1CollectTopLevels p1ast as prev) ->
          let (phase_result, subst) =
            trans ~builtin ~m:(Mod_handle.inner mh) ~subst p1ast
            |> wrap_failure' ~prev ~subst
          in
          Mod_handle.update_phase_result mh ~phase_result;
          subst
      | _ -> subst
  end

  module Phase1_declare_impls = struct
    let trans ~builtin ~m ~subst p1ast =
      let open Result.Let_syntax in
      let%bind (ctx, p1ast) =
        let ctx = Sema.Phase1_2.context ~m ~subst ~builtin in
        Sema.Phase1_2.declare_impls ~ctx p1ast
      in
      let subst = Sema.Phase1_2.(ctx.subst) in

      let phase = Mod_handle.Phase1DeclareImpls p1ast in
      Ok (phase, subst)

    let to_analyzed mh ~builtin ~subst =
      match Mod_handle.phase_result mh with
      | Ok (Mod_handle.Phase1DeclareTopLevels p1ast as prev) ->
          let (phase_result, subst) =
            trans ~builtin ~m:(Mod_handle.inner mh) ~subst p1ast
            |> wrap_failure' ~prev ~subst
          in
          Mod_handle.update_phase_result mh ~phase_result;
          subst
      | _ -> subst
  end

  module Phase2 = struct
    let trans ~builtin ~m ~subst p1ast =
      let open Result.Let_syntax in
      let Mod.{ ds; _ } = m in

      let%bind (p2ast, subst) =
        let ctx = Sema.Phase2.Ctx.create ~ds ~subst ~builtin in
        Sema.Phase2.into_typed_tree ~ctx p1ast
        |> Result.map ~f:(fun n -> (n, Sema.Phase2.Ctx.(ctx.subst)))
      in
      Sema.Mod.set_latest_subst m subst;

      let phase = Mod_handle.Phase2 p2ast in
      Ok phase

    let to_analyzed mh ~builtin ~subst =
      match Mod_handle.phase_result mh with
      | Ok (Mod_handle.Phase1DeclareImpls p1ast as prev)
        when not (Mod_handle.has_errors mh) ->
          let phase_result =
            trans ~builtin ~m:(mh |> Mod_handle.inner) ~subst p1ast
            |> wrap_failure ~prev
          in
          Mod_handle.update_phase_result mh ~phase_result;
          ()
      | _ -> ()
  end
end

module Generator = struct
  module To_phase3 = struct
    let trans ~m p2ast =
      let Mod.{ ds; _ } = m in
      let subst = Mod.subst_of m in

      let ctx = Sema.Phase3.context ~ds ~subst in
      let env = Sema.Phase3.Env.create () in
      let p3ast = Sema.Phase3.normalize ~ctx ~env p2ast in

      let phase = Mod_handle.Phase3 p3ast in
      Ok phase

    let apply mh =
      match Mod_handle.phase_result mh with
      | Ok (Mod_handle.Phase2 p2ast as prev) ->
          let phase_result =
            trans ~m:(mh |> Mod_handle.inner) p2ast |> wrap_failure ~prev
          in
          Mod_handle.update_phase_result mh ~phase_result;
          ()
      | _ -> ()
  end

  module To_rill_ir = struct
    let trans ~builtin ~pkg_handle ~m p3ast =
      let root_mod = Package_handle.root_mod pkg_handle in
      let root_mod_env = Mod.menv root_mod in

      let rir =
        let ctx = Codegen.Rir_gen.context ~m ~builtin ~root_mod_env in
        Codegen.Rir_gen.generate_module ~ctx p3ast
      in

      let phase =
        let art = Emitter.Artifact.Rill_ir { m = rir } in
        Mod_handle.Artifact art
      in
      Ok phase

    let to_artifact mh ~builtin ~pkg_handle =
      match Mod_handle.phase_result mh with
      | Ok (Mod_handle.Phase3 p3ast as prev) when not (Mod_handle.has_errors mh)
        ->
          let phase_result =
            trans ~builtin ~pkg_handle ~m:(mh |> Mod_handle.inner) p3ast
            |> wrap_failure ~prev
          in
          Mod_handle.update_phase_result mh ~phase_result;
          ()
      (* ignore *)
      | _ -> ()
  end

  module To_llvm_ir = struct
    let trans ~builtin ~pkg_handle ~m rir =
      let root_mod = Package_handle.root_mod pkg_handle in

      let root_mod_env = Mod.menv root_mod in
      let subst = Mod.subst_of m in
      let ds = Mod.ds m in

      (* *)
      let llvm =
        let ctx = Llvm_gen.context ~ds ~subst ~builtin in
        Llvm_gen.generate_module ~ctx rir
      in
      let phase =
        let art = Emitter.Artifact.Llvm_ir { m = llvm } in
        Mod_handle.Artifact art
      in
      Ok phase

    let to_artifact mh ~builtin ~pkg_handle =
      match Mod_handle.phase_result mh with
      | Ok (Mod_handle.Artifact (Emitter.Artifact.Rill_ir { m = rir }) as prev)
        ->
          let phase_result =
            trans ~builtin ~pkg_handle ~m:(mh |> Mod_handle.inner) rir
            |> wrap_failure ~prev
          in
          Mod_handle.update_phase_result mh ~phase_result;
          ()
      (* ignore *)
      | _ -> ()
  end
end
