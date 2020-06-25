(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module Mod = Sema.Mod
module Triple = Common.Triple
module Project_buildspace = Pkg_dict
module Pkg_buildspace = Mod_dict

let wrap_failure ~prev phase_result_res =
  match phase_result_res with
  | Ok phase_result -> phase_result
  | Error last_error -> Error Mod_state.{ previous_phase = prev; last_error }

module Phases = struct
  module Parse = struct
    let trans m =
      let open Result.Let_syntax in
      let Mod.{ ds; path; _ } = m in

      (* TODO: check state and ds to restrict compilaction *)
      let%bind (_stete, parsed) = Syntax.parse_from_file ~ds path in
      let phase_result = Ok (Mod_state.Parsed parsed) in
      Ok phase_result

    let to_parsed ~compiler ms =
      let Mod_state.{ m; phase_result; _ } = ms in
      match phase_result with
      | Ok (Mod_state.NotParsed as prev) ->
          let phase_result = trans m |> wrap_failure ~prev in
          Mod_state.{ ms with phase_result }
      | _ -> ms
  end

  module Phase1_collect_toplevels = struct
    let trans ~builtin ~m ~menv ast =
      let open Result.Let_syntax in
      let Mod.{ ds; subst; _ } = m in

      let%bind p1ast =
        let ctx = Sema.Phase1.context ~parent:menv ~ds ~subst ~builtin in
        Sema.Phase1.collect_toplevels ~ctx ast
      in
      m.Sema.Mod.subst <- subst;

      let phase_result = Ok (Mod_state.Phase1CollectTopLevels p1ast) in
      Ok phase_result

    let to_analyzed ~compiler ~builtin ~menv ms =
      let Mod_state.{ m; phase_result; _ } = ms in
      match phase_result with
      | Ok (Mod_state.Parsed ast as prev) ->
          let phase_result =
            trans ~builtin ~m ~menv ast |> wrap_failure ~prev
          in
          Mod_state.{ ms with phase_result }
      | _ -> ms
  end

  module Phase1_declare_toplevels = struct
    let trans ~builtin ~m ~external_pkgs_env p1ast =
      let open Result.Let_syntax in
      let Mod.{ ds; subst; _ } = m in

      let%bind subst =
        let ctx =
          Sema.Phase1_1.context ~ds ~subst ~builtin ~external_pkgs_env
        in
        Sema.Phase1_1.declare_toplevels ~ctx p1ast
        |> Result.map ~f:(fun e -> Sema.Phase1_1.(ctx.subst))
      in
      m.Sema.Mod.subst <- subst;

      let phase_result = Ok (Mod_state.Phase1DeclareTopLevels p1ast) in
      Ok phase_result

    let to_analyzed ~compiler ~builtin ~external_pkgs_env ms =
      let Mod_state.{ m; phase_result; _ } = ms in
      match phase_result with
      | Ok (Mod_state.Phase1CollectTopLevels p1ast as prev) ->
          let phase_result =
            trans ~builtin ~m ~external_pkgs_env p1ast |> wrap_failure ~prev
          in
          Mod_state.{ ms with phase_result }
      | _ -> ms
  end

  module Phase2 = struct
    let trans ~builtin ~m p1ast =
      let open Result.Let_syntax in
      let Mod.{ ds; subst; _ } = m in

      let%bind (p2ast, subst) =
        let ctx = Sema.Phase2.context ~ds ~subst ~builtin in
        Sema.Phase2.into_typed_tree ~ctx p1ast
        |> Result.map ~f:(fun n -> (n, Sema.Phase2.(ctx.subst)))
      in
      m.Sema.Mod.subst <- subst;

      let phase_result = Ok (Mod_state.Phase2 p2ast) in
      Ok phase_result

    let to_analyzed ~compiler ~builtin ms =
      let Mod_state.{ m; phase_result; _ } = ms in
      match phase_result with
      | Ok (Mod_state.Phase1DeclareTopLevels p1ast as prev)
        when not (Mod_state.has_errors ms) ->
          let phase_result = trans ~builtin ~m p1ast |> wrap_failure ~prev in
          Mod_state.{ ms with phase_result }
      | _ -> ms
  end

  let phase3 ~compiler ~m p2ast =
    let Mod.{ ds; subst; _ } = m in

    let ctx = Sema.Phase3.context ~ds ~subst in
    let env = Sema.Phase3.Env.create () in
    Sema.Phase3.normalize ~ctx ~env p2ast
end

module To_rill_ir = struct
  let trans ~compiler dict builtin ms p3ast =
    let Mod_state.{ m; _ } = ms in
    let Mod.{ ds; subst; _ } = m in

    let rir =
      let ctx = Codegen.Rir_gen.context ~ds ~subst ~builtin in
      Codegen.Rir_gen.generate_module ~ctx p3ast
    in
    let phase_result =
      let art = Emitter.Artifact.Rill_ir { m = rir } in
      Ok (Mod_state.Artifact art)
    in
    Ok phase_result

  let to_artifact ~compiler dict builtin ms =
    let Mod_state.{ m; phase_result; _ } = ms in
    match phase_result with
    | Ok (Mod_state.Phase2 p2ast as prev) when not (Mod_state.has_errors ms) ->
        let p3ast =
          (* TODO: check that there are no errors in ds *)
          Phases.phase3 ~compiler ~m p2ast
        in
        let phase_result =
          trans ~compiler dict builtin ms p3ast |> wrap_failure ~prev
        in
        Mod_state.{ ms with phase_result }
    (* ignore *)
    | _ -> ms
end

module To_llvm_ir = struct
  let trans ~compiler dict builtin ms rir =
    let Mod_state.{ m; _ } = ms in
    let Mod.{ ds; subst; _ } = m in

    let llvm =
      let ctx = Llvm_gen.context ~ds ~subst ~builtin in
      Llvm_gen.generate_module ~ctx rir
    in
    let phase_result =
      let art = Emitter.Artifact.Llvm_ir { m = llvm } in
      Ok (Mod_state.Artifact art)
    in
    Ok phase_result

  let to_artifact ~compiler dict builtin ms =
    let Mod_state.{ phase_result; _ } = ms in
    match phase_result with
    | Ok (Mod_state.Artifact (Emitter.Artifact.Rill_ir { m = rir }) as prev) ->
        let phase_result =
          trans ~compiler dict builtin ms rir |> wrap_failure ~prev
        in
        Mod_state.{ ms with phase_result }
    (* *)
    | _ -> ms
end
