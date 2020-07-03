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

let wrap_failure ~prev phase_res =
  match phase_res with
  | Ok phase -> Ok phase
  | Error last_error -> Error Mod_state.{ previous_phase = prev; last_error }

let wrap_failure' ~prev ~subst phase_res =
  match phase_res with
  | Ok (phase, subst) -> (Ok phase, subst)
  | Error last_error ->
      (Error Mod_state.{ previous_phase = prev; last_error }, subst)

module Phases = struct
  module Parse = struct
    let trans m =
      let open Result.Let_syntax in
      let Mod.{ path; ds; _ } = m in

      (* TODO: check state and ds to restrict compilaction *)
      let%bind (_stete, parsed) = Syntax.parse_from_file ~ds path in
      let phase = Mod_state.Parsed parsed in
      Ok phase

    let to_parsed ~compiler ms =
      let Mod_state.{ m; phase_result; _ } = ms in
      match phase_result with
      | Ok (Mod_state.NotParsed as prev) ->
          let phase_result = trans m |> wrap_failure ~prev in
          Mod_state.{ ms with phase_result }
      | _ -> ms
  end

  module Phase1_collect_toplevels = struct
    let trans ~builtin ~m ~subst ast =
      let open Result.Let_syntax in
      let Mod.{ menv; ds; _ } = m in

      let%bind p1ast =
        let ctx = Sema.Phase1.context ~m ~subst ~builtin in
        Sema.Phase1.collect_toplevels ~ctx ast
      in

      let phase = Mod_state.Phase1CollectTopLevels p1ast in
      Ok phase

    let to_analyzed ~compiler ~builtin ~subst ms =
      let Mod_state.{ m; phase_result; _ } = ms in
      match phase_result with
      | Ok (Mod_state.Parsed ast as prev) ->
          let phase_result =
            trans ~builtin ~m ~subst ast |> wrap_failure ~prev
          in
          Mod_state.{ ms with phase_result }
      | _ -> ms
  end

  module Phase1_declare_toplevels = struct
    let trans ~builtin ~m ~subst p1ast =
      let open Result.Let_syntax in
      let%bind subst =
        let ctx = Sema.Phase1_1.context ~m ~subst ~builtin in
        Sema.Phase1_1.declare_toplevels ~ctx p1ast
        |> Result.map ~f:(fun e -> Sema.Phase1_1.(ctx.subst))
      in

      let phase = Mod_state.Phase1DeclareTopLevels p1ast in
      Ok (phase, subst)

    let to_analyzed ~compiler ~builtin ~subst ms =
      let Mod_state.{ m; phase_result; _ } = ms in
      match phase_result with
      | Ok (Mod_state.Phase1CollectTopLevels p1ast as prev) ->
          let (phase_result, subst) =
            trans ~builtin ~m ~subst p1ast |> wrap_failure' ~prev ~subst
          in
          (Mod_state.{ ms with phase_result }, subst)
      | _ -> (ms, subst)
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

      let phase = Mod_state.Phase2 p2ast in
      Ok phase

    let to_analyzed ~compiler ~builtin ~subst ms =
      let Mod_state.{ m; phase_result; _ } = ms in
      match phase_result with
      | Ok (Mod_state.Phase1DeclareTopLevels p1ast as prev)
        when not (Mod_state.has_errors ms) ->
          let phase_result =
            trans ~builtin ~m ~subst p1ast |> wrap_failure ~prev
          in
          Mod_state.{ ms with phase_result }
      | _ -> ms
  end

  let phase3 ~compiler ~m p2ast =
    let Mod.{ ds; _ } = m in
    let subst = Mod.subst_of m in

    let ctx = Sema.Phase3.context ~ds ~subst in
    let env = Sema.Phase3.Env.create () in
    Sema.Phase3.normalize ~ctx ~env p2ast
end

module To_rill_ir = struct
  let trans ~compiler ~pkg_space ~builtin ms p3ast =
    let Mod_state.{ m; _ } = ms in
    let Pkg_buildspace.{ root_mod_env; _ } = pkg_space in

    let rir =
      let ctx = Codegen.Rir_gen.context ~m ~builtin ~root_mod_env in
      Codegen.Rir_gen.generate_module ~ctx p3ast
    in

    let phase =
      let art = Emitter.Artifact.Rill_ir { m = rir } in
      Mod_state.Artifact art
    in
    Ok phase

  let to_artifact ~compiler ~pkg_space ~builtin ms =
    let Mod_state.{ m; phase_result; _ } = ms in
    match phase_result with
    | Ok (Mod_state.Phase2 p2ast as prev) when not (Mod_state.has_errors ms) ->
        let p3ast =
          (* TODO: check that there are no errors in ds *)
          Phases.phase3 ~compiler ~m p2ast
        in
        let phase_result =
          trans ~compiler ~pkg_space ~builtin ms p3ast |> wrap_failure ~prev
        in
        Mod_state.{ ms with phase_result }
    (* ignore *)
    | _ -> ms
end

module To_llvm_ir = struct
  let trans ~compiler dict builtin ms rir =
    let Mod_state.{ m; _ } = ms in
    let Mod.{ ds; _ } = m in
    let subst = Mod.subst_of m in

    (* *)
    let rir_mod = Codegen.Rir_gen_filters.finish ~subst rir in

    let llvm =
      let ctx = Llvm_gen.context ~ds ~subst ~builtin in
      Llvm_gen.generate_module ~ctx rir
    in
    let phase =
      let art = Emitter.Artifact.Llvm_ir { m = llvm } in
      Mod_state.Artifact art
    in
    Ok phase

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
