(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base
module Diagnostics = Common.Diagnostics
module Package = Common.Package
module Mod = Sema.Mod

type t = {
  mutable has_fatal : bool;
  workspace : Common.Workspace.t;
  host : (module Triple.PRESET);
  target : (module Triple.PRESET);
  subst_counter : Common.Counter.t;
}

let create ~workspace ~host ~target : t =
  let subst_counter = Common.Counter.create () in
  { has_fatal = false; workspace; host; target; subst_counter }

module ModState = struct
  type t = {
    m : Mod.t;
    phase_result : (phase_t, failed_t) Result.t;
    format : Emitter.t option;
  }

  and phase_t =
    | NotParsed
    | Parsed of Syntax.Ast.t
    | Phase1CollectTopLevels of Sema.Phase1.TopAst.t
    | Phase1DeclareTopLevels of Sema.Phase1.TopAst.t
    | Phase2 of Sema.Phase2.TAst.t
    | Artifact of Emitter.Artifact.t

  and failed_t = { previous_phase : phase_t; last_error : Diagnostics.Elem.t }

  let create ~m = { m; phase_result = Ok NotParsed; format = None }

  (* if true, cannot forward steps completely *)
  let has_fatal m = Result.is_error m.phase_result

  let has_errors m = Mod.has_errors m.m || has_fatal m
end

let wrap_pipeline_failure ~prev phase_result_res =
  match phase_result_res with
  | Ok phase_result -> phase_result
  | Error last_error -> Error ModState.{ previous_phase = prev; last_error }

module Phases = struct
  module Parse = struct
    let trans m =
      let open Result.Let_syntax in
      let Mod.{ ds; path; _ } = m in

      (* TODO: check state and ds to restrict compilaction *)
      let%bind (_stete, parsed) = Syntax.parse_from_file ~ds path in
      let phase_result = Ok (ModState.Parsed parsed) in
      Ok phase_result

    let to_parsed ~compiler ms =
      match ms.ModState.phase_result with
      | Ok (ModState.NotParsed as prev) ->
          let ModState.{ m; _ } = ms in
          let phase_result = trans m |> wrap_pipeline_failure ~prev in
          ModState.{ ms with phase_result }
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

      let phase_result = Ok (ModState.Phase1CollectTopLevels p1ast) in
      Ok phase_result

    let to_analyzed ~compiler ~builtin ~menv ms =
      match ms.ModState.phase_result with
      | Ok (ModState.Parsed ast as prev) ->
          let ModState.{ m; _ } = ms in
          let phase_result =
            trans ~builtin ~m ~menv ast |> wrap_pipeline_failure ~prev
          in
          ModState.{ ms with phase_result }
      | _ -> ms
  end

  module Phase1_declare_toplevels = struct
    let trans ~builtin ~m ~pkg_env p1ast =
      let open Result.Let_syntax in
      let Mod.{ ds; subst; _ } = m in

      let%bind subst =
        let ctx = Sema.Phase1_1.context ~ds ~subst ~builtin ~pkg_env in
        Sema.Phase1_1.declare_toplevels ~ctx p1ast
        |> Result.map ~f:(fun e -> Sema.Phase1_1.(ctx.subst))
      in
      m.Sema.Mod.subst <- subst;

      let phase_result = Ok (ModState.Phase1DeclareTopLevels p1ast) in
      Ok phase_result

    let to_analyzed ~compiler ~builtin ~pkg_env ms =
      match ms.ModState.phase_result with
      | Ok (ModState.Phase1CollectTopLevels p1ast as prev) ->
          let ModState.{ m; _ } = ms in
          let phase_result =
            trans ~builtin ~m ~pkg_env p1ast |> wrap_pipeline_failure ~prev
          in
          ModState.{ ms with phase_result }
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

      let phase_result = Ok (ModState.Phase2 p2ast) in
      Ok phase_result

    let to_analyzed ~compiler ~builtin ms =
      match ms.ModState.phase_result with
      | Ok (ModState.Phase1DeclareTopLevels p1ast as prev)
        when not (ModState.has_errors ms) ->
          let ModState.{ m; _ } = ms in
          let phase_result =
            trans ~builtin ~m p1ast |> wrap_pipeline_failure ~prev
          in
          ModState.{ ms with phase_result }
      | _ -> ms
  end

  let phase3 ~compiler ~m p2ast =
    let Mod.{ ds; subst; _ } = m in

    let ctx = Sema.Phase3.context ~ds ~subst in
    let env = Sema.Phase3.Env.create () in
    Sema.Phase3.normalize ~ctx ~env p2ast
end

module ModDict = struct
  type t = { root_mod_env : Sema.Env.t; rel : (string, ModState.t) Hashtbl.t }

  let create root_mod_env =
    { root_mod_env; rel = Hashtbl.create (module String) }

  let update dict ms =
    let key = ms.ModState.m.Mod.path in
    Hashtbl.set dict.rel ~key ~data:ms

  let to_alist d = Hashtbl.to_alist d.rel
end

module PkgDict = struct
  type t = { rel : (Package.id_t, Package.t * ModDict.t) Hashtbl.t }

  let create () = { rel = Hashtbl.create (module Int) }

  let update d ~key ~data : unit =
    Hashtbl.add_exn d.rel ~key:key.Package.id ~data:(key, data)

  let get d ~key =
    let (_, mod_dict) = Hashtbl.find_exn d.rel key.Package.id in
    mod_dict

  let to_alist d = Hashtbl.to_alist d.rel |> List.map ~f:snd
end

let return_if_failed ~compiler ms r =
  let open Base.With_return in
  if ModState.has_fatal ms then (
    compiler.has_fatal <- true;
    r.return ms )

let preload_module ~compiler ~builtin ~menv ms =
  let open Base.With_return in
  with_return (fun r ->
      (* parse *)
      let ms = Phases.Parse.to_parsed ~compiler ms in
      return_if_failed ~compiler ms r;

      (* phase1 *)
      let ms =
        Phases.Phase1_collect_toplevels.to_analyzed ~compiler ~builtin ~menv ms
      in
      return_if_failed ~compiler ms r;

      ms)

let rec preload_pkg compiler dict builtin pkg =
  (* preload deps *)
  let deps = Package.deps pkg in
  List.iter deps ~f:(preload_pkg compiler dict builtin);

  (* preload self *)
  let mod_dict =
    (* TODO: create root module for pkg *)
    let root_mod_env =
      let root_mod =
        let path = Package.base_dir pkg (* TODO: fix *) in

        (* TODO: fix treatment of subst *)
        let subst_id = Common.Counter.fresh compiler.subst_counter in
        let subst = Typing.Subst.create subst_id in

        Mod.create ~path ~subst ~pkg
      in
      let visibility = Sema.Env.Public in
      let binding_mut = Typing.Type.MutImm in
      let ty =
        Typing.Type.{ ty = Module; binding_mut; span = Common.Span.undef }
      in
      Sema.Env.create pkg.Package.name ~parent:None ~visibility ~ty
        ~ty_w:(Sema.Env.M root_mod)
    in

    let dict = ModDict.create root_mod_env in

    let paths = Package.src_paths pkg in
    List.iter paths ~f:(fun path ->
        (* TODO: fix treatment of subst *)
        let subst_id = Common.Counter.fresh compiler.subst_counter in
        let subst = Typing.Subst.create subst_id in

        let m = Mod.create ~path ~subst ~pkg in
        let menv =
          let name =
            Caml.Filename.basename path |> Caml.Filename.remove_extension
          in
          let visibility = Sema.Env.Public in
          let binding_mut = Typing.Type.MutImm in
          let ty =
            Typing.Type.{ ty = Module; binding_mut; span = Common.Span.undef }
          in
          Sema.Env.create name ~parent:None ~visibility ~ty ~ty_w:(Sema.Env.M m)
        in

        let ms = ModState.create ~m in
        let ms = preload_module ~compiler ~builtin ~menv ms in

        (* TODO: open modules by root module *)
        Sema.Env.insert_meta root_mod_env menv |> Sema.Phase1.assume_new;

        ModDict.update dict ms);
    dict
  in
  PkgDict.update dict ~key:pkg ~data:mod_dict

let rec preload_pkg_cont compiler dict builtin pkg_env pkg =
  (* preload deps *)
  let deps = Package.deps pkg in
  List.iter deps ~f:(preload_pkg_cont compiler dict builtin pkg_env);

  let mod_dict = PkgDict.get dict ~key:pkg in
  let mod_rels = ModDict.to_alist mod_dict in
  List.iter mod_rels ~f:(fun (path, ms) ->
      let ms =
        Phases.Phase1_declare_toplevels.to_analyzed ~compiler ~builtin ~pkg_env
          ms
      in
      ModDict.update mod_dict ms)

module Pipeline_common = struct
  module To_rill_ir = struct
    let trans ~compiler dict builtin ms p3ast =
      let ModState.{ m; _ } = ms in
      let Mod.{ ds; subst; _ } = m in

      let rir =
        let ctx = Codegen.Rir_gen.context ~ds ~subst ~builtin in
        Codegen.Rir_gen.generate_module ~ctx p3ast
      in
      let phase_result =
        let art = Emitter.Artifact.Rill_ir { m = rir } in
        Ok (ModState.Artifact art)
      in
      Ok phase_result

    let to_artifact ~compiler dict builtin ms =
      match ms.ModState.phase_result with
      | Ok (ModState.Phase2 p2ast as prev) ->
          let p3ast =
            (* TODO: check that there are no errors in ds *)
            let ModState.{ m; _ } = ms in
            Phases.phase3 ~compiler ~m p2ast
          in
          let phase_result =
            trans ~compiler dict builtin ms p3ast |> wrap_pipeline_failure ~prev
          in
          ModState.{ ms with phase_result }
      (* ignore *)
      | _ -> ms
  end

  module To_llvm_ir = struct
    let trans ~compiler dict builtin ms rir =
      let ModState.{ m; _ } = ms in
      let Mod.{ ds; subst; _ } = m in

      let llvm =
        let ctx = Llvm_gen.context ~ds ~subst ~builtin in
        Llvm_gen.generate_module ~ctx rir
      in
      let phase_result =
        let art = Emitter.Artifact.Llvm_ir { m = llvm } in
        Ok (ModState.Artifact art)
      in
      Ok phase_result

    let to_artifact ~compiler dict builtin ms =
      match ms.ModState.phase_result with
      | Ok (ModState.Artifact (Emitter.Artifact.Rill_ir { m }) as prev) ->
          let phase_result =
            trans ~compiler dict builtin ms m |> wrap_pipeline_failure ~prev
          in
          ModState.{ ms with phase_result }
      (* *)
      | _ -> ms
  end
end

module type TO_ARTIFACT = sig
  val to_artifact : compiler:t -> ModState.t -> ModState.t
end

module type PIPELINE_TARGET = sig
  val supported_formats : Emitter.t list

  module To_native : TO_ARTIFACT
end

module Pipeline_target_native : PIPELINE_TARGET = struct
  let supported_formats = [ Emitter.Asm; Emitter.Obj ]

  module To_native : TO_ARTIFACT = struct
    let trans ~m ~target llvm =
      let open Result.Let_syntax in
      let Mod.{ ds; subst; _ } = m in

      let%bind native = Llvm_gen.Backend.create ~triple:target llvm in
      let phase_result =
        let art = Emitter.Artifact.Native { native } in
        Ok (ModState.Artifact art)
      in
      Ok phase_result

    let to_artifact ~compiler ms =
      match ms.ModState.phase_result with
      | Ok (ModState.Artifact (Emitter.Artifact.Llvm_ir { m = llvm }) as prev)
        ->
          let ModState.{ m; _ } = ms in
          let (module Target : Triple.PRESET) = compiler.target in
          let phase_result =
            trans ~m ~target:Target.name llvm |> wrap_pipeline_failure ~prev
          in
          ModState.{ ms with phase_result }
      (* *)
      | _ -> ms
  end
end

let to_artifact ~compiler dict builtin ms format =
  let format =
    let (module Target : Triple.PRESET) = compiler.target in
    Option.value format ~default:(Emitter.default_emitter_of Target.triple)
  in

  let open Base.With_return in
  with_return (fun r ->
      (* to rill-ir *)
      let ms =
        Pipeline_common.To_rill_ir.to_artifact ~compiler dict builtin ms
      in
      return_if_failed ~compiler ms r;
      let () =
        match format with
        | Emitter.Rill_ir -> r.return ModState.{ ms with format = Some format }
        | _ -> ()
      in

      (* to llvm-ir *)
      let ms =
        Pipeline_common.To_llvm_ir.to_artifact ~compiler dict builtin ms
      in
      return_if_failed ~compiler ms r;
      let () =
        match format with
        | Emitter.Llvm_ir | Emitter.Llvm_bc ->
            r.return ModState.{ ms with format = Some format }
        | _ -> ()
      in

      (* to native *)
      let ms = Pipeline_target_native.To_native.to_artifact ~compiler ms in
      return_if_failed ~compiler ms r;
      let () =
        match format with
        | Emitter.Asm | Emitter.Obj ->
            r.return ModState.{ ms with format = Some format }
        | _ -> ()
      in

      ms)

let build_pkg_internal compiler dict builtin pkg format =
  let mod_dict = PkgDict.get dict ~key:pkg in
  let mod_rels = ModDict.to_alist mod_dict in
  List.iter mod_rels ~f:(fun (path, ms) ->
      let ms = Phases.Phase2.to_analyzed ~compiler ~builtin ms in
      ModDict.update mod_dict ms;

      (* *)
      let has_errors = ModState.has_errors ms in
      if has_errors then compiler.has_fatal <- true
      else
        let ms = to_artifact ~compiler dict builtin ms format in
        ModDict.update mod_dict ms)

let build_mod_env pkg_dict =
  (* Ignore modules which couldn't reach to phase1 *)
  let visibility = Sema.Env.Public in
  let binding_mut = Typing.Type.MutImm in
  let ty = Typing.Type.{ ty = Module; binding_mut; span = Common.Span.undef } in
  let env = Sema.Env.create "" ~parent:None ~visibility ~ty ~ty_w:Sema.Env.N in

  let pkg_rels = PkgDict.to_alist pkg_dict in
  List.iter pkg_rels ~f:(fun (_, mod_dict) ->
      let root_mod_env = mod_dict.ModDict.root_mod_env in
      Sema.Env.insert_meta env root_mod_env |> Sema.Phase1.assume_new);

  env

let build_pkg compiler pkg ~format =
  let pkg_dict = PkgDict.create () in
  let builtin = Sema.Builtin.create () in

  let open With_return in
  with_return (fun r ->
      (* TODO: check state and ds to restrict compilaction *)
      preload_pkg compiler pkg_dict builtin pkg;
      if compiler.has_fatal then r.return pkg_dict;

      let mod_env = build_mod_env pkg_dict in

      (* TODO: prevent cyclic imports *)
      preload_pkg_cont compiler pkg_dict builtin mod_env pkg;
      if compiler.has_fatal then r.return pkg_dict;

      (* TODO: check that all top-levels have bound type-vars *)
      build_pkg_internal compiler pkg_dict builtin pkg format;

      pkg_dict)
