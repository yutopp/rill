(*
 * Copyright yutopp 2015 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries
open Type_sets
open Ctfe_value

module L = Llvm
module LE = Llvm_executionengine
module COS = Codegen_option_spec

module TAst = Tagged_ast
module StringSet = Set.Make(String)

type t = {
    cg_ctx                  : Codegen_llvm.ctx_t;
    exec_engine             : LE.llexecutionengine;
    build_options           : Codegen_option_spec.t list;
    loaded_libs             : (string, Dl.library) Hashtbl.t;
    mutable loaded_funcs    : StringSet.t;
}

module JITCounter =
  struct
    let counter = ref Int64.zero

    let generate_fresh_name () =
      let c = !counter in
      if c = Int64.max_int then
        failwith "[ICE] Internal JIT cache id is reached to max id...";
      counter := Int64.succ !counter;

      "__rill_jit_tmp_expr_" ^ (Int64.to_string c)
  end

let initialize type_sets uni_map build_options =
  if not (LE.initialize ()) then
    failwith "[ICE] Couldn't initialize LLVM backend";

  let module CgCtx = Codegen_llvm.Ctx in
  let codegen_ctx =
    Codegen_llvm.make_default_context ~type_sets:type_sets
                                      ~uni_map:uni_map
                                      ~target_module:None
  in
  Codegen_llvm.inject_builtins codegen_ctx;

  let llmod = codegen_ctx.CgCtx.ir_module in
  let jit_engine = LE.create llmod in
  {
    cg_ctx = codegen_ctx;
    exec_engine = jit_engine;
    build_options = build_options;
    loaded_libs = Hashtbl.create 0;
    loaded_funcs = StringSet.empty;
  }

(* TODO: support Windows/Mac environment *)
let make_shared_lib_name basename =
  "lib" ^ basename ^ ".so"

let find_dyn_lib_from_path search_dirs filename =
  try
    let search search_dir =
      let path = Filename.concat search_dir filename in
      Debug.printf "Search dyn lib: %s" path;
      if Sys.file_exists path then
        Some path
      else
        None
    in
    List.find_map search search_dirs |> Option.some
  with
    Not_found -> None

(* First, try to find the lib from user provided dirs.
 * If the lib is found, use the found path.
 * Otherwise, use the libname immediately and rely on dlopen search algorithm *)
let load_dyn_lib search_dirs lib_name =
  let shared_lib_name = make_shared_lib_name lib_name in
  let shared_lib_path =
    match find_dyn_lib_from_path search_dirs shared_lib_name with
    | Some path -> path
    | None -> shared_lib_name
  in
  Debug.printf "Load dyn lib: %s" shared_lib_path;
  Dl.dlopen ~filename:shared_lib_path ~flags:[Dl.RTLD_LAZY]

let prepare_dyn_libs engine =
  match Hashtbl.is_empty engine.loaded_libs with
  | false ->
     Hashtbl.values engine.loaded_libs |> List.of_enum
  | true ->
     let search_dirs =
       engine.build_options
       |> List.filter_map (fun o -> match o with COS.OsLinkDir s -> Some s | _ -> None)
     in
     let lib_names =
       engine.build_options
       |> List.filter_map (fun o -> match o with COS.OsLinkLib s -> Some s | _ -> None)
     in

     let load_lib lib_name =
       let lib = load_dyn_lib search_dirs lib_name in
       Hashtbl.add engine.loaded_libs lib_name lib;
       lib
     in
     List.map load_lib lib_names

let load_dyn_libs engine =
  let module CgCtx = Codegen_llvm.Ctx in

  let libs = prepare_dyn_libs engine in

  let ext_func_names =
    CgCtx.enum_of_external_function_names engine.cg_ctx
    |> StringSet.of_enum
  in
  let diff_func_names =
    StringSet.diff ext_func_names engine.loaded_funcs
  in

  Debug.printf "CtfeENGINE: load funcs / NUM = %d (Loaded = %d)"
               (StringSet.cardinal ext_func_names)
               (StringSet.cardinal engine.loaded_funcs);
  let load_func name s =
    Debug.printf "CtfeENGINE: load func = %s" name;

    let try_to_load lib =
      try
        Dl.dlsym ~handle:lib ~symbol:name
        |> Option.some
      with
      | Dl.DL_error _ -> None
    in
    try
      let f = List.find_map try_to_load libs in
      let fp = Ctypes.ptr_of_raw_address f in
      let gv = CgCtx.find_external_function_by_name engine.cg_ctx name in
      LE.add_global_mapping gv fp engine.exec_engine;

      StringSet.add name s
    with
    | Not_found ->
       failwith ("[ERR] function " ^ name ^ " coultn't be loaded")
  in
  let new_set =
    StringSet.fold load_func diff_func_names engine.loaded_funcs
  in
  engine.loaded_funcs <- new_set;

  ()


let invoke_function engine fname ret_ty type_sets =
  let open Ctypes in
  let call_by_type fn_ty =
    let jit_f =
         LE.get_function_address fname (Foreign.funptr fn_ty)
                                 engine.exec_engine
    in
    jit_f ()
  in

  match ret_ty with
  | ty when Type.has_same_class ty type_sets.ts_type_type ->
     begin
       let cfunc_ty = Ctypes.void @-> returning Ctypes.int64_t in
       let ret_val = call_by_type cfunc_ty in

       let ty =
         Type.Generator.find_type_by_cache_id type_sets.ts_type_gen ret_val in
       Ctfe_value.Type ty
     end

  | ty when Type.has_same_class ty !(type_sets.ts_bool_type_holder) ->
     begin
       let cfunc_ty = Ctypes.void @-> returning Ctypes.bool in
       let ret_val = call_by_type cfunc_ty in

       Ctfe_value.Bool ret_val
     end

  | ty when Type.has_same_class ty !(type_sets.ts_int32_type_holder) ->
     begin
       let cfunc_ty = Ctypes.void @-> returning Ctypes.int32_t in
       let ret_val = call_by_type cfunc_ty in

       Ctfe_value.Int32 ret_val
     end

  | ty when Type.has_same_class ty !(type_sets.ts_uint32_type_holder) ->
     begin
       let cfunc_ty = Ctypes.void @-> returning Ctypes.uint32_t in
       let ret_val = call_by_type cfunc_ty in

       Ctfe_value.Uint32 (Stdint.Uint32.of_int32 (Unsigned.UInt32.to_int32 ret_val))
     end

  | _ ->
     begin
       failwith "[ICE] this type is not supported"
     end

let register_metavar engine ctfe_val env =
  Codegen_llvm.register_metaval ctfe_val env engine.cg_ctx

let execute' engine expr_node expr_ty type_sets =
  let module CgCtx = Codegen_llvm.Ctx in
  (* TODO: add cache... *)
  (*Debug.printf "JIT ==== execute!!!\n";*)

  (* save the module which has previous definitions to JIT engine *)
  LE.add_module engine.cg_ctx.CgCtx.ir_module engine.exec_engine;

  (* generate a new module to define a temporary function for CTFE *)
  (*Codegen_llvm.regenerate_module engine.cg_ctx;*)

  (* alias *)
  let ir_ctx = engine.cg_ctx.CgCtx.ir_context in
  let ir_mod = engine.cg_ctx.CgCtx.ir_module in
  let ir_builder = engine.cg_ctx.CgCtx.ir_builder in

  (**)
  let expr_llty = Codegen_llvm.lltype_of_typeinfo expr_ty engine.cg_ctx in

  (**)
  let tmp_expr_fname = JITCounter.generate_fresh_name () in

  (* declare temporary funtion : unit -> (typeof expr) *)
  let f_ty = L.function_type expr_llty [||] in
  let f = L.declare_function tmp_expr_fname f_ty ir_mod in

  let bb = L.append_block ir_ctx "entry" f in
  L.position_at_end bb ir_builder;

  (* generate a LLVM value from the expression *)
  try
    begin
      let (expr_llval, _, is_addr, _) =
        Codegen_llvm.generate_code expr_node Codegen_flowinfo.empty engine.cg_ctx
      in
      (* CTFEed value must not be addressed form *)
      let expr_llval = match is_addr with
        | true -> L.build_load expr_llval "" ir_builder
        | faise -> expr_llval
      in
      ignore @@ L.build_ret expr_llval ir_builder;

      Debug.printf ">>> CtfeENGINE: DUMP FUNC";
      Llvm_analysis.assert_valid_function f;
      Codegen_llvm.debug_dump_value f;

      (**)
      LE.add_module ir_mod engine.exec_engine;

      let _ = load_dyn_libs engine in

      (**)
      let ctfe_val = invoke_function engine tmp_expr_fname expr_ty type_sets in

      (* Remove the module for this tmporary function from execution engine.
       * However, the module will be used until next time.
       *)
      LE.remove_module ir_mod engine.exec_engine;
      (* Codegen_llvm.debug_dump_module engine.cg_ctx.CgCtx.ir_module; *)

      L.delete_function f;

      ctfe_val
    end
  with
  | Ctfe_exn.Meta_var_un_evaluatable uni_id ->
     begin
       L.delete_function f;
       Ctfe_value.Undef uni_id
     end

(* TODO: implement cache *)
let execute engine expr_node expr_ty type_sets =
  match expr_node with
  | TAst.GenericId (name, lt_args, (_, Some rel_env)) ->
     begin
       let { Env.er = er; _ } = rel_env in
       match er with
       | Env.Class (_, r) ->
          let ty_attr_val_default = {
              Type_attr.ta_ref_val = Type_attr.Val;
              Type_attr.ta_mut = Type_attr.Const;
            } in
            (* generics *)
            assert (List.length r.Env.cls_generics_vals >= List.length lt_args);
            let generics_args =
              let rec f pl al acc =
                match (pl, al) with
                | ([], []) -> acc
                | ([], _) -> failwith ""
                | (_::px, []) -> f px [] (Lifetime.LtUndef :: acc)  (* TODO: check undef *)
                | (p::px, a::ax) -> f px ax (a :: acc)
              in
              f r.Env.cls_generics_vals lt_args [] |> List.rev
            in

            (* type *)
            let ty =
              Type.Generator.generate_type type_sets.Type_sets.ts_type_gen
                                           (Type_info.UniqueTy rel_env)
                                           r.Env.cls_template_vals
                                           generics_args
                                           ty_attr_val_default
            in

            Ctfe_value.Type ty
       | _ ->
          execute' engine expr_node expr_ty type_sets
     end

  (* execute normally... *)
  | _ ->
    execute' engine expr_node expr_ty type_sets
