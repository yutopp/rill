(*
 * Copyright yutopp 2017 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

(*
 * Special member has two state, 'implicit-defined' or 'not'.
 *   If implicit, there are to state, 'trivial' or 'non-trivial'
 *
 * There are 6 special membersa
 *   Default constructor
 *   Copy constructor
 *   Move constructor (not implemented)
 *   Copy assign
 *   Move assign (not implemented)
 *   Destructor
 *)

open Batteries

module SpecialMemberStates = struct
  type state_t = {
    is_explicitly_defined   : bool;
    is_callable             : bool;
    is_trivial              : bool;
    is_provided_by_user     : bool;
  }

  let init_scanner =
    {
      is_explicitly_defined = false;
      is_callable = true;
      is_trivial = true;
      is_provided_by_user = false;
    }

  let merge_special_func_state ?(explicit=false) diagnosis state =
    match state with
    | Env.FnDefDefaulted b ->
       { diagnosis with
         is_explicitly_defined = explicit;
         is_callable = true && diagnosis.is_callable;
         is_trivial = b && diagnosis.is_trivial;
       }
    | Env.FnDefProvidedByUser ->
       {
         is_explicitly_defined = explicit;
         is_callable = true && diagnosis.is_callable;
         is_trivial = false && diagnosis.is_trivial;
         is_provided_by_user = true;
       }
    | Env.FnDefDeleted ->
       { diagnosis with
         is_explicitly_defined = explicit;
         is_callable = false && diagnosis.is_callable;
         is_trivial = false && diagnosis.is_trivial;
       }

  (* use this function after diagnose *)
  let diagnosis_to_trait diagnosis has_user_defined =
    match diagnosis with
    | { is_callable = true; is_trivial = false; is_provided_by_user = true } ->
       Env.FnDefProvidedByUser

    | { is_callable = true; is_trivial = b; is_provided_by_user = false } ->
       Env.FnDefDefaulted b

    | _ ->
       Env.FnDefDeleted

  let string_of_state s =
    Printf.sprintf "is_explicit = %b; is_callable = %b;\n is_trivial = %b; is_provided_by_user = %b" s.is_explicitly_defined s.is_callable s.is_trivial s.is_provided_by_user

  type t =
      {
        default_ctor_diagnosis  : state_t;
        copy_ctor_diagnosis     : state_t;
        has_user_defined_ctor   : bool;     (* has constructors except for default/copy/move *)
        dtor_diagnosis          : state_t;
      }

  let init_states =
    {
      has_user_defined_ctor = false;
      default_ctor_diagnosis = init_scanner;
      copy_ctor_diagnosis = init_scanner;

      dtor_diagnosis = init_scanner;
    }

  let merge_special_func_traits diagnoses traits =
    {
      diagnoses with
      default_ctor_diagnosis =
        merge_special_func_state diagnoses.default_ctor_diagnosis
                                 traits.Env.cls_traits_default_ctor_state;
      copy_ctor_diagnosis =
        merge_special_func_state diagnoses.copy_ctor_diagnosis
                                 traits.Env.cls_traits_copy_ctor_state;

      dtor_diagnosis =
        merge_special_func_state diagnoses.dtor_diagnosis
                                 traits.Env.cls_traits_dtor_state;
    }

  let diagnoses_to_traits traits states =
    {
      traits with

      Env.cls_traits_default_ctor_state =
        diagnosis_to_trait states.default_ctor_diagnosis states.has_user_defined_ctor;
      Env.cls_traits_copy_ctor_state =
        diagnosis_to_trait states.copy_ctor_diagnosis states.has_user_defined_ctor;

      Env.cls_traits_dtor_state =
        diagnosis_to_trait states.dtor_diagnosis false; (* dtor has no user_defined one *)
    }

  let string_of_states ss =
    let s = Printf.sprintf "default_ctor_state -> %s\n"
                           (string_of_state ss.default_ctor_diagnosis)
    in
    let s = s ^ Printf.sprintf "copy_ctor_state -> %s\n"
                               (string_of_state ss.copy_ctor_diagnosis)
    in
    let s = s ^ Printf.sprintf "has_user_defined_ctor -> %b\n"
                               ss.has_user_defined_ctor
    in
    let s = s ^ Printf.sprintf "dtor_state -> %s\n"
                               (string_of_state ss.dtor_diagnosis)
    in
    s
end

type special_func_type =
  | SfUserDefinedCtor
  | SfDefaultCtor
  | SfCopyCtor
  | SfDtor
  | SfNone

type special_func_op =
  | SfOpDefault
  | SfOpDelete
  | SfOpNone

let constructor_kind param_kinds cenv ctx =
  let required_params = Sema_type.exclude_optional_params param_kinds in

  (* TODO: check non-templated for copy/move ctor *)
  match List.length required_params with
  (* default constructor *)
  | 0 ->
     SfDefaultCtor
  (* copy/mode constructor *)
  | 1 ->
     begin
       let ref_self_ty =
         let attr = Type_attr.make (Type_attr.Ref []) Type_attr.Const in
         Sema_type.make_class_type cenv attr cenv ctx (* XXX: *)
       in
       (* TODO: support move ctor *)
       match List.hd required_params with
       (* copy ctor *)
       | ty when Type.is_same_class_ref ref_self_ty ty ->
          SfCopyCtor
       (* else *)
       | _ ->
          SfUserDefinedCtor
     end
  | _ ->
     SfUserDefinedCtor

let destructor_kind param_kinds =
  (* destructor disallows default paramaters, thus use param_kinds directly *)
  match List.length param_kinds with
  | 1 -> (* implicit 'this' param *)
     SfDtor
  | _ ->
     failwith "[ICE] dtor cannot have params"


let scan_variables cenv_r =
  let f diagnoses venv =
    let venv_r = Env.VariableOp.get_record venv in
    let var_ty = venv_r.Env.var_type in
    let vcenv = Type.as_unique var_ty in
    let vcenv_r = Env.ClassOp.get_record vcenv in
    let vcls_traits = vcenv_r.Env.cls_traits in

    SpecialMemberStates.merge_special_func_traits diagnoses vcls_traits
  in
  List.fold_left f SpecialMemberStates.init_states cenv_r.Env.cls_member_vars



let special_func_kind fenv cenv ctx =
  let open Sema_definitions in
  let open SpecialMemberStates in
  match Env.get_env_record fenv with
  (* if there are template constructors at least 1, class has user-defined constructor *)
  | Env.Template r ->
     begin
       match r.Env.tl_name with
       (* template has ctor name.
        * TODO: support default constructable *)
       | Id_string.Pure n when n = ctor_name ->
          SfUserDefinedCtor
       | _ ->
          SfNone
     end

  (* normal functions *)
  | Env.Function (_, r)  ->
     begin
       match r.Env.fn_name with
       | Id_string.Pure n when n = ctor_name ->
          constructor_kind r.Env.fn_param_kinds cenv ctx

       | Id_string.Pure n when n = dtor_name ->
          destructor_kind r.Env.fn_param_kinds

       | _ ->
          SfNone
     end

  | _ ->
     SfNone

let scan_member_funcs diagnoses name cenv_r cenv ctx =
  let special_func_types =
    List.map (fun fenv -> special_func_kind fenv cenv ctx)
             cenv_r.Env.cls_member_funcs
  in

  let _ =
    let memoize func_type fenv =
      match func_type with
      | SfDefaultCtor ->
         Sema_utils.register_default_ctor_to_class_env cenv fenv
      | SfCopyCtor ->
         Sema_utils.register_copy_ctor_to_class_env cenv fenv
      | SfDtor ->
         Sema_utils.register_dtor_to_class_env cenv fenv
      | _ ->
         ()
    in
    List.iter2 memoize special_func_types cenv_r.Env.cls_member_funcs
  in

  let has_user_provided_default_ctor =
    List.exists ((=) SfDefaultCtor) special_func_types
  in

  let has_user_provided_copy_ctor =
    List.exists ((=) SfCopyCtor) special_func_types
  in

  let has_user_provided_move_ctor = false in

  let has_user_defined_ctor =
    List.exists ((=) SfUserDefinedCtor) special_func_types
  in

  let has_user_provided_ctor =
    has_user_provided_default_ctor
    || has_user_provided_copy_ctor
    || has_user_defined_ctor
  in

  let has_user_provided_dtor =
    List.exists ((=) SfDtor) special_func_types
  in

  (* default ctor *)
  let default_ctor_diagnosis =
    let open SpecialMemberStates in
    let is_special_def = false in (* true iff '= default' or '= delete' are specified *)
    let special_def_op = SfOpNone in

    match has_user_provided_default_ctor with
    | true when not is_special_def ->
       {
         is_explicitly_defined = false; (* no meaning *)
         is_callable = true;
         is_trivial = false;
         is_provided_by_user = true;
       }

    | _ ->
       (* there is no user-provided default ctor OR 'default' are specified.
        * define implicit default ctor *)
       begin
         match has_user_provided_ctor with
         (* if any other ctors are defined, doesn't generate default ctor *)
         | true when not is_special_def ->
            {
              is_explicitly_defined = false;
              is_callable = false;
              is_trivial = false;
              is_provided_by_user = false;
            }

         (* defaulted *)
         | _ when special_def_op = SfOpDefault || special_def_op = SfOpNone ->
            (* TODO: check condition for trivial default ctor *)
            {
              is_explicitly_defined = false; (* no meaning *)
              is_callable = true;
              is_trivial = true;
              is_provided_by_user = false;
            }

         (* deleted *)
         | _ when special_def_op = SfOpDelete ->
            failwith "[ERR] currently, not supported"

         | _ ->
            failwith "[ICE]"
       end
  in

  (* copy ctor *)
  let copy_ctor_diagnosis =
    let open SpecialMemberStates in
    let is_special_def = false in (* true iff '= default' or '= delete' are specified *)
    let special_def_op = SfOpNone in

    match has_user_provided_copy_ctor with
    | true when not is_special_def ->
       {
         is_explicitly_defined = false; (* no meaning *)
         is_callable = true;
         is_trivial = false;
         is_provided_by_user = true;
       }

    | _ ->
       (* there is no user-provided copy ctor OR 'default' are specified.
        * define implicit copy ctor *)
       begin
         match has_user_provided_move_ctor with
         (* if move ctor are defined, doesn't generate copy ctor *)
         | true when not is_special_def ->
            {
              is_explicitly_defined = false;
              is_callable = false;
              is_trivial = false;
              is_provided_by_user = false;
            }

         (* defaulted *)
         | _ when special_def_op = SfOpDefault || special_def_op = SfOpNone ->
            (* TODO: check condition for trivial copy ctor *)
            {
              is_explicitly_defined = false; (* no meaning *)
              is_callable = true;
              is_trivial = true;
              is_provided_by_user = false;
            }

         (* deleted *)
         | _ when special_def_op = SfOpDelete ->
            failwith "[ERR] currently, not supported"

         | _ ->
            failwith "[ICE]"
       end
  in

  (* destructor *)
  let dtor_diagnosis =
    let open SpecialMemberStates in
    let is_special_def = false in (* true iff '= default' or '= delete' are specified *)
    let special_def_op = SfOpNone in

    match has_user_provided_dtor with
    | true when not is_special_def ->
       {
         is_explicitly_defined = false;
         is_callable = true;
         is_trivial = false;
         is_provided_by_user = true;
       }
    | _ ->
       begin
         match special_def_op with
         (* defaulted *)
         | SfOpDefault
         | SfOpNone ->
            (* TODO: check condition for trivial dtor *)
            {
              is_explicitly_defined = false; (* no meaning *)
              is_callable = true;
              is_trivial = true;
              is_provided_by_user = false;
            }

         (* deleted *)
         | SfOpDelete ->
            failwith "[ERR] currently, not supported"

       end
  in

  let class_states =
    let open SpecialMemberStates in
    {
      has_user_defined_ctor = has_user_defined_ctor;
      default_ctor_diagnosis = default_ctor_diagnosis;
      copy_ctor_diagnosis = copy_ctor_diagnosis;
      dtor_diagnosis = dtor_diagnosis;
    }
  in
  let open SpecialMemberStates in
  Debug.printf "MEMBERS\n";
  Debug.printf "= CLASS: %s\n%s\n" name (string_of_states diagnoses);
  Debug.printf "CTORS\n";
  Debug.printf "= CLASS: %s\n%s\n" name (string_of_states class_states);

  class_states
