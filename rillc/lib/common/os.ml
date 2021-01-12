(*
 * Copyright yutopp 2020 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open! Base

module Option = struct
  include Option

  let map_or_else opt ~default ~f =
    match opt with Some v -> f v | None -> default ()

  let ok_or_else opt ~err = match opt with Some v -> v | None -> err ()
end

exception Unexpected_result of string

exception Not_exited_with_code_zero of string * Unix.process_status

module Fd = struct
  type t = { mutable fd : Unix.file_descr option }

  let from fd = { fd = Some fd }

  let close desc =
    match desc.fd with
    | Some fd ->
        Unix.close fd;
        desc.fd <- None
    | None -> ()

  let into_in_ch desc =
    let ch = Unix.in_channel_of_descr (Option.value_exn desc.fd) in
    desc.fd <- None;
    ch

  let raw desc = Option.value_exn desc.fd
end

module Pipe = struct
  let create () =
    let (in_r, in_w) = Unix.pipe ~cloexec:true () in
    (Fd.from in_r, Fd.from in_w)
end

let current_exe () = Unix.readlink "/proc/self/exe"

let read_dir_names dir =
  let handle = Unix.opendir dir in
  Exn.protectx
    ~f:(fun handle ->
      let rec f (acc : string list) =
        try
          let name = Unix.readdir handle in
          f (name :: acc)
        with End_of_file -> acc
      in
      f [])
    handle ~finally:Unix.closedir

(* TODO: check is_file/is_dir *)
let grob_dir dir pattern =
  let reg = Str.regexp pattern in
  let names = read_dir_names dir in
  List.filter names ~f:(fun name -> Str.string_match reg name 0)

(* TODO: fix *)
let join_path paths =
  match paths with
  | [] -> ""
  | x :: xs -> List.fold_left xs ~init:x ~f:Stdlib.Filename.concat

let exec args ~f =
  let () =
    let s = String.concat ~sep:" " args in
    [%loga.debug "exec = %s" s]
  in

  let (in_r, in_w) = Pipe.create () in
  [%defer Fd.close in_w];
  [%defer Fd.close in_r];

  let (out_r, out_w) = Pipe.create () in
  [%defer Fd.close out_w];
  let out_ch = Fd.into_in_ch out_r in
  [%defer Stdlib.close_in_noerr out_ch];

  let (err_r, err_w) = Pipe.create () in
  [%defer Fd.close err_r];
  let err_ch = Fd.into_in_ch err_r in
  [%defer Stdlib.close_in_noerr err_ch];

  let pid =
    let args = Array.of_list args in
    Unix.create_process args.(0) args (Fd.raw in_r) (Fd.raw out_w)
      (Fd.raw err_w)
  in
  let (_pid, status) = Unix.waitpid [] pid in
  Fd.close out_w;
  Fd.close err_w;

  f out_ch err_ch status (String.concat ~sep:" " args)

let assume_exit_successfully ~status ~args =
  match status with
  | Unix.WEXITED 0 -> Ok ()
  | s -> Error (Not_exited_with_code_zero (args, s))

let assume_exit_successfully_with_out ~status ~args ~out_ch ~err_ch =
  let ret = assume_exit_successfully ~status ~args in
  Result.iter_error ret ~f:(fun _ ->
      let out = Stdio.In_channel.input_all out_ch in
      Stdio.Out_channel.printf "%s" out;
      let err = Stdio.In_channel.input_all err_ch in
      Stdio.Out_channel.eprintf "%s" err);
  ret

let mktemp_dir prefix =
  let open Result.Let_syntax in
  let tmp = Caml.Filename.get_temp_dir_name () in
  exec [ "mktemp"; "-d"; "-p"; tmp; "-t"; prefix ]
    ~f:(fun out_ch err_ch status args ->
      let%bind () = assume_exit_successfully ~status ~args in
      let out =
        Stdio.In_channel.input_all out_ch |> String.chop_suffix_exn ~suffix:"\n"
      in
      let%bind () =
        if Stdlib.Filename.is_relative out then Error (Unexpected_result "")
        else Ok ()
      in
      Ok out)

module Spec_env = struct
  let cc ~spec =
    Sys.getenv "RILL_CC"
    |> Option.ok_or_else ~err:(fun () -> Target_spec.(spec.cc))

  let cc_sysroot ~spec =
    Sys.getenv "RILL_CC_SYSROOT"
    |> Option.bind ~f:(fun path ->
           if String.equal path "" then None else Some path)
    |> Option.map_or_else
         ~default:(fun () -> Target_spec.(spec.cc_sysroot))
         ~f:Option.return

  let ar ~spec =
    Sys.getenv "RILL_AR"
    |> Option.ok_or_else ~err:(fun () -> Target_spec.(spec.ar))

  let ranlib ~spec =
    Sys.getenv "RILL_RANLIB"
    |> Option.ok_or_else ~err:(fun () -> Target_spec.(spec.ranlib))
end

let cc_obj src out =
  let open Result.Let_syntax in
  let args = [ [ "gcc" ] ] in
  let args = [ Printf.sprintf "-o%s" out; "-c"; src ] :: args in

  let args = args |> List.rev |> List.concat in
  exec args ~f:(fun out_ch err_ch status args ->
      let%bind () =
        assume_exit_successfully_with_out ~status ~args ~out_ch ~err_ch
      in
      Ok ())

let cc_exe ~spec ?(only_pp = false) ?(only_comp = false)
    ?(only_comp_asm = false) ?(linker_flags = []) ~lib_dirs ~lib_names ~objs
    ~out () =
  let open Result.Let_syntax in
  let args = [ [ Spec_env.cc ~spec ] ] in
  let args =
    Spec_env.cc_sysroot ~spec
    |> Option.value_map ~default:args ~f:(fun path ->
           [ Printf.sprintf "--sysroot=%s" path ] :: args)
  in
  let args = objs :: args in

  let args =
    match (only_pp, only_comp, only_comp_asm) with
    (* with linking *)
    | (false, false, false) ->
        let args = List.map lib_dirs ~f:(Printf.sprintf "-L%s") :: args in
        let args = [ "-static" ] :: args in
        let args = linker_flags :: args in
        let args = List.map lib_names ~f:(Printf.sprintf "-l%s") :: args in
        args
    (* without linking *)
    | (_, _, _) ->
        let args = (if only_pp then [ "-E" ] else []) :: args in
        let args = (if only_comp then [ "-S" ] else []) :: args in
        let args = (if only_comp_asm then [ "-c" ] else []) :: args in
        args
  in
  let args = [ Printf.sprintf "-o%s" out ] :: args in
  let args = args |> List.rev |> List.concat in
  exec args ~f:(fun out_ch err_ch status args ->
      let%bind () =
        assume_exit_successfully_with_out ~status ~args ~out_ch ~err_ch
      in
      Ok ())

let ar ~spec ~objs ~out () =
  let open Result.Let_syntax in
  let args = [ [ Spec_env.ar ~spec; "qc"; out ] ] in
  let args = objs :: args in

  let args = args |> List.rev |> List.concat in
  exec args ~f:(fun _out_ch _err_ch status args ->
      let%bind () = assume_exit_successfully ~status ~args in
      Ok ())

let ranlib ~spec ~out () =
  let open Result.Let_syntax in
  let args = [ [ Spec_env.ranlib ~spec; out ] ] in

  let args = args |> List.rev |> List.concat in
  exec args ~f:(fun _out_ch _err_ch status args ->
      let%bind () = assume_exit_successfully ~status ~args in
      Ok ())

let cp ~src ~dst =
  let open Result.Let_syntax in
  let args = [ [ "cp"; src; dst ] ] in

  let args = args |> List.rev |> List.concat in
  exec args ~f:(fun _out_ch _err_ch status args ->
      let%bind () = assume_exit_successfully ~status ~args in
      Ok ())
