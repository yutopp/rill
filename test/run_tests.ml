(*
 * Copyright yutopp 2016 - .
 *
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE_1_0.txt or copy at
 * http://www.boost.org/LICENSE_1_0.txt)
 *)

open Batteries

type target_dir_t =
    Compilable
  | Runnable

let string_of_tdir td = match td with
  | Compilable -> "compilable"
  | Runnable -> "runnable"

type context_t = {
  compiler_bin:     string;
  compiler_options: string list;
}

type executed_status =
    Success
  | Failure of int * string

let is_success x = match x with
  | Success -> true
  | Failure _ -> false

let is_failure x = match x with
  | Success -> false
  | Failure _ -> true

let string_of_stat x = match x with
  | Success -> "SUCCESS"
  | Failure (code, reason) -> Printf.sprintf "FAILURE (%d, %s)" code reason


let show_reports files stats =
  let total_num = stats |> List.length in
  let succ_num = stats |> List.filter is_success |> List.length in
  let fail_num = stats |> List.filter is_failure |> List.length in
  let unk_num = total_num - succ_num - fail_num in

  let fn_max_width = List.fold_left (fun w s -> max w (String.length s)) 0 files in

  let show file stat =
    let pretty_filename = file ^ String.make (fn_max_width - String.length file) ' ' in
    Printf.printf "%s - %s\n" pretty_filename (string_of_stat stat)
  in
  List.iter2 show files stats;

  Printf.printf "SUCCESS (%03d/%03d) : FAILURE (%03d/%03d) : UNKNOWN (%03d/%03d)\n" succ_num total_num fail_num total_num unk_num total_num;
  ()

let run_executable bin_path args stdin stdout stderr =
  let pid =
    Unix.create_process bin_path args stdin stdout stderr
  in
  let (rpid, ps) = Unix.waitpid [] pid in
  let stat = match ps with
    | Unix.WEXITED 0 -> Success
    | Unix.WEXITED code -> Failure (code, "return code")
    | Unix.WSIGNALED s -> Failure (s, "signaled")
    | _ -> Failure (0, "unexpected")
  in
  stat

let output_file filename =
  let fd = Unix.openfile filename [Unix.O_RDONLY] 0400 in
  let ch = Unix.in_channel_of_descr fd in
  let _ = try while true do
                let s = IO.nread ch 1024 in
                output_string stdout s
              done with
          | IO.No_more_input -> ()
  in
  Unix.close fd

let run_executable_test executable_filename ctx =
  let filename_output_stdout = Filename.temp_file "rill-run-test-" "-pipe-stdout" in
  let fd_stdout = Unix.openfile filename_output_stdout [Unix.O_RDWR] 0600 in
  let filename_output_stderr = Filename.temp_file "rill-run-test-" "-pipe-stderr" in
  let fd_stderr = Unix.openfile filename_output_stderr [Unix.O_RDWR] 0600 in
  let args = Array.of_list ([
                             executable_filename
                           ])
  in
  let executable_path = Filename.concat (Unix.getcwd()) executable_filename in
  let stat = run_executable executable_path args Unix.stdin fd_stdout fd_stderr in
  Unix.close fd_stdout;
  Unix.close fd_stderr;

  Printf.printf "RUN    : %s\n" (string_of_stat stat);

  if is_success stat then
    begin
      stat
    end
  else
    begin
      Printf.printf "stdout\n";
      output_file filename_output_stdout;
      Printf.printf "stderr\n";
      output_file filename_output_stderr;

      stat
    end

let run_compilable_tests base_dir files ctx =
  let sep_1 = String.make 80 '-' in
  let sep_2 = String.make 80 '=' in
  let total_num = List.length files in
  let run i filename =
    Printf.printf "\n";
    Printf.printf "%s\n" sep_1;
    Printf.printf "- (%03d/%03d) compilable test / %s\n" (i+1) total_num filename;
    Printf.printf "%s\n" sep_1;
    Printf.printf "\n";
    flush_all ();

    let casename = Printf.sprintf "%03d-%s.out" i (Filename.chop_extension filename) in
    let file_fullpath = Filename.concat base_dir filename in

    let filename_output = Filename.temp_file "rill-run-test-" "-pipe" in
    let fd = Unix.openfile filename_output [Unix.O_RDWR] 0600 in
    let args = Array.of_list ([
                               ctx.compiler_bin;
                               file_fullpath;
                               "-o"; casename;
                             ] @ ctx.compiler_options)
    in
    let stat = run_executable ctx.compiler_bin args Unix.stdin fd fd in
    Unix.close fd;

    if is_failure stat then
      output_file filename_output;

    Printf.printf "COMPILE: %s\n" (string_of_stat stat);
    flush_all ();

    if is_success stat then
      run_executable_test casename ctx
    else
      stat
  in
  let stats = List.mapi run files in

  Printf.printf "\n";
  Printf.printf "%s\n" sep_2;
  Printf.printf "= compilable report\n";
  Printf.printf "%s\n" sep_2;
  Printf.printf "\n";
  show_reports files stats;
  List.exists is_failure stats


let collect_filenames target_dir ext =
  let all_filenames = Sys.readdir target_dir in
  all_filenames
  |> Array.to_list
  |> List.filter (fun s -> Str.string_match ext s 0)


let () =
  let rill_bin = "../rillc/src/rillc" in

  let ctx = {
    compiler_bin = rill_bin;
    compiler_options = ["--system-lib"; "../corelib/src";
                        "--system-lib"; "../stdlib/src";
                        "-L"; "../stdlib/lib";
                        "-l"; "rillstd-rt";
                        "-L"; "../corelib/lib";
                        "-l"; "rillcore-rt";
                        "--no-corelib"; (* because link libs by myself *)
                        "--no-stdlib";  (* because link libs by myself *)
                       ]
  } in

  let test_dir = Sys.getcwd () in
  let target_dir = Filename.concat test_dir "compilable" in

  let extension = Str.regexp "^\\(.*\\)\\.rill$" in

  let filenames = collect_filenames target_dir extension in
  let is_failed = run_compilable_tests target_dir filenames ctx in
  exit (if is_failed then 1 else 0)
