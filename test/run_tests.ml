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

type message =
  | MsgOutput of string * string
  | MsgExpect of string * string

type executed_status =
    Success
  | Failure of int * string * message list

let parallel_num = 10
let mutex = Mutex.create ()

let line_width = 100

let is_success x = match x with
  | Success -> true
  | Failure _ -> false

let is_failure x = match x with
  | Success -> false
  | Failure _ -> true

let string_of_stat x = match x with
  | Success -> "SUCCESS"
  | Failure (code, reason, _) -> Printf.sprintf "FAILURE (%d, %s)" code reason


let show_reports files stats =
  let total_num = stats |> List.length in
  let succ_num = stats |> List.filter is_success |> List.length in
  let fail_num = stats |> List.filter is_failure |> List.length in
  let unk_num = total_num - succ_num - fail_num in

  let fn_max_width = List.fold_left (fun w s -> max w (String.length s)) 0 files in

  let show i (file, stat) =
    let pretty_filename = file ^ String.make (fn_max_width - String.length file) ' ' in
    Printf.printf " %03d / %s - %s\n" (i+1) pretty_filename (string_of_stat stat)
  in
  List.iteri show (List.combine files stats);

  Printf.printf "SUCCESS (%03d/%03d) : FAILURE (%03d/%03d) : UNKNOWN (%03d/%03d)\n"
                succ_num total_num fail_num total_num unk_num total_num;

  (**)
  let sep_1 = String.make line_width '=' in
  let sep_2 = String.make line_width '-' in
  let show_errors file stat =
    match stat with
    | Failure (code, reason, msgs) ->
       Printf.printf "%s\n\n" sep_1;
       let pretty_filename = file ^ String.make (fn_max_width - String.length file) ' ' in
       Printf.printf ":Log: %s - %s\n\n" pretty_filename (string_of_stat stat);
       let show_msg msg = match msg with
         | MsgOutput (title, content) ->
            Printf.printf "<%s>\n%s\n" title sep_2;
            Printf.printf "%s\n" content;
            Printf.printf "%s\n\n" sep_2;
         | _ -> ()
       in
       List.iter show_msg msgs;
       Printf.printf "\n%s\n" sep_1;
    | _ -> ()
  in
  List.iter2 show_errors files stats;

  ()

let run_executable bin_path args env stdin stdout stderr =
  let pid =
    Unix.create_process_env bin_path args env stdin stdout stderr
  in
  let (rpid, ps) = Unix.waitpid [] pid in
  let stat = match ps with
    | Unix.WEXITED 0 -> Success
    | Unix.WEXITED code -> Failure (code, "return code", [])
    | Unix.WSIGNALED s -> Failure (s, "signaled", [])
    | _ -> Failure (0, "unexpected", [])
  in
  stat

let read_file filename =
  let fd = Unix.openfile filename [Unix.O_RDONLY] 0400 in
  let ch = Unix.in_channel_of_descr fd in
  let buf = Buffer.create 1024 in
  let _ = try while true do
                let s = IO.nread ch 1024 in
                Buffer.add_string buf s
              done with
          | IO.No_more_input -> ()
  in
  Unix.close fd;
  Buffer.contents buf

let run_executable_test executable_filename ctx =
  let filename_output_stdout = Filename.temp_file "rill-run-test-" "-pipe-stdout" in
  let fd_stdout = Unix.openfile filename_output_stdout [Unix.O_RDWR] 0600 in
  let filename_output_stderr = Filename.temp_file "rill-run-test-" "-pipe-stderr" in
  let fd_stderr = Unix.openfile filename_output_stderr [Unix.O_RDWR] 0600 in
  let args = [|executable_filename|] in
  let env = Unix.environment () in
  let executable_path = Filename.concat (Unix.getcwd()) executable_filename in
  let stat = run_executable executable_path args env Unix.stdin fd_stdout fd_stderr in
  Unix.close fd_stdout;
  Unix.close fd_stderr;

  (*m_printf "RUN    : %s\n" (string_of_stat stat);*)

  match stat with
  | Success ->
     (* TODO: check outputs *)
     stat

  | Failure (code, reason, msgs) ->
     let s_stdout = read_file filename_output_stdout in
     let m_stdout = MsgOutput ("stdout", s_stdout) in
     let s_stderr = read_file filename_output_stderr in
     let m_stderr = MsgOutput ("stderr", s_stderr) in
     Failure (code, reason, m_stdout :: m_stderr :: msgs)

let run_compilable_tests base_dir files ctx =
  let sep_1 = String.make line_width '-' in
  let sep_2 = String.make line_width '=' in
  let total_num = List.length files in

  let run offset i ch filename =
    let index = offset + i in

    let casename = Printf.sprintf "%03d-%s" (index+1) (Filename.chop_extension filename) in
    let executable_name = Printf.sprintf "%s.out" casename in
    let file_fullpath = Filename.concat base_dir filename in

    let filename_output = Filename.temp_file "rill-run-test-" "-pipe" in
    let fd = Unix.openfile filename_output [Unix.O_RDWR] 0600 in
    let args = Array.of_list ([
                               ctx.compiler_bin;
                               file_fullpath;
                               "-o"; executable_name;
                             ] @ ctx.compiler_options)
    in
    let env =
      Array.concat [
          Unix.environment ();
          [|
            Printf.sprintf "BISECT_FILE=bisect-%s" casename
           |]
        ]
    in
    let stat = run_executable ctx.compiler_bin args env Unix.stdin fd fd in
    Unix.close fd;

    let stat = match stat with
      | Success -> stat
      | Failure (code, reason, msgs) ->
         let out = read_file filename_output in
         let m_out = MsgOutput ("output", out) in
         Failure (code, reason, m_out :: msgs)
    in

    let stat =
      if is_success stat then
        run_executable_test executable_name ctx
      else
        stat
    in
    Event.sync (Event.send ch stat);

    Mutex.lock mutex;
    Printf.printf "\n";
    Printf.printf "%s\n" sep_1;
    Printf.printf "- (%03d/%03d) compilable and executable test / %s\n"
                  (index+1) total_num filename;
    Printf.printf "%s\n" sep_1;
    Printf.printf "\n";
    Printf.printf "RESULT: %s\n" (string_of_stat stat);
    Mutex.unlock mutex;
  in

  let splitted =
    let rec split total_files acc = match total_files with
      | [] -> acc
      | _ ->
         let files = List.take parallel_num total_files in
         split (List.drop parallel_num total_files) (files :: acc)
    in
    split files [] |> List.rev
  in

  let run_parallel i file_block =
    let offset = List.length file_block * i in
    let spawn i file =
      let ch = Event.new_channel () in
      let t = Thread.create (fun ch -> run offset i ch file) ch in
      (t, ch)
    in

    let tx = List.mapi spawn file_block in
    let (ts, cs) = List.split tx in

    let results = cs |> List.map Event.receive |> List.map Event.sync in
    List.iter Thread.join ts;

    results
  in
  let stats = List.mapi run_parallel splitted |> List.flatten in

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
