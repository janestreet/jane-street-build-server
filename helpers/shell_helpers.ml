open Core
open Async.Std

let echo = ref false

let echo_flag =
  Command.Param.(map (flag "-echo"  no_arg ~doc:" Print shell commands")
                   ~f:(fun x -> echo := x))
;;

let print_cmd_line ?working_dir cmd_line =
  match working_dir with
  | None ->
    eprintf "running: %s\n" cmd_line
  | Some d ->
    eprintf "running: (cd %s && %s)\n" (Filename.quote d) cmd_line
;;

let show_command ?working_dir prog args =
  if !echo then
    print_cmd_line ?working_dir
      (List.map (prog :: args) ~f:Filename.quote
       |> String.concat ~sep:" ")
;;

let show_shell_command ?working_dir cmd_line =
  if !echo then
    print_cmd_line ?working_dir cmd_line
;;

let run ?accept_nonzero_exit ?working_dir prog args =
  show_command ?working_dir prog args;
  Process.run ?accept_nonzero_exit ?working_dir ~prog ~args ()
  >>| ok_exn
;;

let run_ignore ?accept_nonzero_exit ?working_dir prog args =
  show_command ?working_dir prog args;
  Process.run ?accept_nonzero_exit ?working_dir ~prog ~args ()
  >>| fun r -> ignore (ok_exn r : string)
;;

let run_lines ?accept_nonzero_exit ?working_dir prog args =
  show_command ?working_dir prog args;
  Process.run_lines ?accept_nonzero_exit ?working_dir ~prog ~args ()
  >>| ok_exn
;;

let run_one ?accept_nonzero_exit ?working_dir prog args =
  run_lines ?accept_nonzero_exit ?working_dir prog args
  >>| function
  | [line] -> line
  | lines ->
    raise_s
      [%sexp "command didn't output exactly one line on stdout",
             { working_dir = (working_dir : string sexp_option)
             ; prog        = (prog        : string)
             ; args        = (args        : string list)
             ; stdout      = (lines       : string list)
             }
      ]

let run_zero ?accept_nonzero_exit ?working_dir prog args =
  show_command ?working_dir prog args;
  Process.run_expect_no_output ?accept_nonzero_exit ?working_dir ~prog ~args ()
  >>| ok_exn
;;

let run_iter_stdout_and_stderr ?working_dir ~f prog args =
  show_command ?working_dir prog args;
  Process.create ?working_dir ~prog ~args ()
  >>= fun process ->
  let process = ok_exn process in
  let iter reader = Reader.lines reader |> Pipe.iter ~f in
  let out = iter (Process.stdout process) in
  let err = iter (Process.stderr process) in
  Process.wait process
  >>= fun res ->
  Deferred.all_unit [out; err]
  >>| fun () ->
  match res with
  | Ok () -> ()
  | Error exit_status ->
    raise_s
      [%sexp "process failed",
             { prog        = (prog        : string)
             ; args        = (args        : string list)
             ; working_dir = (working_dir : string sexp_option)
             ; exit_status = (exit_status : Unix.Exit_or_signal.error)
             }]
;;

let sh_zero ?accept_nonzero_exit ?working_dir fmt =
  ksprintf (fun s ->
    show_shell_command ?working_dir s;
    Process.run_expect_no_output ?accept_nonzero_exit ?working_dir
      ~prog:"/bin/sh" ~args:["-c"; s] ()
    >>| ok_exn)
    fmt
;;

let sh_ignore ?accept_nonzero_exit ?working_dir fmt =
  ksprintf (fun s ->
    show_shell_command ?working_dir s;
    Process.run ?accept_nonzero_exit ?working_dir
      ~prog:"/bin/sh" ~args:["-c"; s] ()
    >>| fun r -> ignore (ok_exn r : string))
    fmt
;;

let sh_lines ?accept_nonzero_exit ?working_dir fmt =
  ksprintf (fun s ->
    show_shell_command ?working_dir s;
    Process.run_lines ?accept_nonzero_exit ?working_dir
      ~prog:"/bin/sh" ~args:["-c"; s] ()
    >>| ok_exn)
    fmt
;;

let test ~true_v ~false_v ?working_dir prog args =
  show_command ?working_dir prog args;
  Process.create ?working_dir ~prog ~args ()
  >>= fun r ->
  Process.collect_output_and_wait (ok_exn r)
  >>| fun output ->
  match output.exit_status with
  | Ok () when true_v  = 0 -> true
  | Ok () when false_v = 0 -> false
  | Error (`Exit_non_zero n) when n = true_v  -> true
  | Error (`Exit_non_zero n) when n = false_v -> false
  | _ ->
    raise_s
      [%sexp "process didn't finish as expected",
             { prog                = (prog : string)
             ; args                = (args : string list)
             ; working_dir         = (working_dir : string sexp_option)
             ; output              = (output : Process.Output.t)
             ; expected_exit_codes = ([true_v; false_v] : int list)
             }]
