open Core
open Async
open Build_pkg_common.Std

module S = Public_release_helpers.Std.Shell_helpers

let write_with_timestamp writer line =
  let time_str = Time.to_string_abs ~zone:(force Time.Zone.local) (Time.now ()) in
  Writer.write writer (time_str ^ " " ^ line ^ "\n")

let log_filename ~build_dir = Common.log_dir ~build_dir ^/ "all.log"

type run_t =
  ?f:(string -> unit) ->
  ?working_dir:string ->
  ?pkg_name:Package_name.t ->
  build_dir:string ->
  string ->
  string list ->
  unit Deferred.t

type log_t =
  ?pkg_name:Package_name.t ->
  build_dir:string ->
  string ->
  unit Deferred.t

type t =
  { queue : string Queue.t;
    run : run_t;
    log : log_t }

let log_last_lines_count = 20

let create () =
  let queue = Queue.create ~capacity:log_last_lines_count () in
  let bounded_enqueue queue x =
    if Queue.length queue = log_last_lines_count then
      ignore (Queue.dequeue queue);
    Queue.enqueue queue x
  in
  let output_to_logs ?pkg_name ~build_dir ~f =
    let write writers =
      let output_line line =
        List.iter writers ~f:(fun writer -> write_with_timestamp writer line);
        bounded_enqueue queue line;
        Deferred.unit
      in
      f output_line
    in
    let filenames =
      log_filename ~build_dir ::
      (Option.map pkg_name ~f:(Common.log_filename_for ~build_dir)
       |> Option.to_list)
    in
    (* This non-obvious fold creates a stack of [Writer.with_file]'s like this:
       Writer.with_file filename1 ~f:(fun writer1 ->
       Writer.with_file filename2 ~f:(fun writer2 ->
       ...
       write [writer1; writer2; ...]
       ...))
    *)
    let write_to_all =
      List.fold filenames ~init:(fun writers -> write writers) ~f:(fun write filename ->
        fun writers ->
          Writer.with_file ~append:true filename ~f:(fun writer ->
            write (writer :: writers)))
    in
    write_to_all []
  in
  let run ?(f=ignore) ?working_dir ?pkg_name ~build_dir prog args =
    output_to_logs ?pkg_name ~build_dir ~f:(fun output_line ->
      let cmd = List.map (prog :: args) ~f:Filename.quote |> String.concat ~sep:" " in
      let info =
        match working_dir with
        | None -> sprintf "running: %s" cmd
        | Some wd -> sprintf "running: %s in %s" cmd wd
      in
      let%bind () = output_line info in
      let%bind () =
        S.run_iter_stdout_and_stderr prog args ?working_dir ~f:(fun line ->
          f line; output_line line)
      in
      ksprintf output_line "finished %s" info)
  in
  let log ?pkg_name ~build_dir line =
    output_to_logs ~build_dir ?pkg_name ~f:(fun output_line -> output_line line)
  in
  { queue; run; log }
