open Core.Std
open Async.Std
open Build_pkg_common.Std
open Common

module S = Public_release_helpers.Std.Shell_helpers

let verify_tarball ~tarball_path ~pkg_name =
  (* Verify that the directory contains a top-level directory whose name is pkg_name. *)
  let%map lines = S.run_lines "tar" ["tf"; tarball_path] in
  let top_level =
    List.filter lines ~f:(fun line ->
      (* no of '/'s *)
      let slashes = String.filter line ~f:((=) '/') |> String.length in
      slashes = 0 || (slashes = 1 && String.is_suffix line ~suffix:"/"))
  in
  let expected_top_level = Package_name.to_string pkg_name ^ "/" in
  match top_level with
  | [ top_level ] ->
    if top_level = expected_top_level
    then Ok ()
    else Or_error.errorf
           "Invalid top_level (%s) expected (%s)"
           top_level
           expected_top_level
  | _ -> Or_error.errorf
           "Expected one (instead of %d) top level %s"
           (List.length top_level)
           expected_top_level

(* Extract the tarball, removing everything but the build artifacts. *)
let extract_tarball ~pkg_name ~tarball_path ~build_dir =
  (* We only use it as a string here. *)
  let pkg_name = Package_name.to_string pkg_name in
  let%bind () =
    match%bind Sys.is_directory (build_dir ^/ pkg_name) with
    | `Yes ->
      S.run_zero ~working_dir:build_dir "find"
        [pkg_name; "-mindepth"; "1";
         "-not"; "-regex"; pkg_name ^ "/_build/.*"; "-a";
         "-not"; "-regex"; pkg_name ^ "/_build$";
         "-delete"]
    | `No | `Unknown -> Deferred.unit
  in
  S.run_zero "tar" ["xf"; tarball_path; "-C"; build_dir]

let do_install ~base_dir ~build_dir ~pkg_name ~(run : Logger.run_t) ~prefix ~log () =
  let install_file = install_file_for ~base_dir pkg_name in
  let working_dir = Common.build_dir ~base_dir ^/ Package_name.to_string pkg_name in
  let run = run ~build_dir ~pkg_name ~working_dir in

  (* Uninstall currently installed package *)
  let%bind () =
    match%bind is_file install_file with
    | true -> run "opam-installer" ["-u"; "--prefix"; prefix; install_file]
    | false -> Deferred.unit
  in
  let%bind () =
    let lib_dir = prefix ^/ "lib" ^/ Package_name.to_string pkg_name in
    match%bind Sys.is_directory lib_dir with
    | `Yes ->
      (* It's a warning, really. *)
      let%bind () = ksprintf log "manually removing leftovers: %s" lib_dir in
      run "rm" ["-rf"; lib_dir]
    | `No | `Unknown ->
      Deferred.unit
  in

  (* Compile and install *)
  let%bind () = run "make" [] in
  let%bind () = run "make" ["install"; "PREFIX=" ^ prefix] in
  (* Compute the checksum *)
  let%bind () = run "rm" ["-rf"; "_install"] in
  let%bind () = run "mkdir" ["_install"] in
  let%bind () = run "make" ["install"; "PREFIX=_install"] in
  let%bind () =
    match%bind Lazy.force Common.os with
    | Darwin ->
      run "sh" ["-c"; "md5 `find _install -type f` > .checksums; \
                       md5 -q .checksums > .checksum"]
    | Linux | Other _ ->
      run "sh" ["-c"; "md5sum `find _install -type f` > .checksums; \
                       md5sum .checksums > .checksum"]
  in
  let%bind () = run "rm" ["-rf"; "_install"] in
  let%bind checksum_str = Reader.file_contents (working_dir ^/ ".checksum") in
  let checksum = String.prefix checksum_str 32 |> Package_checksum.of_string in
  let%bind () = run "cp" [Package_name.to_string pkg_name ^ ".install"; install_file] in
  return checksum

let build ~config ~(pkg_name : Package_name.t) ~tarball_path
      ~(metadata : Package_metadata.t) =
  ignore metadata;
  (* Declaring it all the way up here so that we avoid [return (error, [])] constructs
     throughout the code. *)
  let { Logger.queue; run; log } = Logger.create () in
  begin
    let%bind () =
      setup_environmental_variables
        ~opam_root:(opam_root_config config)
        ~opam_switch:(Config.opam_switch config)
    in
    (* Verify that the directory contains a top-level directory whose name is pkg_name *)
    verify_tarball ~tarball_path ~pkg_name
    >>=? fun () ->
    (* Handful of handy aliases. *)
    let build_dir = build_dir_config config in
    let base_dir = Config.base_dir config in

    let%bind () = rotate_old_log_for ~build_dir pkg_name in
    let log = log ~build_dir ~pkg_name in

    let%bind () = extract_tarball ~build_dir ~pkg_name ~tarball_path in
    (* Always the absolute path to the opam prefix inside the opam root. *)
    let%bind prefix = S.run_one "opam" ["config"; "var"; "prefix"] in

    let%bind result =
      Monitor.try_with_or_error (do_install ~base_dir ~build_dir ~pkg_name ~run ~prefix ~log)
    in
    let%map () =
      match result with
      | Ok _ ->
        ksprintf log !"delegated build %{Package_name} completed successfully" pkg_name
      | Error err ->
        ksprintf log !"delegated build %{Package_name} failed with: %{Error#hum}"
          pkg_name err
    in
    result
  end
  >>| Result.map_error ~f:(fun error -> error, Queue.to_list queue)
