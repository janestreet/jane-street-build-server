open Core
open Async
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
         "-delete"]
    | `No | `Unknown -> Deferred.unit
  in
  S.run_zero "tar" ["xf"; tarball_path; "-C"; build_dir]

let add_line ~opam_file:file pkg_name new_line =
  let lines = In_channel.with_file file ~f:In_channel.input_lines in
  let new_lines =
    let rec insert_sorted = function
      | [] -> [ new_line ]
      | line :: lines as lst->
        let pkg, _ = String.lsplit2_exn line ~on:' ' in
        if pkg_name < pkg then
          new_line :: lst
        else
          line :: insert_sorted lines
    in
    insert_sorted lines
  in
  Writer.with_file_atomic file ~f:(fun writer ->
    List.iter new_lines ~f:(Writer.write_line writer);
    Writer.flushed writer
  )

let opam_fake_pin ~opam_root ~working_dir ~prefix run pkg_name =
  let pkg_name = Package_name.to_string pkg_name in
  let%bind version =
    let default = opam_root ^/ "repo/default/packages" ^/ pkg_name in
    match%bind Sys.file_exists default with
    | `Yes ->
      let%bind versions = Sys.ls_dir default in
      begin match
        List.fold_left versions ~init:None ~f:(fun acc dir ->
          let _pkg, vstring' = String.lsplit2_exn dir ~on:'.' in
          match String.lsplit2_exn vstring' ~on:'v' with
          | exception _ -> acc
          | _, str_vers ->
            let version' = List.map ~f:Int.of_string (String.split str_vers ~on:'.') in
            assert (List.length version' = 3);
            match acc with
            | Some (_, version) when List.compare Int.compare version version' >= 0 ->
              acc
            | _ ->
              Some (vstring', version')
        )
      with
      | None -> (* no vX.YY version?! *) return "fake"
      | Some (version, _) ->
        return version
      end
      (*
          List.filter_map versions ~f:(fun )
      let versions = List.sort ~cmp:(fun x y -> String.compare y x) versions in
      let versions = List.sort ~compare:(fun x y -> String.compare y x) versions in
      let _, version = String.lsplit2_exn (List.hd_exn versions) ~on:'.' in
      return version
         *)
    | _ ->
      (* if the package doesn't exist in the main repo, then nothing depends on it
         and it doesn't actually matter if we give a proper vnum or not. *)
      return "fake"
  in
  let dev_pkgs = prefix ^/ "packages.dev" in
  let%bind () = Unix.mkdir ~p:() dev_pkgs in
  let%map () = run "cp" ["-r"; working_dir; dev_pkgs ^/ pkg_name ]
  and () =
    add_line ~opam_file:(prefix ^/ "pinned") pkg_name
      (sprintf "%s path %s" pkg_name working_dir)
  in
  version

let update_opam_installed_list prefix pkg_name version =
  let pkg_name = Package_name.to_string pkg_name in
  let line = sprintf "%s %s" pkg_name version in
  let%bind () = add_line ~opam_file:(prefix ^/ "installed") pkg_name line
  and () = add_line ~opam_file:(prefix ^/ "installed.roots") pkg_name line in
  return ()

let do_install ~base_dir ~build_dir ~pkg_name ~(run : Logger.run_t) ~prefix ~log () =
  let install_file = install_file_for ~base_dir pkg_name in
  let working_dir = Common.build_dir ~base_dir ^/ Package_name.to_string pkg_name in
  let run = run ~build_dir ~pkg_name ~working_dir in

  (* Uninstall currently installed package *)
  let%bind () =
    match%bind is_file install_file with
    | true ->
      let%bind () = run "opam-installer" ["-u"; "--prefix"; prefix; install_file] in
      run "rm" ["-f"; install_file]
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
  let%bind () =
    (* Tell opam to forget about that package, it's gone. *)
    run "sh" ["-c"; sprintf "grep -v \"^%s \" %s/installed > .installed; \
                             mv .installed %s/installed"
                      (Package_name.to_string pkg_name) prefix prefix ]
  in
  let%bind () =
    (* Tell opam to forget about that package, it's gone. *)
    run "sh" ["-c"; sprintf "grep -v \"^%s \" %s/installed.roots > .installed; \
                             mv .installed %s/installed.roots"
                      (Package_name.to_string pkg_name) prefix prefix ]
  in
  let%bind () =
    (* Tell opam to forget about that package, it's gone. *)
    run "sh" ["-c"; sprintf "grep -v \"^%s \" %s/pinned > .pinned; \
                             mv .pinned %s/pinned"
                      (Package_name.to_string pkg_name) prefix prefix ]
  in
  (* Compile and install *)
  let%bind opam_version =
    opam_fake_pin ~opam_root:(opam_root ~base_dir) ~working_dir ~prefix run pkg_name
  in
  let%bind () = run "make" [] in
  let%bind () = run "make" ["install"; "PREFIX=" ^ prefix] in
  (* Pretend to opam that we've installed (also, remove opam state cache) *)
  let%bind () = update_opam_installed_list prefix pkg_name opam_version in
  let%bind () =
    let opam_root = opam_root ~base_dir in
    let cache = opam_root ^/ "state.cache" in
    match%bind Sys.file_exists cache with
    | `Yes -> Sys.remove cache
    | _ -> return ()
  in
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
