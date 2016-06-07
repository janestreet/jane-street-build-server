open Core.Std
open Async.Std
open Build_pkg_common.Std
open Common

module S = Public_release_helpers.Std.Shell_helpers

(* Useful for debugging when there's many requests being started at the same time. *)
module Ansi = struct
  (* Extracted from our ansi_terminal copy to simplify things for now:

     - our version is incompatible with the one in opam
     - our version is already shipped in patdiff...
  *)

  type color =
    | Black | Red | Green | Yellow | Blue | Magenta | Cyan | White | Default
    | Bright_black | Bright_red | Bright_green | Bright_yellow | Bright_blue
    | Bright_magenta | Bright_cyan | Bright_white
  [@@deriving variants]

  type style =
    | Reset | Bold | Underlined | Dim | Blink | Inverse | Hidden
    | Bold_off | Underlined_off | Dim_off | Blink_off | Inverse_off | Hidden_off
    | Foreground of color
    | Background of color
  [@@deriving variants]

  let ansi_code_of_style = function
    | Reset                     -> "0"
    | Bold                      -> "1"
    | Bold_off                  -> "22"
    | Dim                       -> "2"
    | Dim_off                   -> "22"
    | Underlined                -> "4"
    | Underlined_off            -> "24"
    | Blink                     -> "5"
    | Blink_off                 -> "25"
    | Inverse                   -> "7"
    | Inverse_off               -> "27"
    | Hidden                    -> "8"
    | Hidden_off                -> "28"
    | Foreground Black          -> "30"
    | Foreground Red            -> "31"
    | Foreground Green          -> "32"
    | Foreground Yellow         -> "33"
    | Foreground Blue           -> "34"
    | Foreground Magenta        -> "35"
    | Foreground Cyan           -> "36"
    | Foreground White          -> "37"
    | Foreground Default        -> "39"
    | Foreground Bright_black   -> "90"
    | Foreground Bright_red     -> "91"
    | Foreground Bright_green   -> "92"
    | Foreground Bright_yellow  -> "93"
    | Foreground Bright_blue    -> "94"
    | Foreground Bright_magenta -> "95"
    | Foreground Bright_cyan    -> "96"
    | Foreground Bright_white   -> "97"
    | Background Black          -> "40"
    | Background Red            -> "41"
    | Background Green          -> "42"
    | Background Yellow         -> "43"
    | Background Blue           -> "44"
    | Background Magenta        -> "45"
    | Background Cyan           -> "46"
    | Background White          -> "47"
    | Background Default        -> "49"
    | Background Bright_black   -> "100"
    | Background Bright_red     -> "101"
    | Background Bright_green   -> "102"
    | Background Bright_yellow  -> "103"
    | Background Bright_blue    -> "104"
    | Background Bright_magenta -> "105"
    | Background Bright_cyan    -> "106"
    | Background Bright_white   -> "107"

  let ansi_escape_of_styles styles =
    sprintf "\027[%sm"
      (List.map styles ~f:ansi_code_of_style
       |> String.concat ~sep:";")

  let apply_string styles str =
    sprintf "%s%s%s" (ansi_escape_of_styles styles) str (ansi_escape_of_styles [Reset])
end
let colorize =
  let color_combos = [|
    Ansi.Blue, Ansi.Bright_green;
    Ansi.Red, Ansi.Bright_yellow;
    Ansi.Yellow, Ansi.Blue;
    Ansi.Magenta, Ansi.Bright_cyan;
    Ansi.Bright_green, Ansi.Blue;
    Ansi.Bright_yellow, Ansi.Red;
    Ansi.Blue, Ansi.Yellow;
    Ansi.Bright_cyan, Ansi.Magenta;
  |]
  in
  fun package_name ->
    let str = Package_name.to_string package_name in
    let hash = String.hash str in
    let fore, back = color_combos.(hash mod (Array.length color_combos)) in
    Ansi.apply_string [Ansi.Foreground fore; Ansi.Background back] str

let debug = Log.Global.info
let packages_to_str pkgs = List.map pkgs ~f:Package_name.to_string |> String.concat ~sep:" "
let _ = packages_to_str

let config : Config.t Set_once.t = Set_once.create ()

(* For a given package (identified by its name), we store the build process, the result
   of Process.wait on this process and the parsed response. *)
module Build_info = struct
  type t =
    { process : Process.t;
      (* The result of wait. We can't call Process.wait every time we want to access the
         exit code, since it corresponds to a waitpid call. *)
      wait : Unix.Exit_or_signal.t Deferred.t;
      (* Processed output of the build. *)
      response : Rpcs.Build_v2.response Deferred.t }
end

(* This table contains the status of the last build for each build requested since the
   start of the server *)
let builds : Build_info.t Deferred.Or_error.t Package_name.Table.t =
  Package_name.Table.create ()

(* Packages which depend on a given package "register" in this table, so that when
   a package is rebuilt, its dependencies could be killed. *)
let dependents : Package_name.Set.t Package_name.Table.t = Package_name.Table.create ()

(* Since we don't remove dependents from [dependents] when they finish building, when a
   package that used to depend on another package no longer depends on it, there would be
   an outdated (i.e. incorrect) dependency in [dependents]. To remedy that, we keep track
   of the dependencies of each package. *)
let dependencies : Package_name.Set.t Package_name.Table.t = Package_name.Table.create ()


module External_packages = struct
  (* Installed external dependencies are stored in a set, as trying to install already
     installed packages takes too much time (~0.5 s). *)
  let installed = Package_name.Hash_set.create ()

  type request =
    { package  : Package_name.t
    ; finished : unit Ivar.t
    } [@@deriving fields]

  let do_install_now ~run requests =
    let to_install_set = Package_name.Set.of_list (List.map requests ~f:package) in
    (* Special hack for zarith, see
       https://forge.ocamlcore.org/tracker/index.php?func=detail&aid=1539&group_id=243&atid=1095
    *)
    let%bind to_install_set =
      let zarith = Package_name.of_string "zarith" in
      if not (Set.mem to_install_set zarith) then
        return to_install_set
      else
        let is_32bit =
          String.is_suffix ~suffix:"32bit"
            (Set_once.get_exn config |> Config.opam_switch |> Opam_switch.to_string)
        in
        if not is_32bit then
          return to_install_set
        else
          let%map () =
            run "i386" ["env"; "CC=gcc -m32"; "opam"; "install"; "zarith"; "-y"]
          in
          Set.remove to_install_set zarith
    in
    (* Make sure we're not running it without packages. *)
    let%map () =
      if Set.is_empty to_install_set then
        Deferred.unit
      else
        let packages = Set.to_list to_install_set |> List.map ~f:Package_name.to_string in
        run "opam" (["install"; "-y"] @ packages)
    in
    List.iter requests ~f:(fun r -> Ivar.fill r.finished ())

  let rec install_loop ~run package_stream =
    match%bind Stream.next package_stream with
    | Nil -> assert false
    | Cons (request, rest) ->
      let%bind () = after (sec 1.) in
      let requests, rest = Stream.available_now rest in
      let%bind () = do_install_now ~run (request :: requests) in
      install_loop ~run rest

  (* Stream of requests *)
  let tail = Tail.create ()

  (* Guard against running the install loop multiple times. *)
  let install_loop_guard = Set_once.create ()

  let start_install_loop ~run =
    match Set_once.get install_loop_guard with
    | None -> Set_once.set_exn install_loop_guard (install_loop ~run (Tail.collect tail))
    | Some _ -> Log.Global.info "install loop already running"

  let install package =
    Deferred.create (fun ivar ->
      Tail.extend tail { package; finished = ivar })

  let install_deps ~deps =
    let deps = List.filter deps ~f:(fun pkg -> not (Hash_set.mem installed pkg)) in
    Deferred.List.iter deps ~how:`Parallel ~f:install
end

let aspcud_url = "http://cudf-solvers.irill.org/cudf_remote_proxy"

let setup ~base_dir ~opam_switch =
  Log.Global.info
    "serving rpc setup (opam switch: %s) (base_dir: %s)"
    (Opam_switch.to_string opam_switch)
    base_dir;
  let opam_root = opam_root ~base_dir in
  let switch_str = Opam_switch.to_string opam_switch in
  Unix.putenv ~key:"OPAMROOT" ~data:opam_root;
  let { Logger.queue; run; log = _ } = Logger.create () in
  let build_dir = build_dir ~base_dir in
  let run = run ~build_dir in
  let setup_status =
    Monitor.try_with (fun () ->
      (* Create the directory and get its absolute path - needed for [opam init] *)
      let%bind base_dir =
        if Filename.is_absolute base_dir
        then return base_dir
        else Sys.getcwd () >>| (^/) base_dir
      in
      let%bind () = S.run_zero "mkdir" ["-p"; build_dir] in
      let%bind () = S.run_zero "mkdir" ["-p"; log_dir ~build_dir] in
      let%bind () = S.run_zero "mkdir" ["-p"; install_dir ~base_dir] in
      let%bind () =
        let current_switch =
          match Set_once.get config with
          | None -> Monitor.try_with (fun () ->
            S.run_one "opam" ["config"; "var"; "switch"]
            >>| Opam_switch.of_string)
          | Some config -> return (Ok (Config.opam_switch config))
        in
        match%bind current_switch with
        | Ok current_switch ->
          if current_switch <> opam_switch then
            failwithf !"server already configured with opam switch %{Opam_switch}"
              current_switch ()
          else
            Deferred.unit
        | Error _ -> run "opam" ["init"; "--no-setup"; "--compiler"; switch_str]
      in

      (* Set up aspcud. *)
      let bin_dir = bin_dir ~base_dir in
      let aspcud = bin_dir ^/ "aspcud" in
      let%bind () = S.run_zero "mkdir" ["-p"; bin_dir] in
      let%bind () = S.run_zero "wget" ["-q"; aspcud_url; "-O"; aspcud] in
      let%bind () = S.run_zero "chmod" ["+x"; aspcud] in
      Unix.putenv ~key:"PATH" ~data:(bin_dir ^ ":" ^ (Unix.getenv_exn "PATH"));

      let%bind () = setup_environmental_variables ~opam_root ~opam_switch in
      let%map () = run "opam" ["install"; "-y"; "ocamlfind"; "oasis"] in
      Set_once.set_exn config (Config.create ~base_dir ~opam_switch);
      External_packages.start_install_loop ~run)
  in
  match%map setup_status with
  | Ok () as ok ->
    Log.Global.info "setup in %s, opam_switch %s completed successfully" base_dir switch_str;
    ok
  | Error exn ->
    Log.Global.error !"setup in %s, opam_switch %s failed with %{Exn}" base_dir switch_str exn;
    Error (Error.of_exn exn, Queue.to_list queue)

(* Processing of the result of the child process by the server, to send to the client *)
let process_build_result ~pkg_name proc wait =
  (* Like Process.collect_output_and_wait, but without the Process.wait. *)
  let stdout = Reader.contents (Process.stdout proc) in
  let stderr = Reader.contents (Process.stderr proc) in
  let%bind () = Writer.close (Process.stdin proc) in
  let%bind exit_status = wait in
  let%bind stdout = stdout in
  let%map stderr = stderr in
  let out_and_err () = sprintf "stdout:\n%s\nstderr:\n%s\n" stdout stderr in
  begin
    match exit_status with
    | Ok () ->
      begin
        try
          Ok (Sexp.of_string stdout |>
              [%of_sexp: (Package_checksum.t, Error.t * string list) Result.t])
        with _ ->
          Error (sprintf "Internal error: parsing sexp failed.\n%s" (out_and_err ()))
      end
    | Error `Exit_non_zero code ->
      Error (sprintf "Internal error: invalid build return code: %d\n%s"
               code (out_and_err ()))
    | Error `Signal signal ->
      Error (if signal = Signal.int
             then "Build interrupted"
             else sprintf "Error: program killed by signal %d\n%s"
                    (Signal.to_system_int signal) (out_and_err ()))
  end
  |> function
  | Ok result ->
    begin
      match result with
      | Ok _ ->
        Log.Global.info "build %s completed successfully" (colorize pkg_name);
      | Error (err, _) ->
        Log.Global.info !"build %s ended with a subcommand error: %{Error#hum}" (colorize pkg_name) err;
    end;
    result
  | Error err ->
    Log.Global.info "build %s ended with error: %s" (colorize pkg_name) err;
    Error (Error.of_string err, [])


let kill_process ~wait proc =
  if not (Deferred.is_determined wait)
  then match Signal.send Signal.int (`Pid (Process.pid proc)) with
    | `No_such_process -> Log.Global.error "no child to terminate"
    | `Ok -> ()
  else ()

let start_build ~base_dir ~pkg_name ~pkg_dependencies ~(metadata : Package_metadata.t)
      ~tarball_path ~tarball ~bin_path ~config ~connection_closed ~colorized =
  (* Update dependencies. *)
  let () =
    let pkg_dependencies_set = Package_name.Set.of_list pkg_dependencies in
    let removed_deps =
      let old_deps =
        Hashtbl.find dependencies pkg_name
        |> Option.value ~default:Package_name.Set.empty
      in
      let new_deps = pkg_dependencies_set in
      Set.diff old_deps new_deps
    in
    Set.iter removed_deps ~f:(fun removed_dep ->
      Hashtbl.update dependencies removed_dep ~f:(function
        | None -> assert false
        | Some current_deps -> Set.remove current_deps pkg_name));
    Hashtbl.set dependencies ~key:pkg_name ~data:pkg_dependencies_set
  in

  (* We need to add A to dependents of A's dependencies right now - if we waited,
     there'd be a race: assume A depends on B; A is waiting for B to finish and we
     restart the build for B - B's build is killed and we check for B's dependents
     (which is empty). B starts and then A starts (because the B build on which it was
     bound has finished). *)
  List.iter pkg_dependencies ~f:(fun dep ->
    Hashtbl.update dependents dep ~f:(function
      | None -> Package_name.Set.singleton pkg_name
      | Some current -> Set.add current pkg_name));

  (* Get all dependents (recursively) "kill" them (i.e. set statuses) and later
     actually kill them all. *)
  let rec collect_dependents acc name =
    match Hashtbl.find dependents name with
    | None -> acc
    | Some dep_on ->
      Hashtbl.remove dependents name;
      let not_yet_seen = Set.diff dep_on acc in
      Set.fold not_yet_seen ~init:(Set.union acc dep_on)
        ~f:collect_dependents
  in

  debug "%s: killing dependents..." colorized;
  let to_kill = collect_dependents Package_name.Set.empty pkg_name |> Set.to_list in
  (* Retrieve statuses of dependents to kill and clear them from the table immediately
     (by setting their status to an error) *)
  let statuses = List.filter_map to_kill ~f:(fun name ->
    let build_info_def = Hashtbl.find builds name in
    (match build_info_def with
     | None ->
       Log.Global.error !"no status for a killed dependency: %{Package_name}" name
     | Some _ ->
       let data =
         return (Error (ksprintf Error.of_string
                          !"Build interrupted by %{Package_name}" pkg_name))
       in
       Hashtbl.set builds ~key:name ~data);
    build_info_def)
  in

  (* Start installing external deps in the background *)
  debug "%s: installing external..." colorized;
  let external_deps_installation : unit Deferred.Or_error.t =
    let%map () = External_packages.install_deps ~deps:metadata.external_dependencies in
    debug "%s: external installed" colorized;
    Ok ()
  in

  (* Save the tarball and actually execute the build. *)
  let save_tarball_to_disk =
    Monitor.try_with_or_error (fun () -> Writer.save tarball_path ~contents:tarball)
  in

  (* First kill this one, as dependencies may be waiting for it to finish before they
     start. *)
  let%bind () =
    match Hashtbl.find builds pkg_name with
    | None -> return ()
    | Some build_info_def ->
      debug "%s: killing myself..." colorized;
      match%bind build_info_def with
      | Error _ -> return ()
      | Ok { Build_info.process; wait; response = _ } ->
        debug "%s: really killing myself..." colorized;
        kill_process process ~wait;
        let%bind _ = wait in
        return ()
  in
  (* Kill the rest. *)
  let%bind () =
    Deferred.List.iter ~how:`Parallel statuses ~f:(fun status ->
      match%bind status with
      | Error _ -> return ()
      | Ok { Build_info.process; wait; response = _ } ->
        kill_process process ~wait;
        let%bind _ = wait in
        return ())
  in

  (* Wait for the dependencies to finish. *)
  begin
    debug "%s: waiting for deps (%s)..."
      colorized
      (List.map pkg_dependencies ~f:colorize |> String.concat ~sep:" ");
    let%map deps_errors =
      Deferred.List.filter_map pkg_dependencies ~how:`Parallel ~f:(fun dep_name ->
        match Hashtbl.find builds dep_name with
        | None ->
          (* Check if install file is there. *)
          let%map installed =
            is_file (install_file_for ~base_dir dep_name)
          in
          Option.some_if (not installed) (
            sprintf
              !"Dependency not installed and not building: %{Package_name}" dep_name)
        | Some build_info_def ->
          match%bind build_info_def with
          | Error _ ->
            ksprintf Deferred.Option.return
              !"Dependency failed to start building: %{Package_name}"
              dep_name
          | Ok { Build_info.process = _; wait = _; response } ->
            match%map response with
            | Ok _ -> None
            | Error _ -> ksprintf Option.return
                           !"Dependency build error: %{Package_name}" dep_name)
    in
    debug "%s: deps installed" colorized;
    match deps_errors with
    | [] -> Ok ()
    | lst ->
      Or_error.errorf "Problem(s) installing dependencies:\n%s"
        (String.concat ~sep:"\n" lst)
  end
  >>=? fun () ->

  Deferred.Or_error.combine_errors_unit
    [ external_deps_installation; save_tarball_to_disk ]
  >>=? fun () ->

  debug "%s: building..." colorized;
  let proc_def =
    Process.create
      ~env:(`Extend [Common.subcommand_protection_var, Common.subcommand_protection_value])
      ~prog:bin_path
      ~args:[ "build";
              "-config"; sprintf !"%{sexp: Config.t}" config;
              "-pkg-name"; Package_name.to_string pkg_name;
              "-tarball-path"; tarball_path;
              "-metadata"; sprintf !"%{sexp: Package_metadata.t}" metadata ]
      ()
    >>|? fun proc ->
    let wait = Process.wait proc in
    { Build_info.process = proc;
      wait;
      response = process_build_result ~pkg_name proc wait }
  in
  (* Kill it when the connection is closed. *)
  don't_wait_for (
    (* This is not a memory leak as the client always exits after the request has been
       served. *)
    let%bind () = connection_closed in
    match%map proc_def with
    | Error _ -> ()
    | Ok { Build_info.process; wait; response = _ } -> kill_process ~wait process);
  proc_def

let delegate_build ~(metadata : Package_metadata.t) ~bin_path ~tarball ~connection_closed =
  match Set_once.get config with
  | None -> return (Error (Error.of_string "need to setup the server first", []))
  | Some config ->
    let base_dir = Config.base_dir config in
    let tarball_path =
      build_dir_config config ^/ metadata.name ^ ".tar.gz"
    in
    let pkg_name = Package_name.of_string metadata.name in

    Log.Global.info "serving rpc build (%s)" (colorize pkg_name);

    (* Internal dependencies *)
    let pkg_dependencies = metadata.internal_dependencies in

    let colorized = colorize pkg_name in

    (* Construct the deferred, but don't bind on it. *)
    let start_build =
      start_build
        ~base_dir ~pkg_name ~pkg_dependencies ~metadata ~tarball_path ~tarball ~bin_path
        ~config ~connection_closed ~colorized
    in

    Hashtbl.set builds ~key:pkg_name ~data:start_build;
    match%bind start_build with
    | Error e ->
      Log.Global.info !"Starting build resulted in an error: %{Error#hum}" e;
      return (Error (e, []))
    | Ok { Build_info.process = _; wait = _; response } -> response
