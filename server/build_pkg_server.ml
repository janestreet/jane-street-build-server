open Core
open Async
open Build_pkg_common.Std

let do_setup ~base_dir ~use_irill_solver _conn_closed (query : Rpcs.Setup.query)
  : Rpcs.Setup.response Deferred.t =
  Rpc_impls.setup ~base_dir ~use_irill_solver ~opam_switch:query

let do_build_v2 ~bin_path conn_closed (query : Rpcs.Build_v2.query)
  : Rpcs.Build_v2.response Deferred.t =
  Rpc_impls.delegate_build
    ~metadata:query.metadata ~bin_path ~tarball:query.tarball
    ~connection_closed:conn_closed

let implementations ~base_dir ~use_irill_solver ~bin_path =
  Rpc.Implementations.create_exn
    ~on_unknown_rpc:(`Call (fun _conn_state ~rpc_tag ~version ->
      Log.Global.error "Unknown rpc (%s, %d)" rpc_tag version;
      `Continue))
    ~implementations:
      [ Rpc.Rpc.implement Rpcs.Setup.rpc (do_setup ~base_dir ~use_irill_solver)
      ; Rpc.Rpc.implement Rpcs.Build_v2.rpc (do_build_v2 ~bin_path)
      ]

let serve ?switch port base_dir bin_path use_irill_solver () =
  let bin_path = Option.value bin_path ~default:Sys.executable_name in
  let implementations = implementations ~base_dir ~bin_path ~use_irill_solver in
  let%bind _ =
    Rpc.Connection.serve
      ~implementations
      ~initial_connection_state:(fun _ conn -> Rpc.Connection.close_finished conn)
      ~where_to_listen:(Tcp.on_port port)
      ()
  in
  Log.Global.info "Serving on port %d" port;
  let%bind () =
    match switch with
    | None -> return ()
    | Some opam_switch ->
      match%bind do_setup ~base_dir ~use_irill_solver () opam_switch with
      | Ok () -> return ()
      | Error (error, raw_log) ->
        if not (List.is_empty raw_log) then begin
          Core.eprintf "raw log tail:\n";
          List.iter raw_log ~f:(Core.eprintf "%s\n")
        end;
        Core.eprintf !"Setup error: %{Error#hum}\n%!" error;
        return ()
  in
  Deferred.never ()

(* This is in the same binary so as to make deployment easier.
   It is run as a subcommand from Rpc_impls.build, as we want to kill it when a new
   request comes. *)
let build config pkg_name tarball_path metadata () =
  let env_ok =
    Unix.getenv Common.subcommand_protection_var
    |> Option.value_map ~default:false ~f:((=) Common.subcommand_protection_value)
  in
  let%map result =
    if not env_ok then
      return (Error (Error.createf "build called from outside of build-pkg server", []))
    else
      let config = Sexp.of_string config |> [%of_sexp: Config.t] in
      let metadata = Sexp.of_string metadata |> [%of_sexp: Package_metadata.t] in
      let () = Log.Global.info " -> starting build" in
      Build.build ~config ~pkg_name:(Package_name.of_string pkg_name) ~tarball_path ~metadata
  in
  Log.Global.info " <- build finished";
  ksprintf
    (Writer.write (Lazy.force Writer.stdout))
    !"%{sexp: (Package_checksum.t, Error.t * string list) Result.t}"
    result

let command_serve =
  Command.async ~summary:"start build_pkg_server"
    (let open Command.Let_syntax in
     let%map_open port = flag "-port"  (required int) ~doc:" server port"
     and base_dir  = flag "-base-dir"  (required string) ~doc:" base directory for opam root(s)"
     and bin_path  = flag "-bin-path"  (optional string) ~doc:" location of this binary"
     and use_irill = flag "-use-irill" no_arg ~doc:" use irill solvers instead of local solver"
     and switch    = flag "-switch"    (optional string)
                       ~doc:"OPAM-SWITCH setup the server immediately using this opam switch"
     in
     serve ?switch:(Option.map switch ~f:Opam_switch.of_string) port base_dir bin_path use_irill)

let command_build =
  Command.async ~summary:"build package; do not call directly"
    (let open Command.Let_syntax in
     let%map_open config = flag "config" (required string) ~doc:" base_dir: sexp of Config.t"
     and pkg_name = flag "pkg-name" (required string) ~doc:" pkg_name: string"
     and tarball_path = flag "tarball-path" (required string) ~doc:" tarball_path: string"
     and metadata = flag "metadata" (required string) ~doc:" metadata: sexp of Package.t"
     in
     build config pkg_name tarball_path metadata)

let command =
  Command.group
    ~summary:"build-pkg server"
    [ "serve", command_serve; "build", command_build ]

let () = Command.run command
