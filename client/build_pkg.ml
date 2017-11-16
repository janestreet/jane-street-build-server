open Core
open Async
open Ocaml_re
open Public_release_lib.Package_types
open Public_release_lib.Import
open Build_pkg_common.Std

(* The errors we get from [make] contain file names relative to the package prepared
   for public release. Instead, we want the file names to correspond to file names
   in the repo.

   [file_list] is the list of files copied from the repo and [dir_mapping] tells where
   these files go in the public released package. *)
let process_log =
  let regexp = Re.compile (Re_posix.re "^File \"(.+)\", line ([0-9]+)(.*)$") in
  let module Grep = Public_release_lib.Grep_internal_stuff in
  fun ~file_list ~dir_mapping ~tarball_path ~pkg_name raw_log ->
    let file_mapping = Grep.file_mapping ~file_list ~dir_mapping in
    (* Line mapping between [public_file] and its internal equivalent. *)
    let line_mapping public_file =
      match%map
        let public_file =
          if String.is_prefix public_file ~prefix:"./"
          then String.drop_prefix public_file 2
          else public_file
        in
        match Map.find file_mapping public_file with
        | None -> Deferred.Or_error.errorf "Mapping not found for %s" public_file
        | Some internal_file ->
          Monitor.try_with_or_error (fun () ->
            S.run_lines "tar" ["-Oxf"; tarball_path; pkg_name ^/ public_file])
          >>=? fun public_lines ->
          let%map internal_lines =
            let%bind root = get_root () in
            Reader.file_lines (root ^/ internal_file)
          in
          Ok (Grep.line_mapping' ~public_lines ~internal_lines)
      with
      | Ok map -> map
      | Error err ->
        Log.Global.error !"Error while creating line mapping for %s: %{Error#hum}"
          public_file err;
        Int.Map.empty
    in
    let line_mapping = Core_extended.Cache.memoize line_mapping in

    (* Map file names and line_numbers in 'File "<filename>", line <line_no>, (...)'. *)
    let raw_log =
      List.map raw_log ~f:(fun line ->
        Option.try_with (fun () -> Re.exec regexp line)
        |> Option.value_map ~default:(return line) ~f:(fun execd ->
          let matches = Re.get_all execd in
          let filename = matches.(1) in
          let line_no = matches.(2) |> Int.of_string in
          let line_tail = matches.(3) in
          let new_filename =
            let filename =
              let pp_ml = ".pp.ml" in
              let pp_mli = ".pp.mli" in
              if String.is_suffix filename ~suffix:pp_ml then
                Filename.chop_suffix filename pp_ml ^ ".ml"
              else if String.is_suffix filename ~suffix:pp_mli then
                Filename.chop_suffix filename pp_mli ^ ".mli"
              else
                filename
            in
            match Map.find file_mapping filename with
            | None -> Log.Global.error "%s not found in file mapping" filename; line
            | Some orig_filename -> orig_filename
          in
          let%map line_mapping = line_mapping filename in
          let new_line_no = match Map.find line_mapping line_no with
            | None ->
              Log.Global.error
                !"line mapping incomplete: %d not in\n%{sexp: int Int.Map.t}\n"
                line_no line_mapping;
              line_no
            | Some internal_line_no -> internal_line_no
          in
          let reach_root_from__public_release__repos__pkg_dir = "../../.." in
          sprintf "File \"%s/%s\", line %d%s"
            reach_root_from__public_release__repos__pkg_dir
            new_filename new_line_no line_tail))
      |> Deferred.all
    in
    raw_log


let dispatch ~host ~port rpc query =
  match%bind Rpc.Connection.client ~host ~port () with
  | Error exn -> return (Or_error.of_exn exn)
  | Ok connection ->
    Rpc.Rpc.dispatch rpc connection query

let print_raw_log raw_log =
  if not (List.is_empty raw_log)
  then begin
    Core.eprintf "raw log tail:\n";
    List.iter raw_log ~f:(Core.eprintf "%s\n")
  end

let server_side_metadata_of_client_side_metatada
      (metadata : Package.t) (deps : Package_dep.t list)
  : Package_metadata.t =
  let internal_dependencies, external_dependencies =
    List.partition_map deps ~f:(function
      | Internal opam_package -> `Fst (Package_name.of_string opam_package)
      | External opam_package -> `Snd (Package_name.of_string opam_package))
  in
  { name = metadata.name
  ; internal_dependencies
  ; external_dependencies
  }

let build_v2 host port checksum_file tarball_dir () =
  let tarball_path  = tarball_dir ^/ "dist.tar.gz" in
  let metadata_path = tarball_dir ^/ "metadata.sexp" in
  let deps_path = tarball_dir ^/ "package-deps.sexp" in
  let%bind metadata = Reader.load_sexp_exn metadata_path Package.t_of_sexp in
  let%bind deps = Reader.load_sexp_exn deps_path [%of_sexp: Package_dep.t list] in
  let%bind file_list = Reader.file_lines metadata.file_list_filename in
  let%bind tarball_contents = Reader.file_contents tarball_path in
  let query =
    { Rpcs.Build_v2.
      tarball  = tarball_contents
    ; metadata = server_side_metadata_of_client_side_metatada metadata deps
    }
  in
  dispatch ~host ~port Rpcs.Build_v2.rpc query
  >>=? function
  | Ok checksum ->
    (match checksum_file with
     | Some fn ->
       Writer.save fn ~contents:(Package_checksum.to_string checksum)
       >>| Or_error.return
     | None ->
       printf !"%{Package_checksum}\n" checksum;
       Deferred.Or_error.return ())
  | Error (error, raw_log) ->
    let%map raw_log =
      (* This could potentially skip reading the tarball from disk again (by tar),
         but it's not worth the trouble. *)
      process_log
        raw_log ~file_list ~dir_mapping:metadata.dir_mapping ~tarball_path
        ~pkg_name:metadata.name
    in
    print_raw_log raw_log;
    Or_error.errorf !"Server returned error: %{Error#hum}" error

let setup host port opam_switch () =
  match%map dispatch ~host ~port Rpcs.Setup.rpc (Opam_switch.of_string opam_switch) with
  | Error _ as err -> err
  | Ok result ->
    Result.map_error result ~f:(fun (error, raw_log) ->
      print_raw_log raw_log;
      ksprintf Error.of_string !"Server returned error: %{Error#hum}" error)

let host_port_flags () =
  let open Command.Let_syntax in
  let%map_open host = flag "-host" ~doc:"<host> host of a build-pkg server" (required string)
  and port = flag "-port" ~doc:"<port> port of a build-pkg server" (required int)
  in
  host, port

let rpc_version =
  Command.Arg_type.of_alist_exn
    [ "2", build_v2
    ]

let command_build =
  Command.async_or_error ~summary:"compile packages on remote servers"
    (let open Command.Let_syntax in
     let%map_open host, port = host_port_flags ()
     and checksum_file =
       flag "-checksum-file" (optional file)
         ~doc:"FILE file to which the checksum of the compiled package is saved to"
     and package_dir = anon ("package_dir" %: string)
     and builder =
       flag "-rpc-version" (optional_with_default build_v2 rpc_version)
         ~doc:"VER version of the RPC call to use"
     in
     builder host port checksum_file package_dir)


let command_setup =
  Command.async_or_error ~summary:"setup the server to use the given switch"
    (let open Command.Let_syntax in
     let%map_open host, port = host_port_flags ()
     and opam_switch =
       flag "-opam-switch" ~doc:" the compiler to be used by the server" (required string)
     in
     setup host port opam_switch)

let command =
  Command.group
    ~summary:"manage remote build servers"
    [ "build", command_build;
      "setup", command_setup ]

let () = Command.run command
