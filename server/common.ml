open Core.Std
open Async.Std
open Build_pkg_common.Std

module S = Public_release_helpers.Std.Shell_helpers

let opam_root ~base_dir = base_dir ^/ "opam-root"
let opam_root_config config = opam_root ~base_dir:(Config.base_dir config)
let build_dir ~base_dir = base_dir ^/ "build"
let build_dir_config config = build_dir ~base_dir:(Config.base_dir config)
let install_dir ~base_dir = base_dir ^/ "install-files"
let install_file_for ~base_dir pkg_name =
  install_dir ~base_dir ^/ (Package_name.to_string pkg_name ^ ".install")
let bash bash_command ~run = run "bash" ["-o"; "pipefail"; "-c"; bash_command]

let bin_dir ~base_dir = base_dir ^/ "bin"

let log_dir ~build_dir = build_dir ^/ "log"
let log_filename_for ~build_dir pkg_name =
  log_dir ~build_dir ^/ Package_name.to_string pkg_name ^ ".log"

type os =
  | Linux
  | Darwin
  | Other of string

let os = lazy (
  match%map S.run_one "uname" ["-s"] with
  | "Darwin" -> Darwin
  | "Linux"  -> Linux
  | s        -> Other s
)

let stat_args = lazy(
  match%map Lazy.force os with
  | Darwin          -> ["-f"; "%Sa"; "-t"; "%F %T %z"]
  | Linux | Other _ -> ["-format"; "%y"]
)

let rotate_old_log_for ~build_dir pkg_name =
  let old_logs_dir = log_dir ~build_dir ^/ "old" in
  let%bind () = S.run_zero "mkdir" ["-p"; old_logs_dir] in
  let log_filename = log_filename_for ~build_dir pkg_name in
  let%bind () =
    match%bind Sys.file_exists log_filename with
    | `No | `Unknown -> Deferred.unit
    | `Yes ->
      let%bind stat_args = Lazy.force stat_args in
      let%bind modification_time = S.run_one "stat" (stat_args @ [log_filename]) in
      let old_log_path =
        old_logs_dir ^/ Package_name.to_string pkg_name ^ "-" ^ modification_time ^ ".log"
      in
      let%map () = S.run_zero "mv" [log_filename; old_log_path] in
      (* We can compress the file in the background. *)
      don't_wait_for (S.run_zero "gzip" [old_log_path])
  in
  Writer.save log_filename ~contents:""

let is_file file =
  match%map Sys.is_file file with
  | `Yes -> true
  | `No | `Unknown -> false

let setup_environmental_variables ~opam_root ~opam_switch =
  Unix.putenv ~key:"OPAMROOT" ~data:opam_root;
  Unix.putenv ~key:"OPAMSWITCH" ~data:(opam_switch |> Opam_switch.to_string);
  let%map opam_config_str = S.run "opam" [ "config"; "env"; "--sexp"] >>| String.strip in
  Sexp.of_string_conv_exn opam_config_str [%of_sexp: (string * string) list]
  |> List.iter ~f:(fun (key, data) -> Unix.putenv ~key ~data)

let subcommand_protection_var = "BUILD_PKG_PROTECTION"
let subcommand_protection_value = "dont_call_manually"
