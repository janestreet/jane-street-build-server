open Async.Std
open Build_pkg_common.Std

val install_dir : base_dir:string -> string
val install_file_for : base_dir:string -> Package_name.t -> string

val log_dir : build_dir:string -> string
val log_filename_for : build_dir:string -> Package_name.t -> string
val rotate_old_log_for : build_dir:string -> Package_name.t -> unit Deferred.t

val bin_dir : base_dir:string -> string

val opam_root : base_dir:string -> string
val opam_root_config : Config.t -> string

val build_dir : base_dir:string -> string
val build_dir_config : Config.t -> string

val bash : string -> run:(string -> string list -> 'a) -> 'a

val is_file : string -> bool Deferred.t

val setup_environmental_variables
  :  opam_root:string
  -> opam_switch:Opam_switch.t
  -> unit Deferred.t

(** To protect from calling [build_pkg_server.exe build] from outside of build server, the
    server sets the environmental variable [subcommand_protection_var]
    to [subcommand_protection_value] and then the subprocess checks them. *)
val subcommand_protection_var : string
val subcommand_protection_value : string

type os =
  | Linux
  | Darwin
  | Other of string

val os : os Deferred.t Lazy.t
