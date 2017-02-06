open Core
open Async
open Build_pkg_common.Std

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
  (* Contains last [log_last_lines_count] lines of the outputs of the commands run using
     [run] as well as all lines directly passed to [log]. *)
  { queue : string Queue.t;
    (* Like S.run, but captures the input and output of the command in build-log
       files. *)
    run : run_t;
    (* Prints the line to build-logs and adds it to [queue] *)
    log : log_t }

val log_last_lines_count : int

val create : unit -> t
