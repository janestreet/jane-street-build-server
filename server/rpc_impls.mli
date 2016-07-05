open Async.Std
open Build_pkg_common.Std

val setup
  :  base_dir:string
  -> opam_switch:Opam_switch.t
  -> use_irill_solver:bool
  (* We can't open Rpcs and just use [Setup.response], because of build system quirks
     (there's a setup.ml in public-release/common-files which causes conflicts. *)
  -> Rpcs.Setup.response Deferred.t

(** Handle build request.

    [connection_closed] is to detect when the client disconnects, for instance for manual
    request when the user does Ctrl+C. In this case we kill the build prematurely.

    [bin_path] is the path to this binary, for doing the build in a sub-process. Typically
    it is [Sys.argv.(0)].
*)
val delegate_build
  :  metadata:Package_metadata.t
  -> bin_path:string
  -> tarball:string
  -> connection_closed:unit Deferred.t
  -> Rpcs.Build_v2.response Deferred.t

