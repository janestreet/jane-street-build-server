open Async
open Build_pkg_common.Std

(** The actual build, invoked in the sub-process *)
val build
  :  config:Config.t
  -> pkg_name:Package_name.t
  -> tarball_path:string
  -> metadata:Package_metadata.t
  -> Build_pkg_common.Rpcs.Build_v2.response Deferred.t
