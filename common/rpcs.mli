open Core_kernel
open Async_rpc_kernel
open Std_types

module type S = sig
  type query [@@deriving bin_io, sexp_of]
  type response [@@deriving bin_io, sexp_of]

  val name : string
  val rpc : (query, response) Rpc.Rpc.t
end

module Setup : sig
  include S
    with type query = Opam_switch.t
     (* last lines of the raw log (in case of a failure) *)
     and type response = (unit, Error.t * string list) Result.t
end
(*
module Build_v1 : sig
  type query =
    { tarball : string; (* contents *)
      metadata : Public_release_lib.Package_types.Package.t }
  (* checksum and last lines of the raw log (in case of a failure) *)
  type response = (Package_checksum.t, Error.t * string list) Result.t

  include S
    with type query := query
     and type response := response
end
*)
module Build_v2 : sig
  type query =
    { tarball  : string (* contents *)
    ; metadata : Package_metadata.t
    }
  (* checksum and last lines of the raw log (in case of a failure) *)
  type response = (Package_checksum.t, Error.t * string list) Result.t

  include S
    with type query := query
     and type response := response
end
