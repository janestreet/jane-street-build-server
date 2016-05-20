open Core_kernel.Std
open Async_rpc_kernel.Std
open Std_types

module type S = sig
  type query [@@deriving bin_io, sexp_of]
  type response [@@deriving bin_io, sexp_of]

  val name : string
  val rpc : (query, response) Rpc.Rpc.t
end

module Setup = struct
  type query = Opam_switch.t [@@deriving bin_io, sexp_of]
  type response = (unit, Error.t * string list) Result.t [@@deriving bin_io, sexp_of]

  let name = "setup"
  let rpc = Rpc.Rpc.create ~name ~version:1 ~bin_query ~bin_response
end
(*
module Build_v1 = struct
  type query =
    { tarball : string;
      metadata : Public_release_lib.Package_types.Package.t }
  [@@deriving bin_io, sexp_of]
  type response =
    (Package_checksum.t, Error.t * string list) Result.t [@@deriving bin_io, sexp_of]

  let name = "build"
  let rpc = Rpc.Rpc.create ~name ~version:1 ~bin_query ~bin_response
end
*)
module Build_v2 = struct
  type query =
    { tarball  : string
    ; metadata : Package_metadata.t
    }
  [@@deriving bin_io, sexp_of]

  type response = (Package_checksum.t, Error.t * string list) Result.t
  [@@deriving bin_io, sexp_of]

  let name = "build"
  let rpc = Rpc.Rpc.create ~name ~version:2 ~bin_query ~bin_response
end
