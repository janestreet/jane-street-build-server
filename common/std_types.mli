open Core_kernel

module Package_checksum : Identifiable.S
module Package_name     : Identifiable.S
module Opam_switch      : Identifiable.S

module Package_metadata : sig
  type t =
    { name                  : string
    ; external_dependencies : Package_name.t list
    ; internal_dependencies : Package_name.t list
    }
  [@@deriving sexp, bin_io]
end
