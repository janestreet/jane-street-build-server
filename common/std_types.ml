open Core_kernel.Std

module Package_checksum = String
module Package_name     = String
module Opam_switch      = String

module Package_metadata = struct
  type t =
    { name                  : string
    ; external_dependencies : Package_name.t list
    ; internal_dependencies : Package_name.t list
    }
  [@@deriving sexp, bin_io]
end
