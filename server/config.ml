open Core
open Build_pkg_common.Std

type t =
  { base_dir : string;
    opam_switch : Opam_switch.t
  } [@@deriving sexp, fields]

let create = Fields.create
