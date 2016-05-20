open Build_pkg_common.Std

type t [@@deriving sexp]

val create : base_dir:string -> opam_switch:Opam_switch.t -> t

(** Base directory, everything happens inside *)
val base_dir : t -> string

(** Opam_switch used by this opam installation. *)
val opam_switch : t -> Opam_switch.t
