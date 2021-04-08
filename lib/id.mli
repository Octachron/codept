type core
type t = Id of core [@@unboxed]

val compare: t -> t -> int

val pp: t Pp.t
val sch: t Schematic.t
type seed
val create_seed: Paths.Pkg.t -> seed
val create: seed -> t
