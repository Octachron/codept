(** Location type for error message *)
type t = { pkg:Pkg.t; loc:Loc.t}
val none: t

module Pp: sig
  val simple: t Pp.t
  val tagged: t Pp.t
  val opt: t Pp.t
end
