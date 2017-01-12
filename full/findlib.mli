(** Findlib queries *)
type info

val empty: info

val predicates: info -> string -> info
val pkg: info -> string -> info
val ppxopt: info -> string -> info
val ppopt: info -> string -> info
val syntax: info -> string -> info


val process: info -> string list * string option list
