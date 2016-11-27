(** Read m2l ast and module names from files *)

val name: string -> Name.t
(** [name filename] gives the module name corresponding to filename *)

val file: M2l.kind -> string -> Name.t * M2l.t
