(** Warning logging function and messages *)

val log : ('a, Format.formatter, unit, unit, unit, unit) format6 -> 'a
val log_s : string -> unit

val extension : unit -> unit
val confused : string -> unit

val first_class_module : unit -> unit
val opened_first_class : string -> unit
val included_first_class : unit -> unit
