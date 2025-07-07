type t = ..
type t +=
  | Info | Notification | Warning | Error | Critical
  | Em | Loc | Title | M

val of_string: string -> t
val to_string: t -> string

val enable: simple:bool -> Format.formatter -> unit
val with_tag: t -> (Format.formatter -> 'a -> unit as 'p) -> 'p
val tagged: t -> Format.formatter -> string -> unit
