(** Support module for recently introduced function in stdlib *)

val extension: string -> string
val remove_extension: string -> string
val split_on_char: char -> string -> string list
val opt: ('a -> 'b) -> 'a -> 'b option
val filter_map: ('a -> 'b option) -> 'a list -> 'b list
