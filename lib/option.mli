(** Option helper functions *)

val bind : 'a option -> ('a -> 'b option) -> 'b option
val fmap : ('a -> 'b) -> 'a option -> 'b option
val join : 'a option option -> 'a option

val either : ('a -> 'b) -> 'b -> 'a option -> 'b
val default : 'a -> 'a option -> 'a

(** Monadic operators *)
val ( >>| ) : 'a option -> ('a -> 'b) -> 'b option
val ( >>= ) : 'a option -> ('a -> 'b option) -> 'b option
val ( >> ) : 'a option -> 'b option -> 'b option


val ( >< ) : 'a option -> 'a -> 'a

val ( && ) : 'a option -> 'b option -> ('a * 'b) option
val list_join : 'a option list -> 'a list option
val list_map : ('a -> 'b option) -> 'a list -> 'b list option
