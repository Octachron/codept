(** Option helper functions *)

val bind : 'a option -> ('a -> 'b option) -> 'b option
val fmap : ('a -> 'b) -> 'a option -> 'b option
val iter: ('a -> unit) -> 'a option -> unit
val join : 'a option option -> 'a option


val either : ('a -> 'b) -> 'b -> 'a option -> 'b
val lazy_default: 'a Lazy.t -> 'a option -> 'a
val default : 'a -> 'a option -> 'a

(** Monadic operators *)
val ( >>| ) : 'a option -> ('a -> 'b) -> 'b option
val ( >>= ) : 'a option -> ('a -> 'b option) -> 'b option
val ( >> ) : 'a option -> 'b option -> 'b option
val (|||): 'a option -> 'a option Lazy.t -> 'a option

val ( >< ) : 'a option -> 'a -> 'a


val ( && ) : 'a option -> 'b option -> ('a * 'b) option

module List': sig
  val join : 'a option list -> 'a list option
  val filter: 'a option list -> ' a list
  val map : ('a -> 'b option) -> 'a list -> 'b list option
end
