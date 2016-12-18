(** Result combinators *)

type ('a, 'b) t = ('a, 'b) result

val is_ok : ('a, 'b) t -> bool

val all_done : ('a -> 'b) -> ('a, 'b) t list -> ('a list, 'b list) t
(** [ all_done undone list ], either extracts a full list [Ok l'] is all
    element of [list] are [Ok _] or unravel the [Ok _] elements
    in [list] using the [undone] function, giving back a [Error l].
*)

val fmap : ('error -> 'error2) -> ('ok -> 'ok2) -> ('ok, 'error) t
  -> ('ok2, 'error2) t

val fmap_ok : ('a -> 'b) -> ('a, 'error) t -> ('b, 'error) t

val fmap_error : ('a -> 'b) -> ('ok, 'a) t -> ('ok, 'b) t
