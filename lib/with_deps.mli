type 'a t
val no_deps: 'a -> 'a t
val deps: 'a t -> Deps.t
val value: 'a t -> 'a
val unpack: 'a t -> Deps.t * 'a
val bind: 'a t -> ('a -> 'b t) -> 'b t
val map: 'a t -> ('a -> 'b) -> 'b t
val comm: ('a,'b) result t -> ('a t, 'b t) result

val (>>=): 'a t -> ('a -> 'b t) -> 'b t
val (>>|): 'a t -> ('a -> 'b) -> 'b t

val (<<|): ('a -> 'b) -> 'a t -> 'b t
val (<*>): 'a t -> 'b t -> ('a * 'b) t
val (<+>): Deps.t -> 'a t -> 'a t
