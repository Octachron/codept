(** Pretty printing functions collection *)

val fp : Format.formatter -> ('a, Format.formatter, unit) format -> 'a

(** {2 Short name } *)
val s : ('a, Format.formatter, unit) format -> Format.formatter -> 'a
val const : string -> Format.formatter -> unit
val string : Format.formatter -> string -> unit
val estring : Format.formatter -> string -> unit

val std : Format.formatter
val err : Format.formatter
val p : ('a, Format.formatter, unit) format -> 'a
val e : ('a, Format.formatter, unit) format -> 'a

(** {2 Combinators } *)

val list :
  ?pre:(Format.formatter -> unit) ->
  ?post:(Format.formatter -> unit) ->
  ?sep:(Format.formatter -> unit) ->
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit
val tlist :
  ?sep:(Format.formatter -> unit) ->
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit
val blist :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit
val clist :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit
val opt_list :
  ?pre:(Format.formatter -> unit) ->
  ?post:(Format.formatter -> unit) ->
  ?sep:(Format.formatter -> unit) ->
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit
val opt_list_0 :
  ?pre:(Format.formatter -> unit) ->
  ?post:(Format.formatter -> unit) ->
  ?sep:(Format.formatter -> unit) ->
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a list -> unit

val decorate :
  string ->
  string ->
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a -> unit

val pair :
  ?sep:string ->
  (Format.formatter -> 'a -> unit) ->
  (Format.formatter -> 'b -> unit) -> Format.formatter -> 'a * 'b -> unit

val triple :
  ?sep:string ->
  (Format.formatter -> 'a -> unit) ->
  (Format.formatter -> 'b -> unit) ->
  (Format.formatter -> 'c -> unit) ->
  Format.formatter -> 'a * 'b * 'c -> unit


val snd :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'b * 'a -> unit

val fst :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a * 'b -> unit

val opt :
  ?pre:string ->
  ?post:string ->
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a option -> unit
