(** Module name.

    the module name is a [string] that respects certain predicates. It is
    impossible to have any other characters than:
    - ['a' .. 'z']
    - ['A' .. 'Z']
    - ['0' .. '9']
    - ['_'], ['-'] or ['\'']

    {b NOTE}: The dash is accepted even if it is normally prohibited. You can
    compile a module [my-module.ml]. However, the compiler issues a warning. In
    order to be the least restrictive, we accept this character.

    The module name must start with a letter.

    {b NOTE}: The name of a {i module type} can start with a non-capital
    letter:

    {[
      module type foo = sig end
    ]} *)

type t
(** The type of module names. *)

val of_string : string -> (t, [> `Msg of string ]) result
(** [of_string str] validates the given [str] as a module name or return an
    error with an explanation. *)

val v : string -> t
(** [v str] calls {!val:of_string}. It raises an [Invalid_argument] instead of
    returning an error. *)

val pp : t Pp.t
(** Pretty printer of {!type:t}. *)

val to_string : t -> string
(** [to_string t] casts the given [t] as a [string]. *)

val compare : t -> t -> int
val reflect : t Pp.t

module Map : Support.Map.S with type key = t
