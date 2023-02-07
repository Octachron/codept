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

    [{
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

val reflect : t Pp.t

val of_path : string -> t
(** [of_path fpath] takes the {i basename} of the given [fpath], remove the
    extension if it exists, {b capitalises} the first character and validates
    the result as a module name. If the result is {b not} a valid module name
    (if it contains invalid characters such as ['.']), the function raises an
    [Invalid_argument]. Some valid examples are:

    {[
      # open Modname
      # #install_printer pp ;;
      # of_path "foo/bar.ml" ;;
      - : t = "Bar"
      # of_path "foo.ml" ;;
      - : t = "Foo"
      # of_path "module" ;;
      - : t = "Module"
      # of_path "Module" ;;
      - : t = "Module"
    ]} *)

val to_string : t -> string
(** [to_string t] casts the given [t] as a [string]. *)

val modulize : string -> t
(** [modulize str] transforms the given [str] to a valid module name. It
    replaces all invalid characters by ['_'] and {b capitalises} the first
    character. If the first character is {b not} a letter, it raises an
    [Invalid_argument]. *)

val compare : t -> t -> int

module Map : Map.S with type key = t
