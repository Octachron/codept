(** The Unit name is the name of a module which can be represented by a file.
    The unit name {b transforms} the given file path into a {!type:Modname.t}
    and keep the file path internally.

    By this way, an {!t} as two views:
    - as a file path (where the module is located into the file-system)
    - as a module name
*)

type t
(** Type of an unit name. *)

val modulize : string -> t
(** [modulize filename] makes a new {!type:t} which contains the given
    [filename] and {i modulize} it: it removes the extension (if it exists), it
    capitalises the first letter of the [filename]'s {!val:Filename.basename}
    and replace any wrong characters by ['_'].

    For instance:
    - ["foo.ml"] => ["Foo"]
    - ["Foo"] => ["Foo"]
    - ["foo'.ml"] => ["Foo_"]
    - ["lib/foo.ml"] => ["Foo"]
    - ["foo-bar.ml"] => ["Foo-bar"]

    We assert that:
    {[
      # let v = Unitname.modulize "foo.ml" ;;
      # assert (Stdlib.compare v Unitname.(modulize (filepath v)) = 0) ;;
    ]}
*)

val modname : t -> Modname.t
val filename : t -> string
(** [filename v] returns the {b filename} of the given unit name.
    The filename is the {!val:Filename.basename} of the [filepath]
    given to construct [v] with {!val:modulize}. *)

val filepath : t -> string
(** [filepath v] returns the {b filepath} of the given unit name.
    The file path is the one used to construct [v] with {!val:modulize}. *)

val pp : t Pp.t
val pp_as_modname : t Pp.t
val pp_as_filepath : t Pp.t
val reflect : t Pp.t

val compare_as_modnames : t -> t -> int
(** [compare_as_modnames a b] compares [a] and [b] from their
    modname's views. For instance,

    {[
      # let a = Unitname.modulize "foo/a.ml" ;;
      # let b = Unitname.modulize "bar/a.ml" ;;
      # Unitname.compare_as_modnames a b ;;
      - : int = 0
    ]} *)

val change_file_extension : (string -> string) -> t -> t
(** [change_file_extension f t] tries to replace the current extension of the
    given [t] (the {i filename} view) by something else returned by [f]. If [t]
    has no extension, we return it unchanged. Otherwise, we call [f] and give to
    it the current extension.

    {b NOTE}: The {i modname} view is {b unchanged} in any cases:
    {[
      # let v0 = Unitname.modulize "foo.ml" ;;
      # let v1 = Unitname.change_file_extension (fun _ -> "mli") v0 ;;
      # assert (Modname.compare (Unitname.modname v0) (Unitname.modname v1) = 0) ;;
    ]}

    However, [v0] and [v1] are not equal anymore. *)

module Map : Map.S with type key = t
module Set : Set.S with type elt = t
