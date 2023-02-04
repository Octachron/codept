(** Support module for recently introduced function in stdlib *)

val extension: string -> string
(** [extension name] is the shortest suffix [ext] of [name0] where:
    - [name0] is the longest suffix of [name] that does not contain
      a directory separator;
    - [ext] does {b not} starts with a period (such as
      [name0 ^ "." ^ ext = name])
    - [ext] plus the dot is preceded by at least one non-period character in
      [name0].

    If such a suffix does not exist, [extension name] is the empty string. *)

val remove_extension: string -> string
(** Return the given file name without its extension, as defined in
    {!val:extension}. If the extension is empty, the function returns the given
    file name.

    The following invariant holds for any file name [s]:
    [remove_extension s ^ "." ^ extension s = s]
*)

val split_on_char: char -> string -> string list
val opt: ('a -> 'b) -> 'a -> 'b option
val filter_map: ('a -> 'b option) -> 'a list -> 'b list
