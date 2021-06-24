(** Findlib queries *)

type query =
  { pkgs: Name.t list;
    predicates: string list;
    syntaxes: Name.t list;
    ppxopts: string list Name.map;
    ppopt: string list
  }

type result = { libs: string list; ppxs: string list; pp: string option }
val pp: Format.formatter -> result -> unit

val empty: query

val predicates: query -> string -> query
val pkg: query -> string -> query
val ppxopt: query -> string -> query
val ppopt: query -> string -> query
val syntax: query -> string -> query


val process: query -> result
