(** Namespaced name, useful for packed module *)

type p = Paths.S.t
type t = { namespace: p; name: Modname.t; file: string}
type namespaced = t
val pp: t Pp.t
val pp_as_filepath: t Pp.t
val reflect: t Pp.t

val cons: p -> t -> t
val to_string: t -> string
val make: ?nms:p -> Name.t -> t
val flatten: t -> p
val of_path: p -> t
val head: t -> string
val compare : t -> t -> int


val sch: t Schematic.t

val module_path_of_filename: ?nms:p -> string -> t
val filepath_of_filename: ?nms:p -> string -> t


module Map: sig
  include Map.S with type key = t
  val find_opt: namespaced -> 'a t -> 'a option
end
type 'a map = 'a Map.t

module Set : sig
  include Set.S with type elt = t
  val pp: Format.formatter -> t -> unit
  val sch: t Schematic.t
end
type set = Set.t

(** {2 Extension and parsing} *)
val change_file_extension : (string -> string) -> t -> t
val module_name: t -> Modname.t
