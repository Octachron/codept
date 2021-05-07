(** Localized path for package *)

type source = Local | Unknown | Pkg of Namespaced.t | Special of Name.t
type t = { source : source; file : Namespaced.t; }
type path = t
val sch: t Schematic.t

(** {2 Printing function } *)
val pp_source : Format.formatter -> source -> unit
val pp_simple : Format.formatter -> path -> unit
val pp_gen : string -> Format.formatter -> path -> unit
val pp : Format.formatter -> path -> unit
val reflect: Format.formatter -> path -> unit

val filename : ?sep:string -> t -> string

val local : string -> path
val (/): Paths.Simple.t -> t -> t

val is_known : t -> bool
val module_name : path -> string

(** {2 Extension handling } *)
val update_extension : (string -> string) -> path -> path
val change_extension : string -> path -> path

val cmo : path -> path
val o : path -> path
val cmi : path -> path
val cmx : path -> path
val cmxs : path -> path


val mk_dep : bool -> bool -> path -> path
(** [mk_dep all native path] generates either
    a cmi, a cmo or a cmx depending on the extension of path*)

module Set : sig
  include Set.S with type elt = t
  val pp: Format.formatter -> t -> unit
end
type set = Set.t

module Map : sig
  include Map.S with type key = t
  val find_opt: key -> 'a t -> 'a option
end
type 'a map = 'a Map.t
