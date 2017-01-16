(** File types for codept *)
type kind = Interface | Implementation | Signature
type info = { format: Read.format; kind : kind }

(** Standard file format *)
val ml: info
val mli: info

(** Convert info to standard type *)
val classic: info -> Read.kind option

(** Filesystem related parameters *)
type param = {
    synonyms: info Name.Map.t;
    includes: Paths.Pkg.path Name.map;
}

(** Task *)
type task =
  {
    files: (Read.kind * string) list Unit.pair;
    signatures: Module.t list;
    invisibles: Paths.S.set;
    libs: string list;
    opens: Paths.S.t list
  }


(** [local_dependencies sort u] *)
val local_dependencies:
  (Paths.Pkg.t list -> Paths.Pkg.t list) -> Unit.r -> Paths.Pkg.t list

val make_abs: bool -> Paths.Pkg.t -> Paths.Pkg.t

val is_stdlib_pkg: Name.t -> bool
