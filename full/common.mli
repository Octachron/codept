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

(** Task types *)
type task =
  {
    files: (info * string) list; (** files to be analyzed *)
    seeds: Name.t list; (** modules of which ancestors needs to be resolved *)
    invisibles: Paths.S.set; (** files to be analyzed, quietly *)
    libs: Name.t list; (** libraries to be used in the analysis *)
    opens: Paths.S.t list (** modules to be opened at the start of any file *)
  }


(** [local_dependencies u] *)
val local_dependencies: Unit.r -> Paths.Pkg.t list

(**[make_abs bool] if <bool> convert relative path to absolute path *)
val make_abs: bool -> Paths.Pkg.t -> Paths.Pkg.t

(** Check if a package name corresponds to one of the compiler distributed
    libraries *)
val is_stdlib_pkg: Name.t -> bool
