(** File types for codept *)

type kind = Interface | Implementation | Signature
type info = { format: Read.format; kind : kind }

(** Standard file format *)
val ml: info
val mli: info

(** Convert info to standard type *)
val classic: info -> Read.kind option

(** Mapping between extension and file type *)
type synonyms =  info Name.Map.t

(** Task types *)
type task =
  {
    files: (info * string * Namespaced.t option) list;
    (** files to be analyzed *)
    seeds: Namespaced.t list;
    (** modules of which ancestors needs to be resolved *)
    invisibles: Namespaced.Set.t; (** files to be analyzed, quietly *)
    libs: Name.t list; (** libraries to be used in the analysis *)
    opens: Paths.S.t list (** modules to be opened at the start of any file *)
  }


(** [expand_dir dir] expands [+name] to [$(ocamlc -where)/name] *)
val expand_dir: string -> string

(**[make_abs bool] if <bool> convert relative path to absolute path *)
val make_abs: bool -> Pkg.t -> Pkg.t

(** Check if a package name corresponds to one of the compiler
    distributed libraries *)
val is_stdlib_pkg: Name.t -> bool

(** [classify policy synonyms filename] classifies file type
    according to the dictionary [synonyms] *)
val classify: Fault.Policy.t -> info Name.map -> string -> info option
