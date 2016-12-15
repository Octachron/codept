(** Read m2l ast and module names from files *)


(** Format type *)
type format =
  | Src (** standard source file *)
  | M2l (** M2l serialized file *)
  | Parsetree (** parsetree ast file *)
  | Cmi (** binary file: e.g. cmi *)

(** Extend M2l.kind to include the format of read file *)
type kind = { format: format; kind: M2l.kind }

(** error type *)
type error = Ocaml of Syntaxerr.error | M2l


val name: string -> Name.t
(** [name filename] gives the module name corresponding to filename *)

val file: kind -> string ->
  Name.t * (M2l.t, error) result
