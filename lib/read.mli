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
type ocaml_parsing_error = Syntax of Syntaxerr.error | Lexer of Lexer.error
type error = Ocaml of ocaml_parsing_error | Serialized of Schematic.Ext.error


val name: string -> Modname.t
(** [name filename] gives the module name corresponding to filename *)

val file: kind -> string ->
  Modname.t * (M2l.t, error) result
