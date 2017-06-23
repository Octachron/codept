(** Codept output modes *)

type filter =
  | Inner
  | Extern
  | Lib
  | Dep

type variant =
  | Standard
  | Nl

type t =
  | Aliases
  | Dot
  | Export of Name.t
  | Modules of variant * filter
  | Info
  | Signature
  | Sort


type mode = string -> Io.writer -> Format.formatter
  -> Params.t -> Unit.r list Unit.pair -> unit

val eval: t -> mode

(** Display aliases:
    compilation unit submodules aliased to another compiler units.
    Note that aliases are constructing by the interpreter only if
    the option -no-alias-deps is active.
 *)
val aliases: mode

(** Display interface dependencies in graphviz format (for graph vizualisation *)
val dot: mode

(** Display compilation units using the topological partial order for
    implementation files *)
val sort: mode

(** Export the inferred m2l signature in ocaml syntax *)
val export: Name.t -> mode

(** Display generic information on the result of codept analysis *)
val info: mode

(** Display the list of modules dependencies of the input compilation units
    using ocamldep format *)
val modules : ?filter:(Paths.Pkg.t -> bool) -> mode

(** Display module dependencies line-by-line, intended to use by ocamlbuild *)
val line_modules: ?filter:(Paths.Pkg.t -> bool) -> mode

(** Export the inferred m2l signature in a s-expression format, intended
    to be read by ulterior call to codept *)
val signature: mode
