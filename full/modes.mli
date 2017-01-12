(** Codept output modes *)

type mode = Format.formatter -> Params.t -> Unit.r list Unit.pair -> unit

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
val dsort: mode

(** Export the inferred m2l signature in ocaml syntax *)
val export: mode

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
