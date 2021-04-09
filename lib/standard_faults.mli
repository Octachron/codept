(** Standard faults *)


(** {2 Extension node fault} *)
val extension_ignored : ( Uloc.t * string ) Fault.info
val extension_traversed : ( Uloc.t * string ) Fault.info

(** {2 First-class module faults} *)
val opened_first_class : ( Uloc.t * string option ) Fault.info
val included_first_class : Uloc.t Fault.info

(** {2 Typing faults} *)
val applied_structure : ( Uloc.t * Module.Partial.t ) Fault.info
val included : ( Uloc.t * Module.sty * [ `Abstract | `Functor ] ) Fault.info
val opened : ( Uloc.t * Module.sty * [ `Abstract | `Functor ] ) Fault.info
val applied_unknown : ( Uloc.t * Module.Partial.t ) Fault.info
val unknown_approximated : ( Uloc.t * Module.level * Name.t ) Fault.info
val nonexisting_submodule:
  (Uloc.t * Paths.S.t * Module.level * Name.t ) Fault.info

val ambiguous : (Uloc.t * Name.t * Module.Divergence.t ) Fault.info

(** {2 Input faults} *)

(** Module name conflicts: same module path for different
    compilation units *)
val module_conflict: ( Namespaced.t * Paths.P.t list ) Fault.info

(** Local module name conflicts: same module name for different
    compilation units *)
val local_module_conflict: ( Namespaced.t * Paths.P.t list ) Fault.info


(** {2 Parsing approximation faults} *)
val concordant_approximation : Paths.Pkg.t Fault.info
val discordant_approximation :
  ( Paths.Pkg.t * Paths.S.t list * Paths.S.t list ) Fault.info
val syntaxerr : Syntaxerr.error Fault.info
val lexerr : ( string * Lexer.error ) Fault.info


(** Syntax error when parsing internal files *)
val unknown_file_format : ( string * string ) Fault.info
val future_version :
 ( string * (int * int * int) * (int * int * int) ) Fault.info
val wrong_file_kind : ( string * string * string ) Fault.info
val parsing_error : ( string * string ) Fault.info

val schematic_errors:
  Fault.Policy.t -> string * Name.t * Schematic.Ext.error -> unit
