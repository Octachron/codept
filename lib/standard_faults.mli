(** Standard faults *)


(** {2 Extension node fault} *)
val extension_ignored : ( Fault.loc * string ) Fault.info
val extension_traversed : ( Fault.loc * string ) Fault.info

(** {2 First-class module faults} *)
val opened_first_class : ( Fault.loc * string option ) Fault.info
val included_first_class : Fault.loc Fault.info

(** {2 Typing faults} *)
val applied_structure : ( Fault.loc * Module.Partial.t ) Fault.info
val structure_expected : ( Fault.loc * Module.Partial.t ) Fault.info
val applied_unknown : ( Fault.loc * Module.Partial.t ) Fault.info
val unknown_approximated : ( Fault.loc * Module.level * Name.t ) Fault.info
val nonexisting_submodule:
  (Fault.loc * Paths.S.t * Module.level * Name.t ) Fault.info

val ambiguous : (Fault.loc * Name.t * Module.Divergence.t ) Fault.info

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
