(** Standard faults *)


(** {2 Extension node fault} *)
val extension_ignored : ( Fault.loc -> string -> unit) Fault.t
val extension_traversed : (Fault.loc -> string -> unit) Fault.t

(** {2 First-class module faults} *)
val opened_first_class : (Fault.loc -> string -> unit) Fault.t
val included_first_class : (Fault.loc -> unit) Fault.t

(** {2 Typing faults} *)
val applied_structure : (Fault.loc -> Module.Partial.t -> unit) Fault.t
val structure_expected : (Fault.loc -> Module.Partial.t -> unit) Fault.t
val applied_unknown : (Fault.loc -> Module.Partial.t -> unit) Fault.t
val unknown_approximated : (Module.level -> Name.t -> Fault.loc -> unit) Fault.t
val nonexisting_submodule:
  (Fault.loc -> Paths.S.t -> Module.level -> Name.t -> unit) Fault.t

val ambiguous :
  (Fault.loc -> Name.t -> Module.Divergence.t -> unit) Fault.t

(** {2 Input faults} *)

(** Module name conflicts: same module path for different
    compilation units *)
val module_conflict: (Namespaced.t -> Paths.P.t list -> unit) Fault.t

(** Local module name conflicts: same module name for different
    compilation units *)
val local_module_conflict: (Namespaced.t -> Paths.P.t list -> unit) Fault.t


(** {2 Parsing approximation faults} *)
val concordant_approximation : (Paths.Pkg.t -> unit) Fault.t
val discordant_approximation :
  (Paths.Pkg.t -> Paths.S.t list -> Paths.S.t list -> unit) Fault.t
val syntaxerr : (Syntaxerr.error -> unit) Fault.t

(** Syntax error when parsing internal files *)
val unknown_file_format: (string -> string -> unit) Fault.t
val future_version: ((int * int * int) -> (int * int * int) -> unit) Fault.t
val wrong_file_kind: (string -> string -> unit) Fault.t
val parsing_error: (string -> string -> unit) Fault.t
