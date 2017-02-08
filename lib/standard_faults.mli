(** Standard faults *)


(** {2 Extension node fault} *)
val extension_ignored : ( Fault.loc -> string -> unit) Fault.t
val extension_traversed : (Fault.loc -> string -> unit) Fault.t

(** {2 First-class module faults} *)
val opened_first_class : (Fault.loc -> string -> unit) Fault.t
val included_first_class : (Fault.loc -> unit) Fault.t

(** {2 Typing fault} *)
val applied_structure : (Fault.loc -> Module.Partial.t -> unit) Fault.t
val structure_expected : (Fault.loc -> Module.Partial.t -> unit) Fault.t
val applied_unknown : (Fault.loc -> Module.Partial.t -> unit) Fault.t
val unknown_approximated : (Paths.S.t -> Fault.loc -> unit) Fault.t
val ambiguous : (Fault.loc -> Name.t -> Module.Divergence.t -> unit) Fault.t


(** {2 Parsing approximation faults} *)
val concordant_approximation : (Paths.Pkg.t -> unit) Fault.t
val discordant_approximation :
  (Paths.Pkg.t -> string list -> string list -> unit) Fault.t
val syntaxerr : (Syntaxerr.error -> unit) Fault.t

(** Syntax error when parsing serialized m2l file *)
val m2l_syntaxerr: (string -> unit) Fault.t