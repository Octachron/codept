(** Codept fault polycies *)

(** {2 Codept specific fault} *)

val unknown_extension: (string -> unit) Fault.t

(** Syntax error when parsing serialized m2l file *)
val m2l_syntaxerr: (string -> unit) Fault.t

(** Solver error when trying to resolve dependencies *)
val solver_error: (Solver.i list -> unit) Fault.t

(** Module name conflicts: same module name for different compilation units *)
val module_conflict: (Name.t -> Paths.P.t list -> unit) Fault.t

(** Default polycy *)
val polycy: Fault.Polycy.t

(** Default polycy but recovers parser failure with the heuristic parser *)
val parsing_approx: Fault.Polycy.t

(** Parsing_approx polycy and fails only on completely unrecoverable error *)
val lax: Fault.Polycy.t

(** Lax polycies and does not display any messges*)
val quiet: Fault.Polycy.t

(** Default polycy and fails on module typing error *)
val strict:Fault.Polycy.t
