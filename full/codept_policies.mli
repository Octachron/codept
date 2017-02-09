(** Codept fault polycies *)

(** {2 Codept specific fault} *)

val unknown_extension: (string -> unit) Fault.t

(** Solver error when trying to resolve dependencies *)
val solver_error: (Solver.i list -> unit) Fault.t


(** Default policy *)
val policy: Fault.Policy.t

(** Default policy but recovers parser failure with the heuristic parser *)
val parsing_approx: Fault.Policy.t

(** Parsing_approx policy and fails only on completely unrecoverable error *)
val lax: Fault.Policy.t

(** Lax polycies and does not display any messges*)
val quiet: Fault.Policy.t

(** Default policy and fails on module typing error *)
val strict:Fault.Policy.t
