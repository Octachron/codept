(** Codept fault policies *)

(** {2 Codept specific fault} *)
val nonexisting_file: string Fault.info
val unknown_extension: string Fault.info
val invalid_file_group: (string * string) Fault.info


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
