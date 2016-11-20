(** Extract information from cmi files *)

val m2l : string -> M2l.t
(** [m2l filename] is the m2l AST representation of the
    cmi file filename *)
