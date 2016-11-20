(** Errors and associated exceptions *)

(** Log an error message *)
val log : ('a, Format.formatter, unit, unit, unit, 'b) format6 -> 'a

(** Log a string only error message *)
val log_s : string -> 'a

(** Error message for using a functor when a signature was expected *)
val signature_expected : unit -> 'a
