(** Analysis on single files *)

type t =
  | Approx_file
  | One_pass
  | M2l_info
  | M2l

type single = string -> Io.writer -> Format.formatter -> Params.t
  -> Common.info * string * Namespaced.t option -> unit

(** Use the heuristic approximate parser and display the resulting
    lower and upper bound m2l approximation *)
val approx_file: single

(** Do a single pass of the interpreter and display the result *)
val one_pass: single

(** Display an human-readable representation of the m2l ast *)
val m2l_info: single

(** Display a machine-readable version of the m2l ast using the currently
    selected inner format *)
val m2l: single

val eval: t -> single
