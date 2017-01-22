(** Analysis on single files *)

type t =
  | Approx_file
  | One_pass
  | M2l
  | M2l_sexp

type single = Format.formatter -> Params.t -> Common.info * string -> unit

(** Use the heuristic approximate parser and display the resulting
    lower and upper bound m2l approximation *)
val approx_file: single

(** Do a single pass of the interpreter and display the result *)
val one_pass: single

(** Display an human-readable representation of the m2l ast *)
val m2l: single

(** Display a machine-readable s-expression for the m2l ast *)
val m2l_sexp: single

val eval: t -> single
