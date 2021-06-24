(** Generic args processing *)

(** List of requested output *)
type action = {
  modes : (string * Modes.t) list;
  singles : (string * Single.t) list;
  makefiles : string list;
}


type query = {
  action : action;
  findlib : Findlib.query;
  params : Params.t;
  task : Common.task;
}

(** Translate arguments into a query *)
val process :
  float -> ?extra:(string * Arg.spec * string) list -> string array -> query

(** Query the findlib database and add the relevant information into
    the requested task
*)
val translate_findlib_query : Common.task -> Findlib.query -> Common.task

(** Perform a single action for a given task *)
val eval_single :
  Format.formatter ->
  Io.writer -> Params.t -> Common.task -> string * Single.t -> unit

(** Format codept analysis according to the requested output mode *)
val iter_mode :
  Format.formatter ->
  Io.writer -> Params.t -> Unit.u list Unit.pair -> string * Modes.t -> unit

(** Format codept analysis according into a makefile *)
val iter_makefile :
  Format.formatter -> Params.t -> Unit.u list Unit.pair -> string -> unit
