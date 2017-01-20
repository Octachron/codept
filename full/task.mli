(** Task building functions *)

(** [classify polycy synonyms filename] classifies file type according to
    the dictionary [synonyms] *)
val classify: Fault.Polycy.t -> Common.info Name.map -> string -> Common.info option

val add_impl : Read.format -> Common.task ref -> string -> unit
val add_intf : Read.format -> Common.task ref -> string -> unit
val add_sig : Common.task ref -> string -> unit
val add_file : Params.t ref -> Common.task ref -> string -> unit
val add_seed : Params.t ref -> Common.task ref -> Name.t -> unit


(** Analyze a file to extract contextual information but do not display
    it when outputing analysis result *)
val add_invisible_file : Params.t ref -> Common.task ref -> string -> unit

val add_open : Common.task ref -> string -> unit
val lib : Common.task ref -> string -> unit
val map : Params.t ref -> Common.task ref -> string -> unit
val as_map : Params.t ref -> Common.task ref -> string -> unit
