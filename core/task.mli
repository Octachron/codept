(** Task building functions *)



val add_impl : Read.format -> Common.task ref -> string -> unit
val add_intf : Read.format -> Common.task ref -> string -> unit
val add_sig : Common.task ref -> string -> unit
val add_file : Params.t ref -> Common.task ref -> string -> unit
val add_seed : Params.t ref -> Common.task ref -> string -> unit


(** Analyze a file to extract contextual information but do not display
    it when outputing analysis result *)
val add_invisible_file :
  Params.t ref -> Common.task ref -> string -> unit

val add_open : Common.task ref -> string -> unit
val lib : Common.task ref -> string -> unit
val map : Params.t ref -> Common.task ref -> string -> unit
val as_map : Params.t ref -> Common.task ref -> string -> unit
