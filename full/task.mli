(** Task building functions *)

(** [classify policy synonyms filename] classifies file type according to
    the dictionary [synonyms] *)
val classify: Fault.Policy.t -> Common.info Name.map -> string -> Common.info option

(** [expand_dir dir] expands [+name] to [$(ocamlc -where)/name] *)
val expand_dir: string -> string

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
