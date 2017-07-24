
(** System interaction functions *)
type format = Sexp | Json | Sexp2

type reader = {
  sign: format -> string -> Module.t list option;
  m2l: format -> Fault.Policy.t -> Read.kind -> string -> Namespaced.t -> Unit.s;
  findlib: Common.task -> Findlib.query -> Common.task ;
  env: Module.dict
}

type writer = {
  sign: format -> string -> Format.formatter -> Module.t list -> unit;
  m2l: format -> (Read.kind * string) -> Format.formatter -> M2l.t -> unit
}

type t = {
  reader: reader;
  writer: writer;
}


(** Read signature file *)
val read_sigfile: format -> string -> Module.t list option

val direct: t
