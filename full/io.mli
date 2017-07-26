
(** System interaction functions *)

type reader = {
  sign: string -> Module.t list option;
  m2l: Fault.Policy.t -> Read.kind -> string -> Namespaced.t
    -> Unit.s;
  findlib: Common.task -> Findlib.query -> Common.task ;
  env: Module.dict
}

type writer = {
  sign: Schematic.format -> string -> Format.formatter -> Module.t list -> unit;
  m2l: Schematic.format -> (Read.kind * string) -> Format.formatter -> M2l.t -> unit
}

type t = {
  reader: reader;
  writer: writer;
}

(** Read signature file *)
val read_sigfile: string -> Module.t list option

val direct: t
