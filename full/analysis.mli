(** Analysys glue code *)

type param = {
  transparent_aliases: bool;
  transparent_extension_nodes: bool;
  polycy: Fault.Polycy.t;
  precomputed_libs: Name.set  ;
  closed_world: bool;
  sig_only:bool;
}

(** System interaction function *)
type io = {
  sign: string -> Module.t list option;
  m2l: Fault.Polycy.t -> Read.kind -> string -> Unit.s;
  findlib: Common.task -> Findlib.query -> Common.task ;
  env: Module.Def.t
}

val direct_io: io

(** Read signature file *)
val read_sigfile: string -> Module.t list option

(** Lift parameter to a module parameter *)
val lift: param -> (module Interpreter.param)

(** [main param task] performs dependency analysis
    with parameters [param] on the given task *)
val main: io -> param -> Common.task -> Unit.r list Unit.pair
