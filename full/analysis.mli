(** Analysys glue code *)

type param = {
  transparent_aliases: bool;
  transparent_extension_nodes: bool;
  polycy: Fault.Polycy.t;
  precomputed_libs: Name.set  ;
  closed_world: bool;
  sig_only:bool;
}


(** Lift parameter to a module parameter *)
val lift: param -> (module Interpreter.param)

(** [main param task] performs dependency analysis
    with parameters [param] on the given task *)
val main: param -> Common.task -> Unit.r list Unit.pair
