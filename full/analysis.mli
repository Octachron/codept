(** Analysys glue code *)

type param = {
  epsilon_dependencies: bool;
  transparent_aliases: bool;
  transparent_extension_nodes: bool;
  policy: Fault.Policy.t;
  precomputed_libs: Name.set  ;
  closed_world: bool;
  sig_only:bool;
  format: Io.format
}


(** Lift parameter to a module parameter *)
val lift: param -> (module Outliner.param)

(** [main param task] performs dependency analysis
    with parameters [param] on the given task *)
val main: Io.reader -> param -> Common.task -> Unit.r list Unit.pair
