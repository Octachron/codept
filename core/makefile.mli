(** Makefile generation *)

type param =
  {
    all: bool;
    native: bool;
    bytecode: bool;
    abs_path: bool;
    slash:string;
    one_line:bool;
    implicits: bool;
    shared: bool;
    includes: string list;
  }

val main:
  Fault.Policy.t -> Format.formatter -> Common.synonyms -> param ->
  Unit.r list Unit.pair -> unit
