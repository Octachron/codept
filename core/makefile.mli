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
  Fault.handler -> Format.formatter -> Common.synonyms -> param ->
  Comp_unit.r list Comp_unit.pair -> unit
