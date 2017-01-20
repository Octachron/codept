(** Makefile generation *)

type param =
  {
    all: bool;
    native: bool;
    bytecode: bool;
    abs_path: bool;
    slash:string;
    implicits: bool;
  }

val main:
  Format.formatter -> Common.param -> param -> Unit.r list Unit.pair -> unit