

(** Data types are defined in M2l_ast *)
open M2l_ast

(** {2 Main types } *)

module Make: functor(Path: sig
  type t
  module Map: Map.S with type key = t
  val sch: t Schematic.t
  val pp: t Pp.t
end) -> S with type ident = Path.t and module Ident_map = Path.Map


include S with type ident = Paths.S.t

type m2l = Def.t
type t = m2l

(**
   {2 Basis analysis }

*)

(**
   The Block module gathers functions that aims to compute the first
   dependencies that need to be resolved before any outliner can make
   progress evaluating a given code block *)
module Block: sig
  val m2l: m2l -> (Summary.t * Paths.S.t) Loc.ext option
end

(** {!Normalize} computes the normal form of a given m2l code fragment.
    When possible, successive expression of the same kind are merged.
*)
module Normalize: sig

  val all: m2l -> bool * m2l
  (** [all fragment ≡ (has_some_simplification_been_made, resulting_m2l) ] *)

  val minor: Annot.t -> Annot.t
  val value: Annot.t -> m2l -> Annot.t
end
