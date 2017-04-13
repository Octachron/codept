(** Functions for handling unit (aka .ml/.mli) files *)

(** Module abbreviations *)
module Pkg = Paths.Pkg
module Pth = Paths.Simple

(** Precision of the inferred module-level signature *)
type precision =
  | Exact
  | Approx

(** Input type *)
type s = {
  path: Namespaced.t; (** module path of the compilation unit *)
  src: Pkg.t; (** source file of the compilation unit *) 
  kind: M2l.kind;
  precision: precision;
  code: M2l.t;
}

(** Output type *)
type r = {
  path: Namespaced.t;
  src: Pkg.t;
  kind: M2l.kind;
  precision: precision;
  code: M2l.t;
  signature: Module.signature;
  dependencies: Deps.t
}
type u = r

(** Conversion function between input and output types *)
val lift: Module.signature -> Deps.t -> s -> r
val proj: r -> s

val read_file : Fault.Policy.t -> Read.kind -> (Pth.t * string) -> s
(** [read_file polycy kind (filename,pth)] reads the file [filename],
    extracting the corresponding m2l ast. If the file is not 
    synctatically valid Ocaml and syntax errors are not set to 
    critical level in [polycy], the approximative parser is used.
*)

(** Pretty-printing function *)
val pp : Format.formatter -> r -> unit
val pp_input : Format.formatter -> s -> unit

(** Pair of implementation/interface units *)
type 'a pair = { ml : 'a; mli : 'a; }
val map: ('a -> 'b) pair -> 'a pair -> 'b pair
val unimap: ('a -> 'b) -> 'a pair -> 'b pair
val adder:  ('a->'b->'b) -> 'b pair -> M2l.kind * 'a -> 'b pair

(** {!group} handles pair of ml/mli files together *)
module type group =
sig
  type elt
  type set
  type ('a,'b) arrow
  type t = set pair
  type group = t

  val add_mli : elt -> group -> group
  val add_ml : elt -> group -> group
  val add : (M2l.kind, elt -> group -> group) arrow
  val empty : group
  module Map :
  sig
    type t = group Pth.map
    val find: Pth.t -> t -> group
    val add : (M2l.kind , elt -> t -> t) arrow
    val of_list : (M2l.kind, elt list -> t) arrow
  end

  val group : elt list pair -> group Pth.map
  val flatten: group -> elt option pair * elt list pair
  val split : group Pth.map -> elt list pair * (Paths.S.t * elt list) list

end

module type group_core= sig
  type elt
  type ('a,'b) arrow
  val lift: ( (elt ->M2l.kind) -> 'c ) -> (M2l.kind, 'c) arrow
  val key: elt -> Pth.t
end

module Groups: sig

  module Make(Base: group_core):
    group with type elt = Base.elt
           and type ('a,'b) arrow = ('a,'b) Base.arrow

    module Filename: group with
    type elt = string and type ('a,'b) arrow = 'a -> 'b

  module Unit: group with
    type elt = s and type ('a,'b) arrow = 'b

  module R: group with
    type elt = r and type ('a,'b) arrow = 'b

end

(** Unit sets *)
module Set : Set.S with type elt = u
