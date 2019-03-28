(** Functions for handling unit (aka .ml/.mli) files *)

(** Module abbreviations *)
module Pkg = Paths.Pkg
module Pth = Paths.Simple

(** Precision of the inferred module-level signature *)
type precision =
  | Exact
  | Approx

(** Base type *)
type 'ext t = {
  path: Namespaced.t; (** module path of the compilation unit *)
  src: Pkg.t; (** source file of the compilation unit *)
  kind: M2l.kind;
  precision: precision;
  code: M2l.t;
  more: 'ext
}

(** Extension for output type *)
type ext = {
  signature: Module.signature;
  dependencies: Deps.t
}

type 'ext base = 'ext t
type s = unit t
type u = ext t
type r = u

val signature: r -> Module.signature
val deps: r -> Deps.t
val update: Deps.t -> r -> r

(** Conversion function between input and output types *)
val lift: Module.signature -> Deps.t -> s -> u
val proj: u -> s


val read_file :
     Fault.Policy.t
  -> Read.kind
  -> string
  -> Namespaced.t
  -> s
(** [read_file polycy kind filename pth] reads the file [filename],
    extracting the corresponding m2l ast and associating it to the
    path [pth]. If the file is not  synctatically valid Ocaml and
    syntax errors are not set to critical level in [polycy], the
    approximative parser is used.
*)

(** Pretty-printing function *)
val pp : Format.formatter -> u -> unit
val pp_input : Format.formatter -> s -> unit

(** Pair of implementation/interface units *)
type 'a pair = { ml : 'a; mli : 'a; }
val map: ('a -> 'b) pair -> 'a pair -> 'b pair
val unimap: ('a -> 'b) -> 'a pair -> 'b pair
val adder:  ('a->'b->'b) -> 'b pair -> M2l.kind * 'a -> 'b pair

(** {!group} handles pair of ml/mli files together *)
module Group: sig
  type 'ext group

  val add_mli : 'ext t -> 'ext group -> 'ext group
  val add_ml : 'ext t -> 'ext group -> 'ext group
  val add : 'ext t -> 'ext group -> 'ext group
  val empty : 'any group
  module Map :
  sig
    type 'ext t = 'ext group Pth.map
    val find: Pth.t -> 'ext t -> 'ext group
    val add : 'ext base -> 'ext t -> 'ext t
    val of_list : 'ext base list -> 'ext t
  end

  val group : 'ext t list pair -> 'ext Map.t
  val flatten: 'ext group -> 'ext t option pair * 'ext t list pair
  val split : 'ext Map.t -> 'ext t list pair * (Paths.S.t * 'ext t list) list

end

(** Unit sets *)
module Set : Set.S with type elt = u
