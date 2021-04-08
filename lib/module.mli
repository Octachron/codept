
(** Arg submodule for functor arguments *)
module Arg :
  sig
    type 'a t = { name : Name.t option; signature : 'a; }
    type 'a arg = 'a t

    val map: ('a->'b) -> 'a arg -> 'b arg

    val pp :
      (Format.formatter -> 'a -> unit) ->
      Format.formatter -> 'a arg option -> unit

    (** print with ocaml syntax *)
    val reflect :
      (Format.formatter -> 'a -> unit) ->
      Format.formatter -> 'a arg option -> unit

    val sch: 'a Schematic.t -> 'a t Schematic.t

    val pp_s :
      (Format.formatter -> 'a -> unit) ->
      Format.formatter -> 'a arg option list -> unit
  end

(** Every time a module with an unknown signature is opened,
    it can possibly shadows all modules present in the current,
    or none. The {!Divergence} module is use to store information
    about such divergence point, in order to be able to pinpoint
    to the user the exact moment where dependency computation might have
    gone awry. *)
module Divergence: sig

  (** The source of the divergence can be either a [First_class_module] or
      an [External] module. *)
  type origin =
    | First_class_module
    | External

  type t = { root: Name.t option;
             origin:origin;
             loc: Paths.Pkg.t * Loc.t
           }

  val pp: Format.formatter -> t -> unit

end


(** Module origin *)
module Origin: sig
  type t =
    | Unit of {source:Paths.Pkg.t; path:Paths.S.t}
    (** toplevel module mapped from a unit file *)
    | Submodule (** non top-level module *)
    | Namespace (** temporary module from namespace *)
    | First_class (** unpacked first-class module *)
    | Arg (** module created for functor application *)
    | Phantom of bool * Divergence.t
    (** Module with an ambiguous signature due to a divergence.
        In particular, its submodule structure should not be trusted
        and is kept around only to handle delayed alias dependencies
    *)

  val at_most : t -> t -> t
  (** [at_most origin origin'] cap the origin [origin'] at
      level origin according to the partial order:
       First_class, Rec, Arg, Extern, Alias ≺ Submodule ≺ Unit
  *)

  val pp : Format.formatter -> t -> unit
  val reflect : Format.formatter -> t -> unit
  val sch: t Schematic.t
end
type origin = Origin.t


(** Type-level tags *)

type extended = private Extended
type simple = private Simple

(** Signature with tracked origin *)
type tracked_signature = {
  origin : Origin.t;
  signature : signature;
}


(** Core module or alias *)
and _ ty =
  | Sig: tracked_signature -> 'any ty (** Classic module *)
  | Alias:
      {
        path: Namespaced.t;
        (** Path.To.Target:
            projecting this path may create new dependencies
        *)
        phantom: Divergence.t option;
        (** Track potential delayed dependencies
            after divergent accident:
            [
              module M = A  (* Alias { name = M; path = [A] } *)
              open Unknownable (* <- divergence *)
              open M (* Alias{ phantom = Some divergence } *)
            ]
            In the example above, [M] could be the local module
            [.M], triggering the delayed alias dependency [A]. Or it could
            be a submodule [Unknownable.M] . Without sufficient information,
            codept defaults to computing an upper bound of dependencies,
            and therefore considers that [M] is [.M], and the inferred
            dependencies for the above code snipet is {A,Unknowable} .
        *)
      } -> extended ty

  | Abstract: Id.t -> 'any ty
  (** Abstract module type may be refined during functor application,
      keeping track of their identity is thus important
  *)

  | Fun: 'a ty Arg.t option * 'a ty -> 'a ty

  | Link: Namespaced.t -> extended ty (** Link to a compilation unit *)
  | Namespace: dict -> extended ty
  (** Namespace are open bundle of modules *)

and t = extended ty

and definition = { modules : dict; module_types : dict }
and signature =
  | Blank (** Unknown signature, used as for extern module, placeholder, … *)
  | Exact of definition
  | Divergence of { point: Divergence.t; before:signature; after:definition}
  (** A divergent signature happens when a signature inference is disturbed
      by opening or including an unknowable module:
      [ module A = …
         include Extern (* <- divergence *)
        module B = A (* <- which A is this: .A or Extern.A ?*)
      ]
  *)

and dict = t Name.map

type sty = simple ty
type named = Name.t * t

type level = Module | Module_type
type modul_ = t

(** {2 Helper function} *)
val is_exact: t -> bool
val is_functor : t -> bool

module Dict: sig
  type t = dict
  val empty: t
  val of_list: named list -> t
  val union: t -> t -> t
  val weak_union: t -> t -> t
end

val spirit_away: Divergence.t -> t -> t
(** transform to a ghost module *)

val md: tracked_signature -> t

val empty : 'a Name.map
val create :
  ?origin:origin -> signature -> tracked_signature

val with_namespace: Paths.S.t -> Name.t -> t -> named
val namespace: Namespaced.t -> named


val aliases: t -> Namespaced.t list

(** Create a mockup module with empty signature *)
val mockup: ?origin:Origin.t -> ?path:Paths.P.t -> Name.t -> tracked_signature

(** {2 Printers} *)

val pp : Format.formatter -> t -> unit
val reflect : Format.formatter -> t -> unit

val pp_signature : Format.formatter -> signature -> unit
val reflect_signature : Format.formatter -> signature -> unit
val reflect_modules:  Format.formatter -> dict -> unit


val pp_alias : Format.formatter -> Paths.Expr.t option -> unit
val pp_level : Format.formatter -> level -> unit
val pp_mdict : Format.formatter -> dict -> unit
val pp_pair : Format.formatter -> string * t -> unit
val pp_arg : Format.formatter -> t Arg.t -> unit

(** {2 Schematic } *)
module Schema: sig
  val module': modul_ Schematic.t
  val m: tracked_signature Schematic.t
end

(** Helper functions for definitions *)
module Def: sig
  val empty : definition
  val modules: dict -> definition

  val add : definition -> string * t ->  definition
  val add_type : definition -> string * t -> definition
  val add_gen : level -> definition -> string * t -> definition

  val merge: definition -> definition -> definition

  (** [weak_merge x y] never redefines values in [x] *)
  val weak_merge: definition -> definition -> definition

  val pp: Format.formatter -> definition -> unit
  val sch: definition Schematic.t
  type t = definition
end

(** Helper functions for signature *)
module Sig :
  sig
    val card : signature -> int
    val merge : signature -> signature -> signature

    (** [weak_merge x y] never redefines values in [x] *)
    val weak_merge: signature -> signature -> signature


    val diff: signature -> signature -> signature

    val flatten: signature -> definition
    val is_exact: signature -> bool

    val create : named -> signature
    val create_type : named -> signature
    val gen_create : level -> named -> signature

    val of_lists: named list -> named list -> signature
    val of_list : named list -> signature
    val of_list_type : named list -> signature

    val add : signature -> named -> signature
    val add_type : signature -> named -> signature
    val add_gen : level -> signature -> named -> signature
    val empty : signature

    val pp : Format.formatter -> signature -> unit
    val sch: signature Schematic.t

    type t = signature
  end


(** Anonymous module and other partial definitions *)
module Partial :
  sig


  type t = { name: string option; mty: sty }

    val empty : t
    val is_exact: t -> bool

    val extend: sty -> modul_
    val simple : signature -> t

    val pp : Format.formatter -> t -> unit
    val pp_sty : Format.formatter -> sty -> unit
    val sch: t Schematic.t

    val refresh: Id.seed -> 'any ty -> 'any ty
    val apply :  arg:modul_ -> param:modul_ -> body:modul_ -> modul_

    val to_module : ?origin:origin -> t -> modul_
    val to_arg : t -> modul_

    val of_extended_mty : modul_ -> sty
    val of_extended: ?name:Name.t -> modul_ -> t

    val of_module : Name.t -> tracked_signature -> t
    val pseudo_module: Name.t -> dict -> t
    val is_functor : t -> bool
    val to_sign : t -> (signature,signature) result



  end
