
(** Arg submodule for functor arguments *)
module Arg :
  sig
    type 'a t = { name : string; signature : 'a; }
    type 'a arg = 'a t

    val pp :
      (Format.formatter -> 'a -> unit) ->
      Format.formatter -> 'a arg option -> unit

    (** print with ocaml syntax *)
    val reflect :
      (Format.formatter -> 'a -> unit) ->
      Format.formatter -> 'a arg option -> unit

    val sexp: ('a, 'b) Sexp.impl -> ('a t,Sexp.many) Sexp.impl

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

  type t = { root: Name.t; origin:origin; loc: Paths.Pkg.t * Loc.t}

  val pp: Format.formatter -> t -> unit

end


(** Module origin *)
module Origin: sig
  type t =
    | Unit of {source:Paths.Pkg.t; path:Paths.S.t}
    (** toplevel module mapped from a unit file *)
    | Submodule (** non top-level module *)
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
  val sexp: (t,Sexp.one_and_many) Sexp.impl

end
type origin = Origin.t

(** Main module type *)
type m = {
  name : string;
  origin : Origin.t;
  args : m option list;
  signature : signature;
}

(** Core module or alias *)
and t =
  | M of m (** Classic module *)
  | Alias of
      {
        name:Name.t; (** module Name = … *)
        path: Namespaced.t; (** … = Path.To.Target *)
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
        weak:bool (** Weak alias are used as placehoder in namespace *)
      }
  | Namespace of {name: Name.t; modules:dict}
  (** Namespace support: Namespace are bundles of modules, used for
      packs or as an higher-level views of the alias atlas idiom *)

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

type arg = definition Arg.t
type level = Module | Module_type
type modul_ = t

(** {2 Helper function} *)
val is_exact: t -> bool
val of_arg : arg -> m
val is_functor : t -> bool
val name: t -> Name.t

module Dict: sig
  type t = dict
  val empty: t
  val of_list: modul_ list -> t
  val union: t -> t -> t
end

val spirit_away: Divergence.t -> t -> t
(** transform to a ghost module *)

val md: m -> t

val empty : 'a Name.map
val create :
  ?args:m option list ->
  ?origin:origin -> Name.t -> signature -> m

val with_namespace: Paths.S.t -> t -> t
val namespace: Namespaced.t -> t


val aliases: t -> Namespaced.t list

(** Create a mockup module with empty signature *)
val mockup: ?origin:Origin.t -> ?path:Paths.P.t -> Name.t -> m

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
val pp_arg : Format.formatter -> m option -> unit
val pp_args : Format.formatter -> m option list -> unit

(** {2 Sexp} *)
val sexp: (modul_,Sexp.one_and_many) Sexp.impl

(** Helper functions for definitions *)
module Def: sig
  val empty : definition
  val modules: dict -> definition

  val add : definition -> t ->  definition
  val add_type : definition -> t -> definition
  val add_gen : level -> definition -> t -> definition

  val merge: definition -> definition -> definition

  val pp: Format.formatter -> definition -> unit
  val sexp: (definition,Sexp.many) Sexp.impl
  type t = definition
end

(** Helper functions for signature *)
module Sig :
  sig
    val card : signature -> int
    val merge : signature -> signature -> signature
    val flatten: signature -> definition
    val is_exact: signature -> bool

    val create : modul_ -> signature
    val create_type : t -> signature
    val gen_create : level -> t -> signature

    val of_lists: t list -> t list -> signature
    val of_list : t list -> signature
    val of_list_type : t list -> signature

    val add : signature -> t -> signature
    val add_type : signature -> t -> signature
    val add_gen : level -> signature -> t -> signature
    val empty : signature

    val pp : Format.formatter -> signature -> unit
    val sexp: (signature,Sexp.many) Sexp.impl

    type t = signature
  end

(** Anonymous module and other partial definitions *)
module Partial :
  sig
    type nonrec t = {
      origin : origin;
      args : m option list;
      result : signature;
    }
    val empty : t
    val is_exact: t -> bool

    val simple : signature -> t

    val pp : Format.formatter -> t -> unit
    val sexp: (t,Sexp.many) Sexp.impl


    val no_arg : signature -> t
    val drop_arg : t -> t option

    val to_module : ?origin:origin -> string -> t -> m
    val to_arg : string -> t -> m
    val of_module : m -> t
    val is_functor : t -> bool
    val to_sign : t -> (signature,signature) result

  end
