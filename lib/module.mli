
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

module Divergence: sig
  type kind =
    | Open
    | Include
  type origin =
    | First_class_module
    | External
  type t = kind * origin * (Paths.Pkg.t * Loc.t)
  val pp: Format.formatter -> t -> unit
end


(** Module origin *)
module Origin: sig
  type t =
    | Unit of Paths.Pkg.t (** toplevel module mapped from a unit file *)
    | Submodule (** non top-level module *)
    | First_class (** unpacked first-class module *)
    | Arg (** module created for functor application *)
    | Phantom of Divergence.t (** ambiguous module that could be either
                  an internal module or an external module *)

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
  | M of m
  | Alias of { name:Name.t; path: Paths.S.t; phantom: Divergence.t option }

and definition = { modules : mdict; module_types : mdict }
and signature =
  | Blank
  | Exact of definition
  | Divergence of { point: Divergence.t; before:signature; after:definition}
and mdict = t Name.map

type arg = definition Arg.t
type level = Module | Module_type
type modul_ = t

(** {2 Helper function} *)
val is_exact: t -> bool
val of_arg : arg -> m
val is_functor : t -> bool
val name: t -> Name.t

val spirit_away: Divergence.t -> t -> t
(** transform to a ghost module *)

val md: m -> t

val empty : 'a Name.map
val create :
  ?args:m option list ->
  ?origin:origin -> Name.t -> signature -> m

val aliases: t -> Name.t list

(** Create a mockup module with empty signature *)
val mockup: ?origin:Origin.t -> ?path:Paths.P.t -> Name.t -> m

(** {2 Printers} *)

val pp : Format.formatter -> t -> unit
val reflect : Format.formatter -> t -> unit

val pp_signature : Format.formatter -> signature -> unit
val reflect_signature : Format.formatter -> signature -> unit



val pp_alias : Format.formatter -> Paths.Expr.t option -> unit
val pp_level : Format.formatter -> level -> unit
val pp_mdict : Format.formatter -> mdict -> unit
val pp_pair : Format.formatter -> string * t -> unit
val pp_arg : Format.formatter -> m option -> unit
val pp_args : Format.formatter -> m option list -> unit

(** {2 Sexp} *)
val sexp: (modul_,Sexp.one_and_many) Sexp.impl

(** Helper functions for definitions *)
module Def: sig

  val empty : definition

  val add : definition -> t ->  definition
  val add_type : definition -> t -> definition
  val add_gen : level -> definition -> t -> definition

  val merge: definition -> definition -> definition

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
