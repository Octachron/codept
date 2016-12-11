
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


    val pp_s :
      (Format.formatter -> 'a -> unit) ->
      Format.formatter -> 'a arg option list -> unit
  end

module Precision:sig
  type t =
    | Exact
    | Unknown
end


(** Module origin *)
module Origin: sig
  type t =
    | Unit of Paths.Pkg.t (** toplevel module mapped from a unit file *)
    | Alias of t (** [Alias t]: alias to [t] *)
    | Submodule (** non top-level module *)
    | First_class (** unpacked first-class module *)
    | Arg (** module created for functor application *)

  val at_most : t -> t -> t
  (** [at_most origin origin'] cap the origin [origin'] at
      level origin according to the partial order:
       First_class, Rec, Arg, Extern, Alias ≺ Submodule ≺ Unit
  *)

  val pp : Format.formatter -> t -> unit
  val reflect : Format.formatter -> t -> unit
end
type origin = Origin.t

(** Main module type *)
type t = {
  name : string;
  precision: Precision.t;
  origin : Origin.t;
  args : t option list;
  signature : signature;
}

and signature = { modules : mdict; module_types : mdict; }
and mdict = t Name.map

type arg = signature Arg.t
type level = Module | Module_type
type modul = t

(** {2 Helper function} *)
val of_arg : ?precision:Precision.t -> arg -> t
val is_functor : t -> bool
val empty : 'a Name.map
val create :
  ?args:t option list -> ?precision:Precision.t ->
  ?origin:origin -> Name.t -> signature -> t


(** {2 Printers} *)

val pp : Format.formatter -> t -> unit
val reflect : Format.formatter -> t -> unit
val persistent: Format.formatter -> t -> unit

val pp_signature : Format.formatter -> signature -> unit
val reflect_signature : Format.formatter -> signature -> unit



val pp_alias : Format.formatter -> Paths.Expr.t option -> unit
val pp_level : Format.formatter -> level -> unit
val pp_mdict : Format.formatter -> mdict -> unit
val pp_pair : Format.formatter -> string * t -> unit
val pp_arg : Format.formatter -> t option -> unit
val pp_args : Format.formatter -> t option list -> unit

(** Helper functions for signature *)
module Sig :
  sig
    val card : signature -> int
    val persistent: Format.formatter -> signature -> unit
    val merge : signature -> signature -> signature
    val create : modul -> signature
    val create_type : t -> signature
    val gen_create : level -> t -> signature
    val of_list : t list -> signature
    val of_list_type : t list -> signature
    val add : signature -> t -> signature
    val add_type : signature -> t -> signature
    val add_gen : level -> signature -> t -> signature
    val empty : signature
    val pp : Format.formatter -> signature -> unit
    type t = signature
  end

(** Anonymous module and other partial definitions *)
module Partial :
  sig
    type nonrec t = {
      precision: Precision.t;
      origin : origin;
      args : t option list;
      result : signature;
    }
    val empty : t
    val simple : signature -> t
    val pp : Format.formatter -> t -> unit
    val no_arg : signature -> t
    val drop_arg : t -> t option
    val to_module : ?origin:origin -> string -> t -> modul
    val to_arg : string -> t -> modul
    val of_module : modul -> t
    val is_functor : t -> bool
    val to_sign : t -> (signature,signature) result
  end
