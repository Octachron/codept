(** Various path implementation *)

(** Path as a list of name *)
module Simple :
  sig
    type t = Name.t list
    val pp : Format.formatter -> string list -> unit
    val sch: t Schematic.t
    module Set : sig
      include Set.S with type elt = t
      val pp: Format.formatter -> t -> unit
      end
    module Map : sig
      include Map.S with type key = t
      val find_opt: key -> 'a t -> 'a option
      val union': 'a t -> 'a t -> 'a t
    end
    type set = Set.t
    type 'a map = 'a Map.t
    val prefix : 'a list -> 'a
    val module_name: t -> Name.t

      (** {2 Extension and parsing} *)
    val change_file_extension : (string -> string) -> t -> t
    val chop_extension: t -> t
    val parse_filename : string -> t
  end
module S = Simple

(** Module paths with application *)
module Expr :
  sig
    type t = T | A of string | S of t * string | F of { f : t; x : t; }
    val sch: t Schematic.t
    exception Functor_not_expected
    val concrete : t -> Simple.t
    val concrete_with_f : t -> Simple.t
    val multiples : t -> Simple.t list
    val rev_concrete : t -> string list
    val from_list : string list -> t
    val pp : Format.formatter -> t -> unit
    val prefix : t -> string
  end
module E = Expr


(** Localized path for package *)
module Pkg :
  sig
    type source = Local | Unknown | Pkg of Simple.t | Special of Name.t
    type t = { source : source; file : Simple.t; }
    type path = t
    val sch: t Schematic.t

    (** {2 Printing function } *)
    val pp_source : Format.formatter -> source -> unit
    val pp_simple : Format.formatter -> path -> unit
    val pp_gen : string -> Format.formatter -> path -> unit
    val pp : Format.formatter -> path -> unit
    val reflect: Format.formatter -> path -> unit

    val filename : ?sep:string -> t -> string

    val local : string -> path
    val (/): S.t -> t -> t

    val is_known : t -> bool
    val module_name : path -> string

    (** {2 Extension handling } *)
    val update_extension : (string -> string) -> path -> path
    val change_extension : string -> path -> path

    val cmo : path -> path
    val o : path -> path
    val cmi : path -> path
    val cmx : path -> path
    val cmxs : path -> path


    val mk_dep : bool -> bool -> path -> path
    (** [mk_dep all native path] generates either
        a cmi, a cmo or a cmx depending on the extension of path*)

    module Set : sig
      include Set.S with type elt = t
      val pp: Format.formatter -> t -> unit
      end
    type set = Set.t

    module Map : sig
      include Map.S with type key = t
      val find_opt: key -> 'a t -> 'a option
    end
    type 'a map = 'a Map.t

  end
module P = Pkg
