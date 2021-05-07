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
      val sch: t Schematic.t
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
module type Expr = sig
  type s
  type t = private
    | Simple of s
    | Apply of { f:t; x:t; proj: Simple.t option }

  val sch: t Schematic.t
  exception Functor_not_expected
  val concrete : t -> s
  val concrete_with_f : t -> s
  val multiples : t -> s list
  val pure : s -> t
  val app: t -> t -> Simple.t option -> t
  val pp : Format.formatter -> t -> unit
  val prefix : t -> string

  module Map: Map.S with type key = t
  type 'a map = 'a Map.t

end

module type simple_core = sig
  type t
  val concat: t -> Simple.t -> t
  val prefix: t -> Name.t
  val sch: t Schematic.t
  val pp: t Pp.t
end

module Make_expr: functor(S:simple_core) -> Expr with type s := S.t
module Expr: Expr with type s := S.t
module E = Expr
