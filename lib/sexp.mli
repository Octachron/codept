(** Sexp provides combinators for sexp parsing/serialization *)

(** {2 Ordinal types } *)

type one = private One
type n = private N
type z = private Z

type atomic = z * one
type many = z * n
type one_and_many = one * n

(** Sexp description *)
type 'kind kind =
  | Atomic: atomic kind
  | Many: many kind
  | One_and_many: one_and_many kind

(** Main type *)
type 'kind t =
  | Atom: string -> atomic t
  | List: any list -> many t
  | Keyed_list: string * any list -> one_and_many t
and any = Any: 'any t -> any

(** {2 Basic functions } *)

val pp: Format.formatter -> 'any t -> unit
val pp_any: Format.formatter -> any -> unit

(** {2 Combinators } *)

type ('a,'b) impl = {
  embed: 'a -> 'b t;
  parse: 'b t -> 'a option;
  kind: 'b kind
}

val parse: ('a,'b) impl -> 'b t -> 'a option
val embed: ('a,'b) impl -> 'a -> 'b t
val any_parse: ('a,'b) impl -> any -> 'a option

(** {2 Record combinators } *)
module Record: sig
  (** Universal map and keys  *)
  type ('a,'b) key
  val key: 'k kind -> string -> 'a -> ('a,'k) key
  val name: ('a,'b) key -> string
  val default: ('a,'b) key -> 'a

  type t
  val find_opt: ('a,'b) key -> t -> 'a option
  val get: ('a,'b) key -> t -> 'a
  val add: ('a,'b) key -> 'a -> t -> t

  type field_declaration
  val field: ('a,'b) key -> ('a,'b) impl -> field_declaration
  val define: field_declaration list -> (t, many) impl

  type field_value
  val create: field_value list -> t
  val let_: ('a,'b) key -> 'a -> field_value
  val (:=): ('a,'b) key -> 'a -> field_value
end
val record: Record.field_declaration list -> (Record.t, many) impl

(** {2 Standard combinators } *)
type ('a,'b) bij = { f: 'a -> 'b; fr:'b -> 'a }
val id: ('a,'a) bij

val int: (int,atomic) impl
val bool: (bool,atomic) impl

val string: (string,atomic) impl
val unit: (unit,many) impl

val atomic: ('a -> string) -> (string -> 'a option) -> ('a,atomic) impl
val list: ('a,'b) impl -> ('a list, many) impl
val opt: ('a,'b) impl -> ('a option, many) impl
val singleton: ('a,'b) impl -> ('a list, many) impl
val keyed_list: string -> ('a,'b) impl -> ('a list, many) impl

val pair: ('a,'k) impl -> ('b,'k2) impl -> ('a * 'b, many) impl
val triple: ('a,'k) impl -> ('b,'k2) impl -> ('c, 'k3) impl ->
  ('a * 'b * 'c, many) impl
val tetra: ('a,'k) impl -> ('b,'k2) impl -> ('c, 'k3) impl -> ('d,'k4) impl
  -> ('a * 'b * 'c * ' d, many) impl


val pair': ('a,'k) impl -> ('b,many) impl -> ('a * 'b, many) impl
val key_list: ('a,atomic) impl -> ('b,many) impl -> ('a * 'b, one_and_many) impl

val triple': ('a,atomic) impl -> ('b,'k2) impl -> ('c, 'k3) impl ->
  ('a * 'b * 'c, one_and_many) impl
val tetra': ('a, atomic) impl -> ('b,'k2) impl -> ('c, 'k3) impl -> ('d,'k4) impl
  -> ('a * 'b * 'c * ' d, one_and_many) impl


val major_minor: ('a,'k) impl -> 'b -> ('b,'k2) impl -> ('a * 'b, many) impl

val cons: ('a,'k) impl -> ('a list,many) impl -> ('a list, many) impl

val conv: ('a,'b) bij -> ('a,'k) impl -> ('b,'k) impl
val convr: ('a,'k) impl -> ('a -> 'b) -> ('b -> 'a) -> ('b,'k) impl

val fix0: 'k kind -> (unit -> ('a,'k) impl) -> ('a,'k) impl
val fix: (unit -> ('a,many) impl) -> ('a,many) impl
val fix': (unit -> ('a, one_and_many) impl) -> ('a, one_and_many) impl


(** {2 Sum types constructors } *)

(** Constructor type with default values *)
type 'a constr =
  | C: {
      name:Name.t;
      proj:'a -> 'b option;
      inj: 'b -> 'a;
      impl: ('b, 'kind) impl;
      default: 'b option
    } -> 'a constr
  | Cs : { name:Name.t; value:'a} -> 'a constr

val sum: 'a constr list -> ('a, one_and_many) impl
val simple_constr: string -> 'a -> 'a constr

module C2: sig
  (** Alternative sum type with default constructors *)
  type 'a constr =
    | C: {
        name:Name.t;
        proj:'a -> 'b option;
        inj: 'b -> 'a;
        impl: ('b, one_and_many) impl;
      } -> 'a constr

  val sum: 'a constr -> 'a constr list -> ('a, one_and_many) impl
end
