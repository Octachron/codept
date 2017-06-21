(** Basic cache for codept server prototype *)

module Findmap : Map.S with type key = Findlib.query

type t = {
  env: Envt.Core.t;
  signatures: Module.t list Name.map;
  m2l: Unit.s Name.map;
  findlib: ( (Common.task -> Common.task) * (unit -> unit) ) Findmap.t
}

val empty: t

module Shared: sig
  type 'a t
  type 'a shared = 'a t

  val make: 'a -> 'a shared
  val get: 'a shared -> 'a
  val set: 'a shared -> 'a -> unit
  val map: ('a -> 'a) -> 'a shared -> unit
end
