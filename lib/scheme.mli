(** Schema for exported data *)

type void = private Void

(** Reexport the standard list module *)
module L: sig
  type 'a t = 'a list =
    | [  ]
    | (::) of 'a * 'a t
end

type 'a tuple =
  | []: void tuple
  | (::): 'a * 'b tuple -> ('a * 'b) tuple

module type name = sig type t val s:string end
module Name: functor(X:sig val s:string end) -> name

type 'a name = (module name with type t = 'a)
val show: 'a name -> string

type required = private Required
type optional = private Optional
type _ modal =
  | Opt:optional modal
  | Req:required modal

type ('m,'a) elt =
  | Just: 'a -> ('any,'a) elt
  | Nothing: (optional,'any) elt

type 'hole t =
  | Float: float t
  | Int: int t
  | String: string t
  | Array: 'hole t -> 'hole list t
  | (::) : 'a t * 'b tuple t -> ('a * 'b) tuple t
  | []: void tuple t
  | Obj:'a record_declaration -> 'a record t

and 'a record_declaration =
  | []: void record_declaration
  | (::): ( 'm modal * 'a name * 'b t) * 'c record_declaration
    -> (  'm * 'a * 'b * 'c ) record_declaration

and 'a record =
  | []: void record
  | (::): ( 'a name * ('m,'b) elt) * 'c record ->
    ('m * 'a * 'b * 'c) record
and ('m,'a,'b) field = 'a name * ('m, 'b) elt

type 'a s = {
  title: string;
  description: string;
  sch: 'a t;
}

val json: 'a s -> Format.formatter -> 'a -> unit
val sexp: 'a s  -> Format.formatter -> 'a -> unit
val json_schema:  Format.formatter -> 'a s -> unit

val ($=): 'a name -> 'b -> ('any,'a,'b) field
val skip: 'a name -> (optional,'a,'any) field
val ($=?): 'a name -> 'b option -> (optional,'a,'b) field

val obj: 'a record -> 'a record
