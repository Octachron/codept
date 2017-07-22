(** Schema for exported data *)

type (_,_) eq = Eq : ('a,'a) eq
type void = (int,float) eq

(** Reexport the standard list module *)
module L: sig
  type 'a t = 'a list =
    | [  ]
    | (::) of 'a * 'a t
end

module Tuple: sig
  type 'a tuple =
    | []: void tuple
    | (::): 'a * 'b tuple -> ('a * 'b) tuple
  type 'a t = 'a tuple
end

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

module Record: sig
  type 'a record =
    | []: void record
    | (::): ( 'a name * ('m,'b) elt) * 'c record ->
      ('m * 'a * 'b * 'c) record
  type 'a t = 'a record
end

type ('a,'b) bijection = { fwd:'a->'b;rev:'b -> 'a}

type 'hole t =
  | Float: float t
  | Int: int t
  | Bool: bool t
  | String: string t
  | Void: void t
  | Array: 'hole t -> 'hole list t
  | (::) : 'a t * 'b Tuple.t t -> ('a * 'b) Tuple.t t
  | []: void Tuple.t t
  | Obj:'a record_declaration -> 'a Record.t t
  | Custom: ('a,'b) custom -> 'a t
  | Sum: 'a sum_decl -> 'a sum t

and ('a,'b) custom = { fwd:'a -> 'b; rev:'b -> 'a; sch:'b t; recs: bool}
and 'a record_declaration =
  | []: void record_declaration
  | (::): ( 'm modal * 'a name * 'b t) * 'c record_declaration
    -> (  'm * 'a * 'b * 'c ) record_declaration

and 'a sum_decl =
    | [] : void sum_decl
    | (::): 'a t * 'b sum_decl -> ('a * 'b) sum_decl

and (_,_) cons =
  | Z: 'a -> ('a * 'any,'a) cons
  | E: (void * 'any,'a) cons
  | S: ('a, 'n ) cons -> ('any * 'a, 'n) cons

and 'a sum = C: ('a, 'elt ) cons -> 'a sum

type ('m,'a,'b) field = 'a name * ('m, 'b) elt

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

val obj: 'a Record.t -> 'a Record.t
val custom: ?recs:bool -> 'b t -> ('a -> 'b) -> ('b -> 'a) -> 'a t

module Untyped: sig
  type t =
    | Array of t list
    | Array_or_obj of t list
    | Atom of string
    | Obj of (string * t) list
end

val retype: 'a t -> Untyped.t -> 'a option
