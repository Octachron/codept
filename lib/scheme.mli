(** Schema for exported data *)

type format = Json | Sexp

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
type 'a name = (module name with type t = 'a)
module Name: functor(X:sig val s:string end) ->
  sig type t val x: t name end

val show: 'a name -> string

type required = private Required
type optional = private Optional

type (_,_,_) modal =
  | Opt:(optional,'a,'a option) modal
  | Req:(required,'a,'a) modal

module Record: sig
  type 'a record =
    | []: void record
    | (::): ( 'a name * 'elt) * 'c record ->
      ('a * 'elt * 'c) record
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

and ('a,'b) custom = { fwd:'a -> 'b; rev:'b -> 'a; sch:'b t; id: string}
and 'a record_declaration =
  | []: void record_declaration
  | (::):  ( ('m,'x,'fx) modal * 'a name * 'x t) * 'c record_declaration
    -> (  'a * 'fx * 'c ) record_declaration

and 'a sum_decl =
    | [] : void sum_decl
    | (::): (string * 'a t) * 'b sum_decl -> ('a * 'b) sum_decl

and (_,_) cons =
  | Z: 'a -> ('a * 'any,'a) cons
  | E: (void * 'any,'a) cons
  | S: ('a, 'n ) cons -> ('any * 'a, 'n) cons

and 'a sum = C: ('a, 'elt ) cons -> 'a sum

type 'a s = {
  title: string;
  description: string;
  sch: 'a t;
}

val json: 'a s -> Format.formatter -> 'a -> unit
val sexp: 'a s  -> Format.formatter -> 'a -> unit
val json_schema:  Format.formatter -> 'a s -> unit

val ($=): 'a name -> 'b -> ('a name * 'b)
val skip: 'a name -> ('a name * 'b option)
val ($=?): 'a name -> 'b option -> ('a name * 'b option)

val obj: 'a Record.t -> 'a Record.t
val custom: string -> 'b t -> ('a -> 'b) -> ('b -> 'a) -> 'a t

module Untyped: sig
  type t =
    | Array of t list
    | List of t list
    | Atom of string
    | Obj of (string * t) list
end

val retype: 'a t -> Untyped.t -> 'a option

val minify: Format.formatter -> ('a, Format.formatter, unit, unit) format4 -> 'a
