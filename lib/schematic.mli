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

module type label = sig type t val l:string end
type 'a label = (module label with type t = 'a)
module Label: functor(X:sig val l:string end) ->
  sig type t val l: t label end

val show: 'a label -> string

type required = private Required
type optional = private Optional

type (_,_,_) modal =
  | Opt:(optional,'a,'a option) modal
  | Req:(required,'a,'a) modal

module Record: sig
  type 'a record =
    | []: void record
    | (::): ( 'a label * 'elt) * 'c record ->
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

and ('a,'b) custom = { fwd:'a -> 'b; rev:'b -> 'a; sch:'b t; id: string list}
and 'a record_declaration =
  | []: void record_declaration
  | (::):  ( ('m,'x,'fx) modal * 'a label * 'x t) * 'c record_declaration
    -> (  'a * 'fx * 'c ) record_declaration

and 'a sum_decl =
    | [] : void sum_decl
    | (::): (string * 'a t) * 'b sum_decl -> ('a * 'b) sum_decl

and (_,_) cons =
  | Z: 'a -> ('a * 'any,'a) cons
  | E: (void * 'any,'a) cons
  | S: ('a, 'n ) cons -> ('any * 'a, 'n) cons

and 'a sum = C: ('a, 'elt ) cons -> 'a sum

type 'a schematic = 'a t

module Version: sig
  type lbl
  type t = { major:int; minor:int; patch:int }
  val sch: t schematic
end


val json: 'a t -> Format.formatter -> 'a -> unit
val sexp: 'a t  -> Format.formatter -> 'a -> unit

val ($=): 'a label -> 'b -> ('a label * 'b)
val skip: 'a label -> ('a label * 'b option)
val ($=?): 'a label -> 'b option -> ('a label * 'b option)

val obj: 'a Record.t -> 'a Record.t
val custom: string list -> 'b t -> ('a -> 'b) -> ('b -> 'a) -> 'a t

module Untyped: sig
  type t =
    | Array of t list
    | List of t list
    | Atom of string
    | Obj of (string * t) list
  type untyped = t
end

val retype: 'a t -> Untyped.t -> 'a option

val minify: Format.formatter -> ('a, Format.formatter, unit, unit) format4 -> 'a

val default: 'a -> 'a -> 'a option
val option: Name.t -> 'a t -> 'a option t


module Ext: sig
type ('lbl,'a) ext = {
  title: string;
  description: string;
  version: Version.t;
  label: 'lbl label;
  inner: 'a t;
}
type ('a,'b) t = ('a,'b) ext


type 'a diff = {expected: 'a; got:'a}
type error =
  | Future_version of Version.t diff
  | Mismatched_kind of string diff
  | Unknown_format
  | Parse_error


val json: ('lbl,'a) t -> Format.formatter -> 'a -> unit
val sexp: ('lbl,'a) t  -> Format.formatter -> 'a -> unit
val json_schema:  Format.formatter -> ('lbl, 'a) t -> unit

val strict: ('lbl,'a) t -> Untyped.t -> ('a, error) result
end
