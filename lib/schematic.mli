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
  sig type t val l: t label end[@@warning "-unused-functor-parameter"]

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
type 'a r = Indexed

type ('hole, 'free) s =
  | Float: (float, 'free) s
  | Int: (int, 'free) s
  | Bool: (bool, 'free) s
  | String: (string, 'free) s
  | Void: (void, 'free) s
  | Array: ('hole,'free) s -> ('hole list, 'free) s
  | (::) : ('a,'free) s * ('b Tuple.t, 'free) s -> (('a * 'b) Tuple.t, 'free) s
  | []: (void Tuple.t, 'free) s
  | Obj: ('a,'free) record_declaration -> ('a Record.t, 'free) s
  | Custom: ('a,'b,'free) custom -> ('a, 'free) s
  | Sum: ('a,'free) sum_decl -> ('a sum,'free) s
  | Description: string * ('hole,'free) s -> ('hole, 'free) s
  | Rec: { id: string list; defs:('defs,'defs r) rec_defs; proj: ('defs, 'res) index}
      -> ('res,'free) s
  | Var: ('free,'result) index -> ('result,'free r) s

and (_,_) index =
  | Zn: ('a * 'b ,'a) index
  | Sn: ('list,'res) index -> ( _ * 'list, 'res) index

and (_,_) rec_defs =
  | []: (void,'free r) rec_defs
  | (::): (string * ('a,'free r)s)  * ('l, 'free r) rec_defs -> ('a * 'l, 'free r) rec_defs

and ('a,'b,'free) custom = { fwd:'a -> 'b; rev:'b -> 'a; sch:('b,'free) s }
and ('a,'free) record_declaration =
  | []: (void, 'free) record_declaration
  | (::): ( ('m,'x,'fx) modal * 'a label * ('x,'free) s) * ('c,'free) record_declaration
    -> (  'a * 'fx * 'c, 'free) record_declaration

and ('a,'mu) sum_decl =
    | [] : (<before:void>, 'mu) sum_decl
    | (::): (string * ('a,'mu) s) * ('b,'mu) sum_decl
        -> (<at:'a; before:'b>,'mu) sum_decl

and (_,_) cons =
  | Z: 'a -> (<at:'a; before: 'any>,'a) cons
  | E: (<at:void; before:'any>,'a) cons
  | S: ('a, 'n ) cons -> (<at:'any; before:'a>, 'n) cons

and 'a sum = C: ('a, 'elt ) cons -> 'a sum

type 'a t = ('a,void) s
type 'a schematic = 'a t

val reopen: 'a t -> ('a,'b) s

module Version: sig
  type lbl
  type t = { major:int; minor:int; patch:int }
  val sch: t schematic
end


val pretty_json: 'a t -> Format.formatter -> 'a -> unit
val simple_json: 'a t -> Format.formatter -> 'a -> unit

val sexp: 'a t  -> Format.formatter -> 'a -> unit

val ($=): 'a label -> 'b -> ('a label * 'b)
val skip: 'a label -> ('a label * 'b option)
val ($=?): 'a label -> 'b option -> ('a label * 'b option)

val obj: 'a Record.t -> 'a Record.t
val custom: ('b,'f) s -> ('a -> 'b) -> ('b -> 'a) -> ('a,'f) s

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
val option: ('a,'f) s -> ('a option,'f) s
val pair: ('a,'f) s -> ('b,'f) s -> ('a * 'b, 'f) s
val (<?>): ('a,'f) s -> string -> ('a,'f) s

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


val pretty_json: ('lbl,'a) t -> Format.formatter -> 'a -> unit
val simple_json: ('lbl,'a) t -> Format.formatter -> 'a -> unit
val sexp: ('lbl,'a) t  -> Format.formatter -> 'a -> unit
val json_schema:  Format.formatter -> ('lbl, 'a) t -> unit

val strict: ('lbl,'a) t -> Untyped.t -> ('a, error) result
end
