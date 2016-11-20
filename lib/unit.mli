(** Functions for handling unit (aka .ml/.mli) files *)

module Pkg = Paths.Pkg
module Pth = Paths.Simple
type kind = Structure | Signature
type t = {
  name : string;
  path : Pkg.t;
  kind : kind;
  code : M2l.t;
  dependencies : Pkg.set;
}

val pp : Format.formatter -> t -> unit

type 'a pair = { ml : 'a; mli : 'a; }
type unit = t

(** {!Group} handles pair of ml/mli files together *)
module Group :
  sig
    type t = unit option pair
    type group = t
    exception Collision of { previous : unit; collision : unit; }
    val add_mli : unit -> group -> group
    val add_ml : unit -> group -> group
    val add : unit -> group -> group
    val empty : group
    module Map :
      sig
        type t = group Pth.map
        val add : unit -> group Pth.map -> group Pth.map
        val of_list : unit list -> group Pth.map
      end
  end


val extract_name : string -> Name.t
(** extract a module name from a file name*)

val read_file : kind -> string -> unit
(** [read_file kind filename] reads the file, extracting
    the corresponding m2l ast
*)

val group_by : (string -> kind) -> string list -> Group.t Pth.map
val group : string list pair -> Group.t Pth.map

val split : Group.t Pth.map -> unit list pair

module Set : Set.S with type elt = unit
