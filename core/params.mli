(** Global parameter records *)

type t = {
  makefile : Makefile.param;
  synonyms : Common.synonyms;
  analyzer : Analysis.param;
  no_include : bool;
  may_approx : bool;
  nested:bool;
  pretty_format:bool;
  internal_format: Schematic.format;
  external_format: Schematic.format;
}

(** Lens module for accessing nested record in params *)
module L :
  sig
    type ('a, 'b) l = { get : 'a -> 'b; update : 'a -> 'b -> 'a; }
    val create : ('a -> 'b) -> ('a -> 'b -> 'a) -> ('a, 'b) l
    val get : ('a, 'b) l -> 'a -> 'b

    val update : 'a -> ('a, 'b) l -> 'b -> 'a
    val fmap : 'a ref -> ('a, 'b) l -> ('b -> 'b) -> unit

    val set : 'a ref -> ('a, 'b) l -> 'b -> unit

    val ( #. ) : 'a -> ('a, 'b) l -> 'b
    val ( #! ) : 'a ref -> ('a, 'b) l -> 'b
    val ( #<- ) : 'a ref -> ('a, 'b) l * 'b -> unit

    val compose : ('a, 'b) l -> ('b, 'c) l -> ('a, 'c) l
    val ( % ) : ('a, 'b) l -> ('b, 'c) l -> ('a, 'c) l

    val makefile : (t, Makefile.param) l
    val analyzer : (t, Analysis.param) l
    val no_include : (t, bool) l
    val may_approx : (t, bool) l
    val all : (t, bool) l
    val native : (t, bool) l
    val bytecode : (t, bool) l
    val abs_path : (t, bool) l
    val slash : (t, string) l
    val one_line : (t, bool) l
    val shared : (t, bool) l
    val implicits : (t, bool) l
    val synonyms : (t, Common.synonyms) l
    val includes : (t, string list) l
    val epsilon_dependencies: (t,bool) l
    val transparent_aliases : (t, bool) l
    val transparent_extension_nodes : (t, bool) l
    val precomputed_libs : (t, Name.set) l
    val closed_world : (t, bool) l
    val sig_only : (t, bool) l
    val policy : (t, Fault.Policy.t) l
    val nested : (t,bool) l
    val inner_fmt : (t, Schematic.format) l
    val ext_fmt : (t,Schematic.format) l
    val pretty_fmt: (t,bool) l
  end
