(** Global parameter records *)

type t = {
  makefile : Makefile.param;
  common : Common.param;
  analyzer : Analysis.param;
  no_include : bool;
  may_approx : bool;
  output : string;
}

(** Lens module for accessing nested record in params *)
module L :
  sig
    type ('a, 'b) l = { get : 'a -> 'b; update : 'a -> 'b -> 'a; }
    val create : ('a -> 'b) -> ('a -> 'b -> 'a) -> ('a, 'b) l
    val get : 'a -> ('a, 'b) l -> 'b
    val update : 'a -> ('a, 'b) l -> 'b -> 'a
    val set : 'a ref -> ('a, 'b) l -> 'b -> unit
    module String :
      sig
        val get : 'a -> ('a, 'b) l -> 'b
        val set : 'a ref -> ('a, 'b) l -> 'b -> unit
      end

    val compose : ('a, 'b) l -> ('b, 'c) l -> ('a, 'c) l
    val ( % ) : ('a, 'b) l -> ('b, 'c) l -> ('a, 'c) l

    val makefile : (t, Makefile.param) l
    val common : (t, Common.param) l
    val analyzer : (t, Analysis.param) l
    val no_include : (t, bool) l
    val may_approx : (t, bool) l
    val output : (t, string) l
    val all : (t, bool) l
    val native : (t, bool) l
    val bytecode : (t, bool) l
    val abs_path : (t, bool) l
    val sort : (t, bool) l
    val slash : (t, string) l
    val implicits : (t, bool) l
    val synonyms : (t, Common.info Name.map) l
    val includes : (t, Paths.Pkg.t Name.map) l
    val transparent_aliases : (t, bool) l
    val transparent_extension_nodes : (t, bool) l
    val no_stdlib : (t, bool) l
    val std_otherlibs : (t, bool) l
    val closed_world : (t, bool) l
    val sig_only : (t, bool) l
    val polycy : (t, Fault.Polycy.t) l
  end
