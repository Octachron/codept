type t =
  {
    makefile: Makefile.param;
    common: Common.param;
    analyzer: Analysis.param;
    no_include:bool;
    may_approx:bool;
    output: string;
  }

module L = struct
  type ('a,'b) l = { get: 'a -> 'b; update: 'a -> 'b -> 'a }

  let create f g = { get = f; update = g}
  let get x lens = lens.get x
  let update x lens y = lens.update x y
  let set x lens y = x := update (!x) lens y

  module String = struct
    let get = get
    let set = set
  end

  let compose (type a b c) (l: (a,b) l)  (r:(b,c) l) =
    { get = (fun x -> x.[l].[r]) ;
      update = (fun x y ->
          update x l ( update x.[l] r y) ) ;
    }

  let (%) = compose


  let makefile = create (fun x -> x.makefile) (fun x y -> { x with makefile = y})
  let common = create (fun x -> x.common) (fun x y -> { x with common = y})
  let analyzer = create (fun x -> x.analyzer) (fun x y -> { x with analyzer = y})
  let no_include = create
      (fun x -> x.no_include) (fun x y -> { x with no_include = y})
  let may_approx = create
      (fun x -> x.may_approx) (fun x y -> { x with may_approx = y})
  let output = create
      (fun x -> x.output) (fun x y -> { x with output = y})
  open Makefile
  let all = makefile % create (fun x-> x.all) (fun x y -> { x with all = y })
  let native = makefile % create
                 (fun x-> x.native) (fun x y -> { x with native = y })
  let bytecode = makefile % create (fun x-> x.bytecode)
                   (fun x y -> { x with bytecode = y })
  let abs_path = makefile % create (fun x-> x.abs_path)
                   (fun x y -> { x with abs_path = y })
  let slash = makefile % create (fun x-> x.slash) (fun x y -> { x with slash = y })
  let implicits = makefile % create (fun x-> x.implicits)
      (fun x y -> { x with implicits = y })
  open Common
  let synonyms = common % create
                   (fun x-> x.synonyms) (fun x y -> { x with synonyms = y })
  let includes = common % create
                   (fun x-> x.includes) (fun x y -> { x with includes = y })
  open Analysis let (%) = compose
  let transparent_aliases = analyzer % create (fun x-> x.transparent_aliases)
      (fun x y -> { x with transparent_aliases = y })
  let transparent_extension_nodes = analyzer %  create
      (fun x-> x.transparent_extension_nodes)
      (fun x y -> { x with transparent_extension_nodes = y })
  let precomputed_libs = analyzer % create (fun x-> x.precomputed_libs)
      (fun x y -> { x with precomputed_libs = y })
  let closed_world = analyzer % create (fun x-> x.closed_world)
      (fun x y -> { x with closed_world = y })
  let sig_only = analyzer % create
                   (fun x-> x.sig_only) (fun x y -> { x with sig_only = y })
  let policy = analyzer % create
                 (fun x-> x.policy) (fun x y -> { x with policy = y })
end
