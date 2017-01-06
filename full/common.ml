module Pkg = Paths.Pkg
module U = Unit

type param = {
    synonyms: Resource.info Name.Map.t;
    includes: Pkg.path Name.map;
}

let local_dependencies sort unit =
  sort
  @@ List.filter
    (function {Pkg.source=Unknown; _ }
            | {Pkg.source=Special _ ; _ } -> false | _ -> true )
  @@ Pkg.Set.elements unit.U.dependencies

type task =
  {
    files: (Read.kind * string) list Unit.pair;
    signatures: Module.t list;
    invisibles: Paths.S.set;
    libs: string list;
    opens: Paths.S.t list
  }
