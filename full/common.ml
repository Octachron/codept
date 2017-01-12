module Pkg = Paths.Pkg
module U = Unit

type kind = Interface | Implementation | Signature
type info = { format: Read.format; kind : kind }

let ml = { format=Src; kind = Implementation }
let mli = { format=Src; kind = Interface }
let classic {format;kind}: Read.kind option = match kind with
  | Interface -> Some { format; kind = M2l.Signature }
  | Implementation -> Some { format; kind = M2l.Structure }
  | Signature -> None


type param = {
    synonyms: info Name.Map.t;
    includes: Pkg.path Name.map;
}

type task =
  {
    files: (Read.kind * string) list Unit.pair;
    signatures: Module.t list;
    invisibles: Paths.S.set;
    libs: string list;
    opens: Paths.S.t list
  }

let local_dependencies sort unit =
  sort
  @@ List.filter
    (function {Pkg.source=Unknown; _ }
            | {Pkg.source=Special _ ; _ } -> false | _ -> true )
  @@ Pkg.Set.elements unit.U.dependencies

let make_abs abs p =
  let open Paths.Pkg in
  if abs && p.source = Local then
    { p with file = Sys.getcwd() :: p.file }
  else
    p
