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


type synonyms =info Name.Map.t

type task =
  {
    files: (info * string) list;
    seeds: Name.t list;
    invisibles: Paths.S.set;
    libs: string list;
    opens: Paths.S.t list
  }


let compiler_dir =
  lazy (
    let ch = Unix.open_process_in "ocamlc -where" in
    let s= input_line ch in
    close_in ch;
    s ^ "/"
  )

let expand_dir dir =
    if dir <> "" && dir.[0] = '+' then
      Lazy.force compiler_dir ^ String.sub dir 1 (String.length dir - 1)
    else dir

let local_dependencies unit =
  List.filter
    (function {Pkg.source=Unknown; _ }
            | {Pkg.source=Special _ ; _ } -> false | _ -> true )
  @@ Pkg.Set.elements unit.U.dependencies

let make_abs abs p =
  let open Paths.Pkg in
  if abs && p.source = Local then
    { p with file = Sys.getcwd() :: p.file }
  else
    p

let is_stdlib_pkg = function
  | "stdlib" | "unix" | "threads" | "bigarray" | "graph" | "num"
  | "dynlink" -> true
  | _ -> false

let extension name =
  let n = String.length name in
  let r = try String.rindex name '.' with Not_found -> n-1 in
  String.sub name (r+1) (n-r-1)

let classify policy synonyms f =
  let ext = extension f in
  match Name.Map.find ext synonyms with
  | x -> Some x
  | exception Not_found ->
    Fault.handle policy Codept_policies.unknown_extension ext; None
