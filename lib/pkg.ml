type source = Local | Unknown | Pkg of Namespaced.t | Special of Name.t

let sep = Filename.dir_sep

type t = { source: source ; file: Namespaced.t }
type path = t

module Sch = struct open Schematic
  let raw_source = Sum [ "Local", Void; "Unknown", Void;
                         "Pkg", Namespaced.sch; "Special", String ]
  let source = custom raw_source
      (function
        | Local -> C E
        | Unknown -> C (S E)
        | Pkg s -> C (S (S (Z s)))
        | Special s -> C(S(S(S(Z s))))
      )
      (function
        | C E -> Local
        | C S E -> Unknown
        | C S S Z s -> Pkg s
        | C S S S Z s -> Special s
        | _ -> .
      )
  let all =
    custom [source; Namespaced.sch]
      (fun {source;file} -> Tuple.[source;file])
      Tuple.(fun [source;file] ->  {source;file} )
end let sch = Sch.all

let filename ?(sep=sep) p =
  let flatten n = n.Namespaced.namespace @ [ Unitname.filename n.name ] in
  begin match p.source with
    | Pkg n -> String.concat sep (flatten n) ^ sep
    | _ -> ""
  end
  ^
  String.concat sep (flatten p.file)

let is_known = function
  | {source=Unknown; _ } -> false
  | _ -> true

let module_name {file; _ } = Namespaced.module_name file

let update_extension f p =
  { p with file = Namespaced.change_file_extension f p.file }

let change_extension ext =
  update_extension ( fun _ -> ext )

let cmo = change_extension ".cmo"
let o = change_extension ".o"
let cmi = change_extension ".cmi"
let cmx = change_extension ".cmx"
let cmxs = change_extension ".cmx"

let mk_dep all native = update_extension @@ function
  | "mli" | "m2li" -> ".cmi"
  | "ml" | "m2l" when all -> ".cmi"
  | "ml" | "m2l" ->
    if native then ".cmx" else ".cmo"
  | "cmi" -> ".cmi"
  | s -> raise @@Invalid_argument ("Unknown extension " ^ s)

let pp_source ppf = function
  | Local -> Pp.fp ppf "Local"
  | Unknown ->  Pp.fp ppf "Unknown"
  | Pkg n -> Pp.fp ppf "Pkg [%a]" Namespaced.pp n
  | Special s -> Pp.fp ppf "Special %s" s


let pp_simple ppf {source;file}=
  Pp.fp ppf "(%a)%a" pp_source source
    Namespaced.pp file

let pp_gen sep ppf {source;file} =
  begin match source with
    | Local -> ()
    | Unknown -> Pp.fp ppf "?"
    | Pkg s ->
      Pp.fp ppf "%a%s" Namespaced.pp_as_filepath s
        sep
    | Special s -> Pp.fp ppf "(*%s*)" s
  end;
  Namespaced.pp_as_filepath ppf file

let pp = pp_gen sep
let es ppf = Pp.fp ppf {|"%s"|}

let reflect_source ppf =
  function
  | Local -> Pp.fp ppf "Local"
  | Unknown ->  Pp.fp ppf "Unknown"
  | Pkg n -> Pp.fp ppf "Pkg [%a]" Namespaced.reflect n
  | Special n -> Pp.fp ppf "Special %a" es n

let reflect ppf {source;file} =
  Pp.fp ppf "{source=%a; file=%a}"
    reflect_source source
    Namespaced.reflect file

module Set = struct
  include Set.Make(struct type t = path let compare = compare end)
  let pp ppf s = Pp.(clist pp) ppf (elements s)
end
module Map = struct
  include Map.Make(struct type t = path let compare = compare end)
  let find_opt k m = try Some(find k m) with Not_found -> None
end
type set = Set.t
type 'a map = 'a Map.t

let local file = { source = Local; file = Namespaced.filepath_of_filename file }
let (/) simple {source; file} = {source; file = Namespaced.cons simple file }

