type source = Local | Unknown | Pkg of Npath.t

let sep = Filename.dir_sep

type t = { source: source ; file: Npath.t }
type path = t

let filename ?(sep=sep) p =
  begin match p.source with
    | Pkg n -> String.concat sep n ^ sep
    | _ -> ""
  end
  ^
  String.concat sep p.file

let is_known = function
  | {source=Unknown; _ } -> false
  | _ -> true

let rec last = function
  | [a] -> a
  | [] -> raise  @@  Invalid_argument "last expected a non-empty-file"
  | _ :: q -> last q


let extension name =
  let n = String.length name in
  try
    let r = String.rindex name '.' in
    Some (String.sub name (r+1) (n-r-1))
  with Not_found -> None

let may_chop_extension a =
  try Filename.chop_extension a with
    Invalid_argument _ -> a

let may_change_extension f a =
  match extension a with
  | None -> a
  | Some ext ->
    let base = Filename.chop_extension a in
    base ^ f ext

let module_name {file; _ } =
  String.capitalize_ascii @@ may_chop_extension @@ last @@ file

let rec chop_suffix l = match l with
  | [] -> []
  | [a] -> [Filename.chop_extension a]
  | a :: q -> a :: chop_suffix q

let rec change_file_extension f = function
  | [] -> []
  | [a] -> [may_change_extension f a ]
  | a :: q -> a :: change_file_extension f q


let update_extension f p =
  { p with file = change_file_extension f p.file }

let change_extension ext =
  update_extension ( fun _ -> ext )

let cmo = change_extension ".cmo"
let o = change_extension ".o"
let cmi = change_extension ".cmi"
let cmx = change_extension ".cmx"

let mk_dep all native = update_extension @@ function
  | "mli" -> ".cmi"
  | "ml" when all -> ".cmi"
  | "ml" ->
    if native then ".cmx" else ".cmo"
  | _ -> assert false

let pp_source ppf = function
  | Local -> Pp.fp ppf "Local"
  | Unknown ->  Pp.fp ppf "Unknown"
  | Pkg n -> Pp.fp ppf "Sys[%a]" Npath.pp n

let pp_simple ppf {source;file}=
  Pp.fp ppf "(%a)%a" pp_source source Npath.pp file

let pp_gen sep ppf {source;file} =
  begin match source with
    | Local -> ()
    | Unknown -> Pp.fp ppf "?"
    | Pkg s ->
      Pp.fp ppf "%a%s"
        Pp.(list ~sep:(const sep) string) s
        sep
  end;
  Pp.fp ppf "%a"
    Pp.(list ~sep:(const sep) string) file

let pp = pp_gen sep

module Set = struct
  include Set.Make(struct type t = path let compare = compare end)
  let pp ppf s = Pp.(clist pp) ppf (elements s)
end

type set = Set.t

let slash = String.get sep 0

let parse_filename filename =
  let n = String.length filename in
  let rec extract shards start current =
    if current = n - 1  then
      let offset = if filename.[current] = slash then 0 else 1 in
      List.rev @@
      String.sub filename start (current-start+offset) :: shards
    else if filename.[current] = slash then
      extract (String.sub filename start (current-start) :: shards)
        (current + 1) (current + 1)
    else
      extract shards start (current + 1) in
  extract [] 0 0

let local file = { source = Local; file }
