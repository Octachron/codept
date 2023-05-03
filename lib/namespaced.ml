type p = Paths.S.t
type t = { namespace: p; name: Name.t }
type namespaced = t


let pp ppf n =
  if n.namespace = [] then
    Pp.string ppf n.name
  else
    Pp.fp ppf "%a.%s" Paths.S.pp n.namespace n.name



let pp_as_filepath ppf n =
  if n.namespace = [] then
    Pp.string ppf n.name
  else
    Pp.fp ppf "%a%s%s"
      Pp.(list ~sep:(const Filename.dir_sep) string) n.namespace Filename.dir_sep n.name

let reflect  ppf n =
  let es ppf = Pp.fp ppf {|"%s"|} in
  Pp.fp ppf "{name=%S;namespace=[%a]}"
    n.name
    Pp.(list ~sep:(const "; ") es) n.namespace

let cons prefix n = { n with namespace = prefix @ n.namespace }

let to_string = Format.asprintf "%a" pp
let make ?(nms=[]) name = { namespace = nms; name }
let flatten n = n.namespace @ [n.name]
let of_path l =
  let rec split l = function
    | [a] -> l, a
    | a :: q -> split (a::l) q
    | [] -> raise @@ Invalid_argument("Namespaced.of_path: empty path")
  in
  let p, name = split [] l in
  { namespace = List.rev p; name }

let head = function
  | {namespace=a :: _ ; _ } -> a
  | {namespace=[]; name } -> name

let sch =
  let open Schematic in
  custom (Array String)
    (fun x -> flatten x)
    (fun x -> of_path x)


module Ordered = struct
  type nonrec t = t
  let compare: t -> t -> int = compare
end

module Map= struct
  include Map.Make(Ordered)
  let find_opt x m = try Some(find x m) with Not_found -> None
end
type 'a map = 'a Map.t

module Set = struct
  include Set.Make(Ordered)
    let pp ppf s = Pp.(clist pp) ppf (elements s)
    let sch = let open Schematic in
      custom (Array sch)
        elements
        (List.fold_left (fun s x -> add x s) empty)
end
type set = Set.t


let may_chop_extension a =
  try Filename.chop_extension a with
    Invalid_argument _ -> a

let module_name file =
  String.capitalize_ascii @@ may_chop_extension @@ file

let module_path_of_filename ?(nms=[]) filename =
  let p = Paths.S.parse_filename filename in
  match List.rev p with
  | [] ->  raise  @@  Invalid_argument "Invalid name for a compilation unit"
  | name :: _ ->
    { namespace = nms ;
      name = module_name name
    }

let filepath_of_filename ?(nms=[]) filename =
  let p = Paths.S.parse_filename filename in
  match List.rev p with
  | [] ->  raise  @@  Invalid_argument "Invalid name for a compilation unit"
  | name :: r ->
    { namespace = nms @ List.rev r ;
      name
    }


let module_name x = module_name x.name

let chop_extension p = { p with name = Filename.chop_extension p.name }

let extension a =
  let ext = Support.extension a in
  if not (ext = "") && ext.[0] = '.' then
    String.sub ext 1 (String.length ext - 1)
  else
    ext

let may_change_extension f a =
  match extension a with
  | "" -> a
  | ext ->
    let base = Filename.chop_extension a in
    base ^ f ext

let change_file_extension f p =
  { p with name = may_change_extension f p.name }
