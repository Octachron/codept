type p = Paths.S.t
type t = { namespace: p; name: Unitname.t; }
type namespaced = t


let pp ppf n =
  if n.namespace = [] then
    Unitname.pp_as_modname ppf n.name
  else
    Pp.fp ppf "%a.%a" Paths.S.pp n.namespace Unitname.pp_as_modname n.name



let pp_as_filepath ppf n =
  if n.namespace = [] then
    Unitname.pp_as_filepath ppf n.name
  else
    Pp.fp ppf "%a%s%s"
      Pp.(list ~sep:(const Filename.dir_sep) string) n.namespace Filename.dir_sep
      (Unitname.filename n.name)

let reflect  ppf n =
  let es ppf = Pp.fp ppf {|"%s"|} in
  Pp.fp ppf "{name=%a;namespace=[%a];}"
    Unitname.reflect n.name
    Pp.(list ~sep:(const "; ") es) n.namespace

let cons prefix n = { n with namespace = prefix @ n.namespace }

let to_string = Format.asprintf "%a" pp
let make ?(nms=[]) file = { namespace = nms; name= Unitname.modulize file }
let flatten n = n.namespace @ [Modname.to_string (Unitname.modname n.name)]
let of_path l =
  let rec split l = function
    | [a] -> l, a
    | a :: q -> split (a::l) q
    | [] -> raise @@ Invalid_argument("Namespaced.of_path: empty path")
  in
  let p, file = split [] l in
  let name = Unitname.modulize file in
  { namespace = List.rev p; name; }

let head = function
  | {namespace=a :: _ ; _ } -> a
  | {namespace=[]; name; _ } -> Unitname.filename name

let sch =
  let open Schematic in
  custom (Array String)
    (fun x -> flatten x)
    (fun x -> of_path x)

let compare a b =
  let v = compare a.namespace b.namespace in
  if v = 0 then Unitname.compare_as_modnames a.name b.name
  else v

module Ordered = struct
  type nonrec t = t
  let compare = compare
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


let module_path_of_filename ?(nms=[]) filename =
  let name = Unitname.modulize filename in
  match List.rev (Support.split_on_char Filename.dir_sep.[0] filename) with
  | [] -> raise @@ Invalid_argument "Invalid name for a compilation unit"
  | _ ->
    { namespace = nms ;
      name;
    }

let filepath_of_filename ?(nms=[]) filename =
  let name = Unitname.modulize filename in
  match List.rev (Support.split_on_char Filename.dir_sep.[0] filename) with
  | [] -> raise @@ Invalid_argument "Invalid name for a compilation unit"
  | _filename :: r ->
    { namespace = nms @ List.rev r ;
      name;
    }

let module_name x = Unitname.modname x.name

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
  { p with file = may_change_extension f p.file }
