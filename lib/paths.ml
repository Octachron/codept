
let rec last = function
  | [a] -> a
  | [] -> raise  @@  Invalid_argument "last expected a non-empty-file"
  | _ :: q -> last q

let may_chop_extension a =
  try Filename.chop_extension a with
    Invalid_argument _ -> a

let module_name file =
  String.capitalize_ascii @@ may_chop_extension @@ last @@ file


module Simple =
struct
  module Core = struct
    type t = Name.t list

    let compare (x:t) (y:t) = compare x y
    let pp = Pp.(list ~sep:(s".") ) Name.pp
  end
  include Core
  let sch = Schematic.(Array String)

  module Set = struct
    include Set.Make(Core)
    let pp ppf s = Pp.(clist Core.pp) ppf (elements s)
    let sch = let open Schematic in
      custom (Array sch)
        elements
        (List.fold_left (fun s x -> add x s) empty)
  end
  module Map = struct
    include (Map.Make(Core))
    let find_opt k m = try Some(find k m) with Not_found -> None
    let union' s = union (fun _key _m1 m2 -> Some m2) s
  end
  type set = Set.t
  type 'a map = 'a Map.t
  let prefix = List.hd

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

  let rec change_file_extension f = function
    | [] -> []
    | [a] -> [may_change_extension f a ]
    | a :: q -> a :: change_file_extension f q

  let rec chop_extension l = match l with
    | [] -> []
    | [a] -> [Filename.chop_extension a]
    | a :: q -> a :: chop_extension q

  let parse_filename name =
    let l = Support.split_on_char (String.get (Filename.dir_sep) 0) name in
    match List.rev l with
    | "" :: q -> List.rev q
    | l -> List.rev l

  let module_name = module_name

end
module S = Simple

module Expr = struct

  type t = { path: S.t; args: (int * t) list }

  module Sch = struct
    open Schematic
    let mu = Schematic_indices.one
    let raw: _ s = [ S.sch; Array [Int; mu] ]
    let rec c = Custom {fwd;rev; sch=raw }
    and fwd {path; args} = let open Tuple in
      [path; List.map (fun (n,x) -> [n;x]) args ]
    and rev = let open Tuple in
      fun [path; args] ->
        {path; args = List.map (fun [n;t] -> n, t) args }
    let t = Rec { id = ["Paths"; "Expr"; "t"]; defs = ["c", c]; proj = Zn }
  end
  let sch = Sch.t


  exception Functor_not_expected
  let concrete  = function
      | {path; args=[] } -> path
      | {args = _ :: _;  _ } -> raise Functor_not_expected

  let concrete_with_f p: Simple.t = p.path

  let multiples p : Simple.t list =
    let rec extract l = function
    | {path; args = [] } -> path :: l
    | {path; args } ->
      List.fold_left (fun l (_,x) -> extract l x) (path::l) args in
    List.rev (extract [] p)

  let rev_concrete p = List.rev @@ concrete p

  let from_list path = {path; args = [] }

  let rec normalize path =
    let args = List.sort (fun (n,_) (n',_) -> compare n n') path.args in
    let args = List.map (fun (n,x) -> n, normalize x) args in
    { path with args }

  (*
  let rec raw_pp ppf { path; args} =
    let int ppf d = Format.fprintf ppf "%d" d in
    Format.fprintf ppf "%a[%a]"
      S.pp path Pp.(list (pair int pp)) args
*)

  let rec pp offset ppf = function
    | {path; args = []} -> S.pp ppf path
    | {path; args = (n,x) :: l } -> subpath ppf offset n [] path x l
  and subpath ppf offset n sub rest x args =
    match n-offset, rest with
    | 0, [a] ->
      Format.fprintf ppf "%a(%a)"
        S.pp (List.rev (a::sub))
        (pp 0) x
    | 0, a :: q ->
      Format.fprintf ppf "%a(%a).%a"
        S.pp (List.rev (a::sub))
        (pp 0) x
        (pp n) { path = q; args }
    | _, []  -> assert false
    | _, a :: rest ->
      subpath ppf (offset+1) n (a::sub) rest x args

  let pp ppf x = pp 0 ppf (normalize x)

  let prefix = function
    | {path = [] ; _ } ->
      raise @@ Invalid_argument "Paths.Expr: prefix of empty path"
    | {path= a :: _ ; _ } -> a
end
module E = Expr

module Pkg = struct
  type source = Local | Unknown | Pkg of Simple.t | Special of Name.t

  let sep = Filename.dir_sep

  type t = { source: source ; file: Simple.t }
  type path = t

  module Sch = struct open Schematic
    let raw_source = Sum [ "Local", Void; "Unknown", Void;
                           "Pkg", Simple.sch; "Special", String ]
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
        custom [source; Simple.sch]
          (fun {source;file} -> Tuple.[source;file])
          Tuple.(fun [source;file] ->  {source;file} )
  end let sch = Sch.all

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

  let module_name {file; _ } = module_name file

  let update_extension f p =
    { p with file = Simple.change_file_extension f p.file }

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
    | Pkg n -> Pp.fp ppf "Pkg [%a]" Pp.(list ~sep:(const sep) string) n
    | Special s -> Pp.fp ppf "Special %s" s


  let pp_simple ppf {source;file}=
    Pp.fp ppf "(%a)%a" pp_source source
      Pp.(list ~sep:(const sep) string) file

  let pp_gen sep ppf {source;file} =
    begin match source with
      | Local -> ()
      | Unknown -> Pp.fp ppf "?"
      | Pkg s ->
        Pp.fp ppf "%a%s"
          Pp.(list ~sep:(const sep) string) s
          sep
      | Special s -> Pp.fp ppf "(*%s*)" s
    end;
    Pp.fp ppf "%a"
      Pp.(list ~sep:(const sep) string) file

  let pp = pp_gen sep
  let es ppf = Pp.fp ppf {|"%s"|}

  let reflect_source ppf =
    function
    | Local -> Pp.fp ppf "Local"
    | Unknown ->  Pp.fp ppf "Unknown"
    | Pkg n -> Pp.fp ppf "Pkg [%a]" Pp.(list ~sep:(s "; ") es) n
    | Special n -> Pp.fp ppf "Special %a" es n

  let reflect ppf {source;file} =
    Pp.fp ppf "{source=%a; file=[%a]}"
      reflect_source source
      Pp.(list ~sep:(const "; ") es) file

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

  let local file = { source = Local; file = S.parse_filename file }
  let (/) simple {source; file} = {source; file = simple @ file }

end
module P = Pkg
