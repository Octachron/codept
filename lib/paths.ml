
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
  let concat = List.append

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

module type simple_core = sig
  type t
  val concat: t -> Simple.t -> t
  val prefix: t -> Name.t
  val sch: t Schematic.t
  val pp: t Pp.t
end

(** Module paths with application *)
module type Expr = sig
  type s
  type t = private
    | Simple of s
    | Apply of { f:t; x:t; proj: Simple.t option }
  val sch: t Schematic.t
  exception Functor_not_expected
  val concrete : t -> s
  val concrete_with_f : t -> s
  val multiples : t -> s list
  val pure : s -> t
  val app: t -> t -> Simple.t option -> t
  val pp : Format.formatter -> t -> unit
  val prefix : t -> string
  module Map: Map.S with type key = t
  type 'a map = 'a Map.t
end

module Make_expr(S:simple_core): Expr with type s := S.t = struct

  type t =
    | Simple of S.t
    | Apply of { f:t; x:t; proj: Simple.t option }

  module Sch = struct
    type w = W of S.t [@@unboxed]
    open Schematic
    let w = Custom { fwd = (fun (W x) -> x); rev=(fun x -> W x); sch=S.sch }
    let mu = Schematic_indices.one
    let raw: _ s = Sum [ "S", reopen w; "Apply", [mu;mu; option (reopen Simple.sch) ] ]
    let rec c = Custom {fwd;rev; sch=raw }
    and fwd  = function
      | Apply {f;x;proj} -> C(S(Z Tuple.[f;x;proj]))
      | Simple x -> C (Z (W x))
    and rev =
      let open Tuple in
      function
      | C Z (W s) -> Simple s
      | C S Z [f;x;proj] -> Apply {f;x;proj}
      | _ -> .
    let t = Rec { id = ["Paths"; "Expr"; "t"]; defs = ["c", c]; proj = Zn }
  end
  let sch = Sch.t


  exception Functor_not_expected
 let concrete  = function
    | Simple x -> x
    | Apply _ -> raise Functor_not_expected

  let rec concrete_with_f = function
    | Simple x -> x
    | Apply {f;proj=None; _ } -> concrete_with_f f
    | Apply {f;proj=Some r; _ } -> S.(concat (concrete_with_f f) r)

  let rec multiples = function
    | Apply {f;x;proj=Some proj} -> fn f proj @ multiples x
    | Apply {f;x;proj=None} ->  multiples f @ multiples x
    | Simple x -> [x]
  and fn f proj = match f with
    | Simple x -> S.[concat x proj]
    | Apply {proj=None; f; x } -> fn f proj @ multiples x
    | Apply {proj=Some p; f; x } -> fn f ( p @ proj) @ multiples x

  let pure path = Simple path
  let app f x proj = Apply {f;x;proj}


  (*
  let rec raw_pp ppf { path; args} =
    let int ppf d = Format.fprintf ppf "%d" d in
    Format.fprintf ppf "%a[%a]"
      S.pp path Pp.(list (pair int pp)) args
*)

  let rec pp ppf = function
    | Simple path -> S.pp ppf path
    | Apply {f;x;proj=None} -> Pp.fp ppf "%a(%a)" pp f pp x
    | Apply {f;x;proj=Some p} -> Pp.fp ppf "%a(%a).%a" pp f pp x Simple.pp p

  let rec prefix = function
    | Simple s -> S.prefix s
    | Apply r -> prefix r.f

  module Map = Map.Make(struct
      let compare = compare
      type nonrec t = t
    end)
  type 'a map = 'a Map.t

end

module Expr = Make_expr(Simple)
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
