
module Edge = struct
  type t = Normal | Epsilon
  let max x y = if x = Epsilon then x else y
  let min x y = if x = Normal then x else y

  let sch = let open Schematic in
    custom ["Deps"; "edge"] (Sum["Normal", Void; "Epsilon", Void ])
      (function Normal -> C E | Epsilon -> C (S E))
      (function C E -> Normal | C S E -> Epsilon | _ -> . )

end

type dep = { path: Paths.S.t; edge:Edge.t; pkg:Paths.Pkg.t }
type t = (Edge.t * Paths.P.t) Paths.S.map
module Map = Paths.S.Map

let sch: t Schematic.t =
  let open Schematic in
  let from_list =
    let open Tuple in
    List.fold_left (fun m [k; x; y] -> Map.add k (x,y) m) Map.empty in
  let to_list m =
    List.map (fun (k, (x,y)) -> Tuple.[k;x;y]) (Map.bindings m) in
  custom ["Deps"; "t"] (Array [Paths.S.sch; Edge.sch; Paths.P.sch])
    to_list
    from_list

module Pth = Paths.S
module P = Paths.P

let empty = Map.empty

let update mp e ps deps: t =
  let ep = let open Option in
    Map.find_opt mp deps
    >>| (fun (e', _ ) -> (Edge.max e e', ps ) )
    >< (e, ps) in
  Map.add mp ep deps

let make mp e ps = update mp e ps empty

let merge = Map.merge (fun _k x y -> match x, y with
    | Some (x,_), Some (y,ps') -> Some (Edge.max x y, ps')
    | None, (Some _ as x) | (Some _ as x), None -> x
    | None, None -> None
  )

let (+) = merge


let find path deps =
  Option.fmap (fun (edge,pkg) -> {path;edge;pkg}) @@ Map.find_opt path deps
let fold f deps acc =
  Map.fold (fun path (edge,pkg) -> f {path;edge;pkg}) deps acc

let pp_elt ppf (path, (edge, pkg)) =
  Pp.fp ppf "%s%a(%a)" (if edge = Edge.Normal then "" else "ε∙")
    Pth.pp path Paths.P.pp pkg

let pp ppf s =
    Pp.fp ppf "@[<hov>{%a}@]" (Pp.list pp_elt) (Map.bindings s)

let of_list l =
  List.fold_left (fun m {path;edge;pkg} -> Map.add path (edge, pkg) m) empty l

let pkgs deps = fold (fun {pkg; _ } x ->  pkg :: x) deps []
let paths deps = fold (fun {path; _ } x ->  path :: x) deps []
let all deps = fold List.cons deps []
let pkg_set x = Map.fold (fun _ (_,p) s -> P.Set.add p s) x P.Set.empty
