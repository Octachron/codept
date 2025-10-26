
module Edge = struct
  type t = Normal | Epsilon
  let max x y = if x = Epsilon then x else y
  let min x y = if x = Normal then x else y

  let sch = let open Schematic in
    custom  (Sum["Normal", Void; "Epsilon", Void ])
      (function Normal -> C E | Epsilon -> C (S E))
      (function C E -> Normal | C S
           E -> Epsilon | _ -> . )
  let pp ppf = function
    | Normal -> Pp.fp ppf "N"
    | Epsilon -> Pp.fp ppf "ε"

end

module S = Namespaced.Set
module Externals = Name.Set
type dep = { path: Namespaced.t; edge:Edge.t; pkg:Pkg.t; aliases:S.t}
type subdep = { edge:Edge.t; pkg:Pkg.t; aliases:S.t }
module Map = Namespaced.Map
type t = { units: subdep Map.t; externals : Externals.t }

let sch: t Schematic.t =
  let module T = Schematic.Tuple in
  let from_ = let open T in
    fun [units; externals] ->
      let units =
        List.fold_left
          (fun m [k; edge; pkg; aliases] -> Map.add k {edge;pkg;aliases} m)
          Map.empty units
      in
      { units; externals = Externals.of_list externals }
  in
  let to_ { units = m; externals }  =
    T.(::)(
      (Map.fold (fun k {edge;pkg;aliases} l -> T.[k;edge;pkg;aliases] :: l) m []),
      T.[Externals.elements externals]
    )
in
  let open Schematic in
  custom ([Array [Namespaced.sch; Edge.sch; Pkg.sch; S.sch]; Array String])
    to_ from_

module Pth = Paths.S
module P = Pkg

let empty = { units = Map.empty; externals = Externals.empty }

let update ~path ?(aliases=S.empty) ~edge pkg { units; externals }: t =
  let ep =
    let update x =
      let aliases = S.union aliases x.aliases in
      { x with edge = Edge.max edge x.edge; aliases } in
    Option.either update {edge;pkg; aliases }
      (Map.find_opt path units) in
  let units = Map.add path ep units in
  { units; externals }

let add_external ext deps = { deps with externals = Externals.add ext deps.externals }
let add_externals ext deps =
  let externals = List.fold_left (fun s x -> Externals.add x s) deps.externals ext in
  { deps with externals }
let externals_only d = { empty with externals = d.externals }

let make ~path ?aliases ~edge pkg = update ~path ?aliases ~edge pkg empty

let merge x y =
  let units =
    Map.union (fun _k x y ->
        let aliases = S.union x.aliases y.aliases in
        Some { y with edge = Edge.max x.edge y.edge; aliases }
      ) x.units y.units
  in
  let externals = Externals.union x.externals y.externals in
  { units; externals }

let (+) = merge


let find path deps =
  Option.fmap (fun {edge;pkg;aliases} -> {path;edge;pkg;aliases}) @@ Map.find_opt path deps.units
let fold f deps acc =
  Map.fold (fun path {edge;pkg;aliases} -> f {path;edge;pkg;aliases}) deps.units acc

let pp_elt ppf (path, {edge;pkg;aliases}) =
  Pp.fp ppf "%s%a(%a)%a" (if edge = Edge.Normal then "" else "ε∙")
    Namespaced.pp path P.pp pkg S.pp aliases

let pp ppf s =
    Pp.fp ppf "@[<v>externals:@[<hov>]%a@,@[<hov>{%a}@]@]"
      Pp.(list string) (Externals.elements s.externals)
      (Pp.list pp_elt) (Map.bindings s.units)

let of_list l =
  let units =
    List.fold_left
      (fun m {path;edge;pkg;aliases} -> Map.add path {edge; pkg; aliases} m)
      empty.units l
  in
  { units; externals = Externals.empty }

let pkgs deps = fold (fun {pkg; _ } x ->  pkg :: x) deps []
let paths deps = fold (fun {path; _ } x ->  path :: x) deps []
let externals deps = Externals.elements deps.externals
let all deps = fold List.cons deps []
let pkg_set x = Map.fold (fun _ x s -> P.Set.add x.pkg s) x.units P.Set.empty
