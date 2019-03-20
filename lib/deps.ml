
module Edge = struct
  type t = Normal | Epsilon
  let max x y = if x = Epsilon then x else y
  let min x y = if x = Normal then x else y

  let sch = let open Schematic in
    custom ["Deps"; "edge"] (Sum["Normal", Void; "Epsilon", Void ])
      (function Normal -> C E | Epsilon -> C (S E))
      (function C E -> Normal | C S E -> Epsilon | _ -> . )

end

type t = (Edge.t * Paths.S.t) Paths.P.map

let sch: t Schematic.t =
  let open Schematic in
  let from_list =
    let open Tuple in
    List.fold_left (fun m [k; x; y] ->
        Paths.P.Map.add k (x,y) m)
      Paths.P.Map.empty in
  let to_list m =
    List.map (fun (k, (x,y)) -> Tuple.[k;x;y])
      (Paths.P.Map.bindings m) in
  custom ["Deps"; "t"] (Array [Paths.P.sch; Edge.sch; Paths.S.sch])
    to_list
    from_list

module Pth = Paths.S
module P = Paths.P

let empty = P.Map.empty

let update mp e ps deps: t =
  let ep = let open Option in
    P.Map.find_opt mp deps
    >>| (fun (e', _ ) -> (Edge.max e e', ps ) )
    >< (e, ps) in
  P.Map.add mp ep deps

let make mp e ps = update mp e ps empty

let merge = P.Map.merge (fun _k x y -> match x, y with
    | Some (x,_), Some (y,ps') -> Some (Edge.max x y, ps')
    | None, (Some _ as x) | (Some _ as x), None -> x
    | None, None -> None
  )

let (+) = merge


let find path deps = Paths.P.Map.find_opt path deps
let fold f deps acc = Paths.P.Map.fold (fun p (e,s) -> f p e s) deps acc

let pp_elt ppf (path,(edge,mpaths)) =
  Pp.fp ppf "%s%a(%a)" (if edge = Edge.Normal then "" else "ε∙")
    Paths.P.pp path Pth.pp mpaths


let pp ppf s =
    Pp.fp ppf "@[<hov>{%a}@]" (Pp.list pp_elt) (P.Map.bindings s)

let of_list l =
  List.fold_left (fun m (k, e,mps) ->
      P.Map.add k (e, mps) m) empty l

module Forget = struct
  let to_set x = P.Map.fold (fun k _ s -> P.Set.add k s) x P.Set.empty
  let to_list d =
    List.map (fun (x, (_,s)) -> x, s) @@ P.Map.bindings d
end
