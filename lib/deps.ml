
module Edge = struct
  type t = Normal | Epsilon
  let max x y = if x = Epsilon then x else y
  let min x y = if x = Normal then x else y
  let sexp = let open Sexp in
    sum [simple_constr "Normal" Normal;
         simple_constr "eps" Epsilon]

  let sch = let open Scheme in
    custom "Deps.edge" (Sum[  "Normal", Void; "Epsilon", Void ])
      (function Normal -> C E | Epsilon -> C (S E))
      (function C E -> Normal | C S E -> Epsilon | _ -> . )

end

type t = (Edge.t * Paths.S.set) Paths.P.map

module Pth = Paths.S
module P = Paths.P

let empty = P.Map.empty

let update mp e ps deps: t =
  let ep = let open Option in
    P.Map.find_opt mp deps
    >>| (fun (e', ps' ) -> (Edge.max e e', Pth.Set.union ps' ps ) )
    >< (e, ps) in
  P.Map.add mp ep deps

let merge = P.Map.merge (fun _k x y -> match x, y with
    | Some (x,ps), Some (y,ps') -> Some (Edge.max x y, Pth.Set.union ps ps')
    | None, (Some _ as x) | (Some _ as x), None -> x
    | None, None -> None
  )

let (+) = merge



let pp_elt ppf (path,(edge,mpaths)) =
  Pp.fp ppf "%s%a(%a)" (if edge = Edge.Normal then "" else "ε∙")
    Paths.P.pp path Pth.Set.pp mpaths


let pp ppf s =
    Pp.fp ppf "@[<hov>{%a}@]" (Pp.list pp_elt) (P.Map.bindings s)

let of_list l =
  List.fold_left (fun m (k,(e,mps)) ->
      P.Map.add k (e, mps) m) empty l

module Forget = struct
  let to_set x = P.Map.fold (fun k _ s -> P.Set.add k s) x P.Set.empty
  let to_list d =
    List.map (fun (x, (_,s)) -> (x,s)) @@ P.Map.bindings d
end
