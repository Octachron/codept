
module Edge = struct
  type t = Normal | Epsilon
  let max x y = if x = Epsilon then Epsilon else y
  let sexp = let open Sexp in
    sum [simple_constr "Normal" Normal;
         simple_constr "ε" Epsilon]
end

(*
(** Edge type for qualifying access *)
module Edge: sig
  type t =
    | Normal (**standard dependency *)
    | Epsilon (** immediate dependency *)
  val max: t -> t -> t
end
*)

type t = Edge.t Paths.P.map

module P = Paths.P

let empty = P.Map.empty

let update p e deps =
  let e = Option.(  Paths.P.Map.find_opt p deps >>| max e >< e )in
  Paths.P.Map.add p e deps

let merge = P.Map.merge (fun _k x y -> match x, y with
    | Some x, Some y -> Some (Edge.max x y)
    | None, (Some _ as x) | (Some _ as x), None -> x
    | None, None -> None
  )

let (+) = merge



let pp_elt ppf (path,edge) =
  Pp.fp ppf "%s%a" (if edge = Edge.Normal then "" else "ε∙") Paths.P.pp path


let pp ppf s =
    Pp.fp ppf "@[<hov>{%a}@]" (Pp.list pp_elt) (Paths.P.Map.bindings s)

let of_list l =
  List.fold_left (fun m (k,x) -> Paths.P.Map.add k x m) empty l

module Forget = struct
  let to_set x = P.Map.fold (fun k _ s -> P.Set.add k s) x P.Set.empty
  let to_list d =
    List.map fst @@ Paths.P.Map.bindings d
end
