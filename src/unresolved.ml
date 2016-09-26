type t = {path:Epath.q; deletions: Epath.set; env:rpath list}
and rpath = Loc of Epath.q * Epath.set | Extern of t

let ($) f u =
  let (k,p) = u.path in
  { u with path = k, f p }

let (//) u path =
  (fun u -> Epath.(u // path) ) $ u

let to_list u = List.rev @@ (Loc (u.path,u.deletions) ) :: u.env

let unlist l =
  match List.rev l with
  | [] -> raise @@ Invalid_argument "Unresolved.unlist: non-empty list expected"
  | Extern u :: _ ->u
  | Loc (path,deletions) :: env -> { path; deletions; env }


let pp_del ppf dels = (Pp.clist Epath.pp) ppf (Epath.Set.elements dels )


let rec pp_rpath ppf =
  function
  | Loc (p,dels) ->
  if Epath.Set.cardinal dels = 0 then
    Epath.pp_q ppf p
  else
    Pp.fp ppf "@[(%a/%a)@]" Epath.pp_q p pp_del dels
  | Extern ext -> pp_u ppf ext
and pp_u ppf u=
  let l = to_list u in
  let pp_main ppf = Pp.clist pp_rpath ppf in
  if Epath.Set.cardinal u.deletions = 0 then
    pp_main ppf l
  else
    Pp.fp ppf "@[(%a/%a)@]" pp_main l
      (Pp.clist Epath.pp) (Epath.Set.elements u.deletions)

module M = struct
  include Map.Make(struct type t = rpath let compare = compare end)
  let (|+>) (k,x) m = add k x m
  let find_opt k m = try Some(find k m) with Not_found -> None
end

type map = Map of map M.t
type direct_map = map M.t

let direct (Map m) = m
let empty = Map M.empty

type update = (map M.t -> map M.t) -> map M.t -> map M.t
type focus = { update: update; prev:update list; env:rpath list; map:map }

let start = { update = (@@); prev = []; env = []; map = empty }

let top foc =
  { foc with update=(@@); prev = []; env = []  }

let map { map = Map m; _ } = m
let update_map foc f = { foc with map = Map (foc.update f @@ map foc) }

let rec pp ppf (Map m)=
  let p fmt = Format.fprintf ppf fmt in
  if m = M.empty then ()
  else begin
    p "@[<hov2>[";
    M.iter (fun k x ->
        p "%a? @,%a" pp_rpath k pp x
      ) m;
    p "]@]@,";
  end

let (|=) = update_map

let add_new x foc = foc |= fun m ->
    match M.find_opt x m with
    | None ->  M.add x empty m
    | Some _ -> m

let down x foc =
  let update f =
    let f' m =
      let Map m' = M.find x m in
      M.add x (Map(f m')) m in
    foc.update f'
  in
  { map = foc.map;
    update;
    prev = foc.update :: foc.prev;
    env = x :: foc.env
  }

let up foc = match foc.prev with
  | [] -> foc
  | update::prev -> {
      map = foc.map;
      update;
      prev;
      env = List.tl foc.env
    }

let down_into x foc = foc |> add_new x |> down x

let move_to path foc =
  List.fold_left (fun foc u -> down u foc) (top foc) path

let refocus_on foc foc' =
  { foc with map = foc'.map }

let alias_with_context foc path = {path; deletions=Epath.Set.empty; env = foc.env }

let delete erased u =
  { u with deletions = Epath.Set.add erased u.deletions }

let delete_all erased u =
  Epath.Set.fold delete erased u

let is_empty m = m = M.empty
