(** Topological order functions *)

type 'a action =
  | Explore of 'a list
  | Add of 'a

let full_topological_sort (type k) ~(key: _ -> k) deps paths =
  let visited = Hashtbl.create 17 in
  let future = Hashtbl.create 17 in
  let add x =
    let k = key x in
    Hashtbl.remove future k;
    Hashtbl.add visited k true
  in
  let cycle x = Hashtbl.mem future (key x) in
  let visited x = Hashtbl.mem visited (key x) in
  let guard x = Hashtbl.add future (key x) true in
  let rec sort path sorted = function
    | [] -> Ok sorted
    | Explore [] :: q -> sort path sorted q
    | Add x :: q ->
      add x;
      sort (List.tl path) (x::sorted) q
    | Explore (a::nodes) :: q ->
      if cycle a then
        Error ( a::path)
      else if visited a then
        sort path sorted (Explore nodes :: q)
      else begin
        guard a;
        sort (a::path) sorted (Explore (deps a) :: Add a :: Explore nodes :: q)
      end
  in
  Mresult.Ok.fmap List.rev @@ sort [] [] [Explore paths]

let remember_order units =
  let open Unit in
  let compute (i,m) u = i+1, Namespaced.Map.add u.Unit.path i m in
  snd @@ List.fold_left compute (0,Namespaced.Map.empty)
  @@ List.rev @@ List.filter (fun u -> Pkg.is_known u.src) @@ units

let topos_compare order x y =
  let get x=Namespaced.Map.find_opt x order in
  match get x, get y with
  | Some k , Some l -> compare k l
  | None, Some _ -> -1
  | Some _, None -> 1
  | None, None -> compare x y

let toposort order m =
    List.sort (fun x y -> topos_compare order (m x) (m y))
