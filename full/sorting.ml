(** Topological order functions *)
let full_topological_sort deps paths =
  let visited = Hashtbl.create 17 in
  let mark x = Hashtbl.add visited x true in
  let is_visited x = Hashtbl.mem visited x in
  let rec sort sorted = function
    | [] -> sorted
    | a :: q ->
      if is_visited a then
        sort sorted q
      else
        let sorted = sort_at sorted a in
        sort sorted q
  and sort_at sorted x =
    let sorted = Paths.Pkg.Set.fold sort_dep (deps x) sorted in
    mark x;
    x :: sorted
  and sort_dep y sorted =
    if is_visited y then
      sorted
    else
      sort_at sorted y in
  List.rev @@ sort [] paths

let remember_order units =
  let open Unit in
  let compute (i,m) u = i+1, Name.Map.add u.Unit.name i m in
  snd @@ List.fold_left compute (0,Name.Map.empty)
  @@ List.rev @@ List.filter (fun u -> Pkg.is_known u.path) @@ units

let topos_compare order x y =
  let get x=Name.Map.find_opt x order in
  match get x, get y with
  | Some k , Some l -> compare k l
  | None, Some _ -> -1
  | Some _, None -> 1
  | None, None -> compare x y

let toposort order m =
    List.sort (fun x y -> topos_compare order (m x) (m y))
