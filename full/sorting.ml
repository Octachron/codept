(** Topological order functions *)
let full_topological_sort deps paths =
  let visited = Hashtbl.create 17 in
  let temporary = Hashtbl.create 17 in
  let guard x = Hashtbl.add temporary x true in
  let cycle x = Hashtbl.mem temporary x in
  let mark x = Hashtbl.add visited x true; Hashtbl.remove temporary x in
  let is_visited x = Hashtbl.mem visited x in
  let rec sort sorted = function
    | [] -> sorted
    | a :: q ->
      if is_visited a then
        sort sorted q
      else if cycle a then None
      else
        let sorted = sort_at sorted a in
        sort sorted q
  and sort_at sorted x =
    guard x;
    let sorted = Paths.Pkg.Set.fold sort_dep (deps x) sorted in
    let open Option in
    sorted >>| fun sorted ->
    mark x;
    x :: sorted
  and sort_dep y sorted =
    if is_visited y then
      sorted
    else if cycle y then
      None
    else
      sort_at sorted y in
  Option.fmap List.rev @@ sort (Some []) paths

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
