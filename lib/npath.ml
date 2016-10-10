module Core = struct
  type t = Name.t list
  type npath = t
  let compare (x:t) (y:t) = compare x y
  let pp = Pp.list ~sep:"." Name.pp
end
include Core
module Set = struct
  include Set.Make(Core)
  let pp ppf s = Pp.(clist Core.pp) ppf (elements s)
end
module Map = struct
  include (Map.Make(Core))
  let find_opt k m = try Some(find k m) with Not_found -> None
  let union' s = union (fun _key _m1 m2 -> Some m2) s
end
type set = Set.t
type 'a map = 'a Map.t
let prefix = List.hd
